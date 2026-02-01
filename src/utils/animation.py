#!/usr/bin/env python3
"""
CEREBRUM Animation Utilities
Provides consistent animation functions for model visualizations across the project
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, PillowWriter
import shutil
from PIL import Image

# imageio is an optional dependency (part of 'lexicon' extras)
# Functions that need it will import lazily with helpful error messages
_imageio = None
_imread = None
_imwrite = None
_mimsave = None


def _ensure_imageio():
    """Lazy-load imageio and its functions. Raises ImportError with helpful message if not installed."""
    global _imageio, _imread, _imwrite, _mimsave
    if _imageio is None:
        try:
            import imageio
            from imageio import imread, imwrite, mimsave
            _imageio = imageio
            _imread = imread
            _imwrite = imwrite
            _mimsave = mimsave
        except ImportError:
            raise ImportError(
                "imageio is required for animation utilities but is not installed. "
                "Install it with: uv pip install -e '.[lexicon]' or pip install imageio"
            )
    return _imageio, _imread, _imwrite, _mimsave

# Setup logging
logger = logging.getLogger("cerebrum-animation")

def ensure_scalar(value):
    """
    Safely convert numpy arrays to scalar values if possible
    
    Parameters:
    -----------
    value : any
        Value to convert to scalar if it's a numpy array
    
    Returns:
    --------
    any
        Scalar value if conversion is possible, otherwise the original value
    """
    try:
        # Handle None values
        if value is None:
            return 0
            
        # Handle numpy arrays
        if isinstance(value, np.ndarray):
            # Empty array check
            if value.size == 0:
                return 0
                
            # Check if it's a single-element array that can be converted to scalar
            if value.size == 1:
                return float(value.item())
                
            # For 1D arrays with a single element
            elif value.ndim == 1 and len(value) == 1:
                return float(value[0])
                
            # For 2D arrays with a single element
            elif value.ndim == 2 and value.shape[0] == 1 and value.shape[1] == 1:
                return float(value[0, 0])
                
            # For arrays with multiple elements that are all the same value
            elif np.all(value == value.flat[0]):
                return float(value.flat[0])
                
            # For 1D arrays, return first element as a last resort
            elif value.ndim == 1 and len(value) > 0:
                logger.warning(f"Converting non-scalar array of shape {value.shape} to scalar by using first element")
                return float(value[0])
                
        # Handle lists
        elif isinstance(value, list):
            if len(value) == 0:
                return 0
            elif len(value) == 1:
                return float(value[0])
                
        # Try to convert to float directly
        try:
            return float(value)
        except (TypeError, ValueError):
            pass
            
        return value
        
    except Exception as e:
        logger.warning(f"Error in ensure_scalar: {str(e)}, returning original value")
        return value

def save_animation(animation, output_path, fps=5, dpi=100, writer='pillow', **kwargs):
    """
    Save an animation with improved error handling and logging
    
    Parameters:
    -----------
    animation : matplotlib.animation.FuncAnimation
        The animation object to save
    output_path : str
        Path where the animation should be saved
    fps : int, default=5
        Frames per second
    dpi : int, default=100
        Dots per inch (resolution)
    writer : str, default='pillow'
        The writer to use for saving the animation
    **kwargs : dict
        Additional keyword arguments to pass to the writer
    
    Returns:
    --------
    bool
        True if the animation was saved successfully, False otherwise
    """
    try:
        # Ensure imageio is available (lazy load)
        _, imread, imwrite, mimsave = _ensure_imageio()
        
        # Create directory if it doesn't exist
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        
        # Set up keyword arguments with defaults
        save_kwargs = {'dpi': dpi}
        save_kwargs.update(kwargs)
        
        # Save the animation with explicit logging
        logger.info(f"Saving animation to {output_path} with {fps} fps and dpi={dpi}")
        
        # Force a draw to ensure all frames are properly rendered
        plt.draw()
        plt.pause(0.01)  # Slightly longer pause to ensure rendering completes
        
        # Get the figure from the animation correctly
        if hasattr(animation, '_fig'):
            fig = animation._fig
        elif hasattr(animation, 'fig'):
            fig = animation.fig
        else:
            fig = plt.gcf()
            
        # Ensure the animation has fully initialized
        if hasattr(animation, '_init_func') and animation._init_func is not None:
            animation._init_func()
            
        # Save frames directly as a fallback option
        frame_dir = os.path.join(os.path.dirname(output_path), "frames_" + os.path.basename(output_path).replace('.gif', ''))
        os.makedirs(frame_dir, exist_ok=True)
        
        # Capture all frames first to ensure they're properly rendered
        logger.info(f"Pre-rendering all animation frames to {frame_dir}")
        frames = []
        
        # Get total number of frames from the animation
        if hasattr(animation, '_frames'):
            total_frames = animation._frames
        elif hasattr(animation, 'frames'):
            total_frames = animation.frames
        elif hasattr(animation, 'save_count'):
            total_frames = animation.save_count
        else:
            total_frames = 30  # Default to 30 frames if we can't determine count
            logger.warning(f"Could not determine frame count, using default of {total_frames}")
        
        # Force the animation to render each frame
        for i in range(total_frames):
            try:
                # Clear the figure and render the frame - use different methods depending on animation type
                if hasattr(animation, '_func'):
                    # FuncAnimation
                    animation._func(i)
                elif hasattr(animation, 'func'):
                    # Some animation classes may use 'func' instead
                    animation.func(i)
                else:
                    logger.warning(f"Could not find animation function to render frame {i}, skipping")
                    continue
                    
                fig.canvas.draw()
                plt.pause(0.01)  # Ensure the frame is fully rendered
                
                # Save individual frame
                frame_path = os.path.join(frame_dir, f"frame_{i:04d}.png")
                fig.savefig(frame_path, dpi=dpi, bbox_inches='tight')
                
                # Read the saved frame
                if os.path.exists(frame_path) and os.path.getsize(frame_path) > 0:
                    try:
                        img = imread(frame_path)
                        if img is not None and img.size > 0:
                            frames.append(img)
                        else:
                            logger.warning(f"Frame {i} appears to be empty, skipping")
                    except Exception as e:
                        logger.warning(f"Could not read frame {i}: {str(e)}")
            except Exception as e:
                logger.warning(f"Error rendering frame {i}: {str(e)}")
                
        # If we captured frames directly, save them as GIF
        if frames:
            try:
                logger.info(f"Saving {len(frames)} pre-rendered frames as GIF")
                
                # Check for inconsistent frame shapes
                sizes = set((frame.shape[0], frame.shape[1]) for frame in frames)
                if len(sizes) > 1:
                    logger.warning(f"Found inconsistent frame sizes: {sizes}. Resizing all frames to match the first frame.")
                    
                    # Get the shape of the first frame
                    target_shape = frames[0].shape[:2]
                    
                    # Resize all frames to match the first one
                    processed_frames = []
                    
                    for i, frame in enumerate(frames):
                        if frame.shape[:2] != target_shape:
                            # Convert numpy array to PIL Image
                            img = Image.fromarray(frame)
                            # Resize to match target shape (width, height)
                            img = img.resize((target_shape[1], target_shape[0]), Image.LANCZOS)
                            # Convert back to numpy array
                            processed_frames.append(np.array(img))
                        else:
                            processed_frames.append(frame)
                            
                    frames = processed_frames
                
                mimsave(output_path, frames, fps=fps, loop=0)
                
                # Verify the GIF was created with content
                if os.path.exists(output_path) and os.path.getsize(output_path) > 0:
                    logger.info(f"Successfully created GIF from pre-rendered frames: {output_path} ({os.path.getsize(output_path)/1024:.1f} KB)")
                    return True
            except Exception as e:
                logger.warning(f"Failed to save frames directly: {str(e)}")
        else:
            logger.warning("No valid frames were captured in pre-rendering phase")
                
        # If direct frame capture failed or resulted in no frames, try using ffmpeg writer if available
        if shutil.which('ffmpeg'):
            try:
                logger.info(f"Attempting to save with ffmpeg")
                animation.save(output_path, writer='ffmpeg', fps=fps, **save_kwargs)
                
                if os.path.exists(output_path) and os.path.getsize(output_path) > 0:
                    logger.info(f"Animation saved successfully with ffmpeg to {output_path} ({os.path.getsize(output_path)/1024:.1f} KB)")
                    return True
            except Exception as e:
                logger.warning(f"Failed to save with ffmpeg: {str(e)}, trying PillowWriter")
        
        # Try using PillowWriter as fallback
        try:
            logger.info(f"Attempting to save with PillowWriter")
            writer_obj = PillowWriter(fps=fps)
            animation.save(output_path, writer=writer_obj, **save_kwargs)
            
            if os.path.exists(output_path) and os.path.getsize(output_path) > 0:
                logger.info(f"Animation saved successfully with PillowWriter to {output_path} ({os.path.getsize(output_path)/1024:.1f} KB)")
                return True
            else:
                logger.warning(f"Animation file appears to be empty: {output_path}")
        except Exception as e:
            logger.warning(f"Failed to save with PillowWriter: {str(e)}, trying imageio")

        # Try with imageio using the frames we already captured
        if frames:
            try:
                logger.info(f"Saving frames with imageio using different parameters")
                # Try with different optimization parameters
                mimsave(output_path, frames, fps=fps, loop=0, optimize=False)
                
                if os.path.exists(output_path) and os.path.getsize(output_path) > 0:
                    logger.info(f"Animation saved with imageio optimization off: {output_path} ({os.path.getsize(output_path)/1024:.1f} KB)")
                    return True
            except Exception as e:
                logger.warning(f"Failed to save with imageio optimization off: {str(e)}")
                
        # Last resort: Try with MP4 format
        if output_path.lower().endswith('.gif'):
            mp4_path = output_path.replace('.gif', '.mp4')
            try:
                logger.info(f"Attempting fallback to MP4 format: {mp4_path}")
                
                if frames:
                    # Save directly from frames if we have them
                    import imageio.plugins.ffmpeg
                    mimsave(mp4_path, frames, fps=fps, codec='libx264', quality=7)
                else:
                    # Use animation.save with ffmpeg
                    animation.save(mp4_path, writer='ffmpeg', fps=fps, dpi=dpi)
                
                if os.path.exists(mp4_path) and os.path.getsize(mp4_path) > 0:
                    logger.info(f"Animation saved as MP4 instead of GIF: {mp4_path} ({os.path.getsize(mp4_path)/1024:.1f} KB)")
                    
                    # Try to convert MP4 to GIF using ffmpeg directly
                    if shutil.which('ffmpeg'):
                        try:
                            logger.info(f"Converting MP4 to GIF using ffmpeg command")
                            cmd = f"ffmpeg -i {mp4_path} -vf 'fps={fps},scale=320:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse' -loop 0 {output_path}"
                            os.system(cmd)
                            
                            if os.path.exists(output_path) and os.path.getsize(output_path) > 0:
                                logger.info(f"Successfully converted MP4 to GIF: {output_path} ({os.path.getsize(output_path)/1024:.1f} KB)")
                                return True
                        except Exception as e:
                            logger.warning(f"Failed to convert MP4 to GIF: {str(e)}")
                    
                    return True
            except Exception as e:
                logger.warning(f"MP4 fallback failed: {str(e)}")
        
        # If all attempts failed, use the pre-rendered frames as a last resort
        if frames:
            try:
                # Save individual frames in the main output directory
                frames_subdir = os.path.join(os.path.dirname(output_path), "frames_" + os.path.basename(output_path).replace('.gif', ''))
                os.makedirs(frames_subdir, exist_ok=True)
                
                logger.info(f"Saving individual frames to {frames_subdir} as final fallback")
                for i, frame in enumerate(frames):
                    frame_path = os.path.join(frames_subdir, f"frame_{i:04d}.png")
                    imwrite(frame_path, frame)
                    
                logger.info(f"Saved {len(frames)} individual frames that can be used to create GIF manually")
                return True
            except Exception as e:
                logger.error(f"Failed to save individual frames: {str(e)}")
        
        # If we got here, all attempts failed
        logger.error(f"All animation saving methods failed for {output_path}")
        return False
            
    except Exception as e:
        logger.error(f"Error in save_animation: {str(e)}")
        return False

def save_frames_as_gif(frames, output_path, fps=5, loop=0):
    """
    Save a list of image frames as a GIF using imageio directly
    
    Parameters:
    -----------
    frames : list of numpy arrays
        List of image frames to save
    output_path : str
        Path where the GIF should be saved
    fps : int, default=5
        Frames per second
    loop : int, default=0
        Number of times to loop the GIF (0 = infinite)
    
    Returns:
    --------
    bool
        True if the GIF was saved successfully, False otherwise
    """
    try:
        # Ensure imageio is available (lazy load)
        _, imread, imwrite, mimsave = _ensure_imageio()
        
        logger.info(f"Saving {len(frames)} frames as GIF to {output_path}")
        
        # Create directory if it doesn't exist
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        
        # Check for empty frames
        if not frames:
            logger.error("No frames to save")
            return False
            
        # Check for inconsistent frame shapes
        sizes = set((frame.shape[0], frame.shape[1]) for frame in frames)
        if len(sizes) > 1:
            logger.warning(f"Found inconsistent frame sizes: {sizes}. Resizing all frames to match the first frame.")
            
            # Get the shape of the first frame
            target_shape = frames[0].shape[:2]
            
            # Resize all frames to match the first one
            processed_frames = []
            
            for i, frame in enumerate(frames):
                if frame.shape[:2] != target_shape:
                    # Convert numpy array to PIL Image
                    img = Image.fromarray(frame)
                    # Resize to match target shape (width, height)
                    img = img.resize((target_shape[1], target_shape[0]), Image.LANCZOS)
                    # Convert back to numpy array
                    processed_frames.append(np.array(img))
                else:
                    processed_frames.append(frame)
                    
            frames = processed_frames
        
        # Save frames as GIF with optimization off for more reliable results
        mimsave(output_path, frames, fps=fps, loop=loop, optimize=False)
        
        # Verify the file exists and has content
        if os.path.exists(output_path) and os.path.getsize(output_path) > 0:
            logger.info(f"GIF saved successfully to {output_path} ({os.path.getsize(output_path)/1024:.1f} KB)")
            
            # As a backup, also save the individual frames
            frames_dir = os.path.join(os.path.dirname(output_path), "frames_" + os.path.basename(output_path).replace('.gif', ''))
            os.makedirs(frames_dir, exist_ok=True)
            
            for i, frame in enumerate(frames):
                frame_path = os.path.join(frames_dir, f"frame_{i:04d}.png")
                imwrite(frame_path, frame)
                
            return True
        else:
            logger.error(f"GIF file created but appears to be empty: {output_path}")
            
            # Save individual frames as fallback
            frames_dir = os.path.join(os.path.dirname(output_path), "frames_" + os.path.basename(output_path).replace('.gif', ''))
            os.makedirs(frames_dir, exist_ok=True)
            
            for i, frame in enumerate(frames):
                frame_path = os.path.join(frames_dir, f"frame_{i:04d}.png")
                imwrite(frame_path, frame)
                
            logger.info(f"Saved {len(frames)} individual frames to {frames_dir}")
            return False
            
    except Exception as e:
        logger.error(f"Error saving GIF to {output_path}: {str(e)}")
        
        # Try to save individual frames as fallback
        try:
            frames_dir = os.path.join(os.path.dirname(output_path), "frames_" + os.path.basename(output_path).replace('.gif', ''))
            os.makedirs(frames_dir, exist_ok=True)
            
            for i, frame in enumerate(frames):
                frame_path = os.path.join(frames_dir, f"frame_{i:04d}.png")
                imwrite(frame_path, frame)
                
            logger.info(f"Saved {len(frames)} individual frames to {frames_dir} after GIF save failure")
        except Exception as inner_e:
            logger.error(f"Failed to save individual frames: {str(inner_e)}")
            
        return False

def create_linear_regression_animation(X, y, model, output_path, 
                                       title="Linear Regression Animation",
                                       speech_bubbles=True,
                                       fps=5,
                                       dpi=100):
    """
    Create and save an animation showing a linear regression model's predictions
    
    Parameters:
    -----------
    X : numpy.ndarray
        Input features
    y : numpy.ndarray
        Target values
    model : object
        A model object with predict() method
    output_path : str
        Path where the animation should be saved
    title : str, default="Linear Regression Animation"
        Title for the animation
    speech_bubbles : bool, default=True
        Whether to include speech bubbles in the animation
    fps : int, default=5
        Frames per second
    dpi : int, default=100
        Dots per inch (resolution)
    
    Returns:
    --------
    bool
        True if the animation was saved successfully, False otherwise
    """
    try:
        # Ensure X and y are properly formatted as arrays
        X_anim = np.array(X)
        y_anim = np.array(y) if not np.isscalar(y) else np.array([y])
        
        # Handle shape for comparison
        X_shape = X_anim.shape[0] if hasattr(X_anim, 'shape') else len(X_anim)
        y_shape = y_anim.shape[0] if hasattr(y_anim, 'shape') else len(y_anim)
        
        # Ensure arrays are the same size for animation
        if X_shape != y_shape:
            # Find minimum size
            min_size = min(X_shape, y_shape)
            X_anim = X_anim[:min_size]
            y_anim = y_anim[:min_size]
        
        # Extract model parameters if available
        if hasattr(model, '_params'):
            intercept = ensure_scalar(model._params.get('intercept', 0))
            # Handle potential 2D array for coefficients
            coef = model._params.get('coefficients', [0])
            slope = ensure_scalar(coef[0] if hasattr(coef, '__getitem__') else coef)
        else:
            # Try to get parameters from a scikit-learn style model
            try:
                intercept = ensure_scalar(model.intercept_ if hasattr(model, 'intercept_') else 0)
                slope = ensure_scalar(model.coef_[0] if hasattr(model, 'coef_') else 0)
            except:
                # Fallback to estimating from data
                predictions = model.predict(X_anim)
                slope = np.polyfit(X_anim.flatten(), predictions.flatten(), 1)[0]
                intercept = np.mean(predictions) - slope * np.mean(X_anim)
        
        # Create the figure and axes
        fig, ax = plt.subplots(figsize=(10, 8))
        
        # Plot data points
        scatter = ax.scatter(X_anim, y_anim, color='blue', alpha=0.7, label='Data')
        
        # Initialize the line
        line, = ax.plot([], [], 'r-', linewidth=2, label='Model')
        
        # Initialize residual lines
        residual_lines = [ax.plot([], [], 'k-', alpha=0.3)[0] for _ in range(len(X_anim))]
        
        # Initialize prediction scatter
        pred_scatter = ax.scatter([], [], color='red', alpha=0.7, label='Predictions')
        
        # Create text elements for speech bubbles if requested
        speech_bubbles_list = []
        if speech_bubbles:
            for i in range(len(X_anim)):
                # Create a text object for each data point
                bubble = ax.text(0, 0, '', bbox=dict(boxstyle="round,pad=0.3", fc="white", 
                                                 ec="gray", alpha=0.0), visible=False)
                speech_bubbles_list.append(bubble)
            
            # Model response bubble
            model_bubble = ax.text(0.5, 0.9, '', transform=ax.transAxes,
                                  bbox=dict(boxstyle="round,pad=0.5", fc="#E3F4F4", ec="blue", alpha=0.0),
                                  ha='center', va='center', fontsize=12, visible=False)
        else:
            model_bubble = None
        
        # Calculate margins for plot limits with safety checks
        if X_anim.size > 0:
            x_min, x_max = X_anim.min(), X_anim.max()
            x_margin = (x_max - x_min) * 0.2 if x_max > x_min else 1.0
        else:
            x_min, x_max, x_margin = -1, 1, 0.4

        if y_anim.size > 0:
            y_min, y_max = y_anim.min(), y_anim.max()
            y_margin = (y_max - y_min) * 0.2 if y_max > y_min else 1.0
        else:
            y_min, y_max, y_margin = -1, 1, 0.4
        
        # Set axis limits with safe values
        ax.set_xlim(x_min - x_margin, x_max + x_margin)
        ax.set_ylim(y_min - y_margin, y_max + y_margin)
        
        # Add labels and grid
        ax.set_title(title, fontsize=14)
        ax.set_xlabel("X", fontsize=12)
        ax.set_ylabel("Y", fontsize=12)
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        def init():
            """Initialize the animation"""
            line.set_data([], [])
            for residual_line in residual_lines:
                residual_line.set_data([], [])
            
            if speech_bubbles:
                model_bubble.set_visible(False)
                for bubble in speech_bubbles_list:
                    bubble.set_visible(False)
                    
            pred_scatter.set_offsets(np.empty((0, 2)))
            
            artists = [line, *residual_lines, pred_scatter]
            if speech_bubbles:
                artists.extend([model_bubble, *speech_bubbles_list])
                
            return artists
        
        def animate(i):
            """Animation function for each frame"""
            total_frames = 25 + len(X_anim) + 15
            
            if i < 20:
                # Phase 1: Draw the regression line gradually
                progress = i / 19
                x_line = np.linspace(x_min - x_margin, x_min + (x_max - x_min + 2*x_margin) * progress, 100)
                y_line = intercept + slope * x_line
                line.set_data(x_line, y_line)
                
                # Hide other elements
                for residual_line in residual_lines:
                    residual_line.set_data([], [])
                    
                if speech_bubbles:
                    model_bubble.set_visible(False)
                    for bubble in speech_bubbles_list:
                        bubble.set_visible(False)
                        
                pred_scatter.set_offsets(np.empty((0, 2)))
                
                if i == 19 and speech_bubbles:
                    # At the end of phase 1, show model introduction
                    model_bubble.set_text("Hello data points! I'm your linear model.")
                    model_bubble.set_bbox(dict(boxstyle="round,pad=0.5", fc="#E3F4F4", ec="blue", alpha=0.9))
                    model_bubble.set_visible(True)
                    
            elif i < 25:
                # Phase 2: Model introduction
                # Keep the regression line complete
                x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
                y_line = intercept + slope * x_line
                line.set_data(x_line, y_line)
                
                # Keep model bubble visible if using speech bubbles
                if speech_bubbles:
                    model_bubble.set_visible(True)
                
            elif i < 25 + len(X_anim):
                # Phase 3: Add predictions one by one
                point_idx = i - 25
                
                # Keep the regression line complete
                x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
                y_line = intercept + slope * x_line
                line.set_data(x_line, y_line)
                
                # Update the model speech bubble if using speech bubbles
                if speech_bubbles:
                    if len(X_anim.shape) > 1:
                        x_val = X_anim[point_idx, 0]
                    else:
                        x_val = X_anim[point_idx]
                        
                    if len(y_anim.shape) > 1:
                        y_true = y_anim[point_idx, 0]
                    else:
                        y_true = y_anim[point_idx]
                        
                    y_pred_val = intercept + slope * x_val
                    model_bubble.set_text(f"Point at x={x_val:.2f}, I predict y={y_pred_val:.2f}")
                    model_bubble.set_visible(True)
                
                # Get x and y values for current point
                if len(X_anim.shape) > 1:
                    x_val = X_anim[point_idx, 0]
                else:
                    x_val = X_anim[point_idx]
                    
                if len(y_anim.shape) > 1:
                    y_true = y_anim[point_idx, 0]
                else:
                    y_true = y_anim[point_idx]
                
                # Get prediction
                y_pred_val = intercept + slope * x_val
                
                # Show predictions up to current point
                if point_idx == 0:
                    pred_data = np.array([[x_val, y_pred_val]])
                else:
                    # Build array of previous points plus current point
                    pred_data = []
                    for j in range(point_idx + 1):
                        if len(X_anim.shape) > 1:
                            x_j = X_anim[j, 0]
                        else:
                            x_j = X_anim[j]
                        y_j_pred = intercept + slope * x_j
                        pred_data.append([x_j, y_j_pred])
                    pred_data = np.array(pred_data)
                
                pred_scatter.set_offsets(pred_data)
                
                # Draw residual line for current point
                residual_lines[point_idx].set_data([x_val, x_val], [y_true, y_pred_val])
                
                # Show speech bubble for current data point if using speech bubbles
                if speech_bubbles:
                    residual = y_true - y_pred_val
                    bubble_text = f"I'm actually {y_true:.2f}, off by {residual:.2f}"
                    speech_bubbles_list[point_idx].set_position((x_val + 0.15, y_true))
                    speech_bubbles_list[point_idx].set_text(bubble_text)
                    speech_bubbles_list[point_idx].set_bbox(dict(boxstyle="round,pad=0.3", fc="#F8E8EE", ec="pink", alpha=0.9))
                    speech_bubbles_list[point_idx].set_visible(True)
                
            elif i < 25 + len(X_anim) + 10:
                # Phase 4: All predictions shown, model summarizes
                # Keep the regression line complete
                x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
                y_line = intercept + slope * x_line
                line.set_data(x_line, y_line)
                
                # Show all predictions
                pred_data = []
                for j in range(len(X_anim)):
                    if len(X_anim.shape) > 1:
                        x_j = X_anim[j, 0]
                    else:
                        x_j = X_anim[j]
                    y_j_pred = intercept + slope * x_j
                    pred_data.append([x_j, y_j_pred])
                pred_data = np.array(pred_data)
                
                pred_scatter.set_offsets(pred_data)
                
                # Draw all residual lines
                for j, residual_line in enumerate(residual_lines):
                    if len(X_anim.shape) > 1:
                        x_val = X_anim[j, 0]
                    else:
                        x_val = X_anim[j]
                        
                    if len(y_anim.shape) > 1:
                        y_true = y_anim[j, 0]
                    else:
                        y_true = y_anim[j]
                        
                    y_pred_val = intercept + slope * x_val
                    residual_line.set_data([x_val, x_val], [y_true, y_pred_val])
                
                # Update model speech bubble with summary if using speech bubbles
                if speech_bubbles:
                    # Calculate residuals manually
                    residuals = []
                    for j in range(len(X_anim)):
                        if len(X_anim.shape) > 1:
                            x_j = X_anim[j, 0]
                        else:
                            x_j = X_anim[j]
                            
                        if len(y_anim.shape) > 1:
                            y_true_j = y_anim[j, 0]
                        else:
                            y_true_j = y_anim[j]
                            
                        y_pred_j = intercept + slope * x_j
                        residuals.append(y_true_j - y_pred_j)
                    
                    mse = np.mean(np.array(residuals)**2)
                    model_bubble.set_text(f"Thanks for the feedback! My MSE is {mse:.4f}")
                    model_bubble.set_visible(True)
                    
                    # Hide individual speech bubbles
                    for bubble in speech_bubbles_list:
                        bubble.set_visible(False)
                    
            else:
                # Phase 5: Final frame - keep everything visible but change model message
                # Keep the regression line complete
                x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
                y_line = intercept + slope * x_line
                line.set_data(x_line, y_line)
                
                # Show all predictions
                pred_data = []
                for j in range(len(X_anim)):
                    if len(X_anim.shape) > 1:
                        x_j = X_anim[j, 0]
                    else:
                        x_j = X_anim[j]
                    y_j_pred = intercept + slope * x_j
                    pred_data.append([x_j, y_j_pred])
                pred_data = np.array(pred_data)
                
                pred_scatter.set_offsets(pred_data)
                
                # Draw all residual lines
                for j, residual_line in enumerate(residual_lines):
                    if len(X_anim.shape) > 1:
                        x_val = X_anim[j, 0]
                    else:
                        x_val = X_anim[j]
                        
                    if len(y_anim.shape) > 1:
                        y_true = y_anim[j, 0]
                    else:
                        y_true = y_anim[j]
                        
                    y_pred_val = intercept + slope * x_val
                    residual_line.set_data([x_val, x_val], [y_true, y_pred_val])
                
                # Final model message if using speech bubbles
                if speech_bubbles:
                    model_bubble.set_text(f"My equation is y = {intercept:.2f} + {slope:.2f}x")
                    model_bubble.set_visible(True)
                    
                    # Add a few representative speech bubbles back
                    for j in range(min(5, len(speech_bubbles_list))):
                        idx = j * len(speech_bubbles_list) // 5
                        if idx < len(speech_bubbles_list):
                            if len(X_anim.shape) > 1:
                                x_val = X_anim[idx, 0]
                            else:
                                x_val = X_anim[idx]
                                
                            if len(y_anim.shape) > 1:
                                y_true = y_anim[idx, 0]
                            else:
                                y_true = y_anim[idx]
                                
                            y_pred_val = intercept + slope * x_val
                            residual = y_true - y_pred_val
                            bubble_text = f"Residual: {residual:.2f}"
                            speech_bubbles_list[idx].set_position((x_val + 0.15, y_true))
                            speech_bubbles_list[idx].set_text(bubble_text)
                            speech_bubbles_list[idx].set_bbox(dict(boxstyle="round,pad=0.3", fc="#F8E8EE", ec="pink", alpha=0.9))
                            speech_bubbles_list[idx].set_visible(True)
            
            artists = [line, *residual_lines, pred_scatter]
            if speech_bubbles:
                artists.extend([model_bubble, *speech_bubbles_list])
                
            return artists
        
        # Create the animation
        frames = 25 + len(X_anim) + 15  # Total number of frames
        ani = FuncAnimation(fig, animate, frames=frames, init_func=init, blit=True)
        
        # Save the animation using our improved save_animation function
        success = save_animation(ani, output_path, fps=fps, dpi=dpi)
        
        # Close the figure to free memory
        plt.close(fig)
        
        return success
    
    except Exception as e:
        logger.error(f"Error creating linear regression animation: {str(e)}")
        return False 