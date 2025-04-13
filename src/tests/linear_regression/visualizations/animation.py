"""
Animation utilities for linear regression tests.
"""
import os
import sys
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import matplotlib.animation as animation
from typing import Dict, List, Tuple, Any, Optional, Callable

# Add the src directory to the path for imports if needed
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..')))


class Animator:
    """Animation utilities for linear regression tests."""
    
    @staticmethod
    def create_regression_animation(
        X: np.ndarray,
        y: np.ndarray,
        model: Any,  # LinearRegressionModel
        title: str = "Linear Regression Animation",
        xlabel: str = "X",
        ylabel: str = "y",
        figsize: Tuple[int, int] = (10, 6),
        save_path: Optional[str] = None,
        frames: int = 60,
        interval: int = 100,
        fps: int = 15
    ) -> Optional[animation.Animation]:
        """
        Create an animation showing the regression line fitting process.
        
        Args:
            X: Feature matrix
            y: Target vector
            model: LinearRegressionModel instance (must be already fitted)
            title: Plot title
            xlabel: Label for x-axis
            ylabel: Label for y-axis
            figsize: Figure size (width, height) in inches
            save_path: Path to save the animation (if None, animation is not saved)
            frames: Number of animation frames
            interval: Delay between frames in milliseconds
            fps: Frames per second when saving
            
        Returns:
            Animation object (None if save_path is provided)
        """
        # Flatten input arrays for plotting (if they're not already 1D)
        X_flat = X.flatten() if X.ndim > 1 else X
        y_flat = y.flatten() if y.ndim > 1 else y
        
        # Get predictions from the model
        y_pred = model.predict(X)
        y_pred_flat = y_pred.flatten() if y_pred.ndim > 1 else y_pred
        
        # Create figure and axis
        fig, ax = plt.subplots(figsize=figsize)
        
        # Initial plot: just the data
        scatter = ax.scatter(X_flat, y_flat, alpha=0.7, s=50, edgecolor='k', label='Data')
        
        # Line plot that will be updated
        line, = ax.plot([], [], 'r-', linewidth=2, label='Regression Line')
        
        # Set labels and title
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        
        # Set limits with some padding
        x_min, x_max = X_flat.min(), X_flat.max()
        y_min, y_max = min(y_flat.min(), y_pred_flat.min()), max(y_flat.max(), y_pred_flat.max())
        
        x_padding = (x_max - x_min) * 0.1
        y_padding = (y_max - y_min) * 0.1
        
        ax.set_xlim(x_min - x_padding, x_max + x_padding)
        ax.set_ylim(y_min - y_padding, y_max + y_padding)
        
        # Add grid and legend
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        # Equation text
        if hasattr(model, 'intercept_') and hasattr(model, 'coefficients_'):
            intercept = model.intercept_
            coefficient = model.coefficients_[0] if len(model.coefficients_) > 0 else 0
            equation_text = ax.text(0.05, 0.95, '', transform=ax.transAxes, fontsize=10,
                                   verticalalignment='top', 
                                   bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        else:
            equation_text = None
        
        # Calculate the sorted indices for smooth line
        sort_idx = np.argsort(X_flat)
        X_sorted = X_flat[sort_idx]
        y_pred_sorted = y_pred_flat[sort_idx]
        
        # Animation update function
        def update(frame):
            # Determine how much of the line to show (from 0% to 100%)
            progress = frame / (frames - 1)
            
            # For the line, progressively show more points
            n_points = max(2, int(progress * len(X_sorted)))
            line.set_data(X_sorted[:n_points], y_pred_sorted[:n_points])
            
            # For the equation, gradually reveal it
            if equation_text is not None:
                # Start with just the intercept, then add the slope progressively
                current_coef = progress * coefficient
                eq_str = f"y = {intercept:.4f} + {current_coef:.4f} · x"
                equation_text.set_text(eq_str)
            
            return line, equation_text
        
        # Create animation
        anim = FuncAnimation(fig, update, frames=frames, interval=interval, blit=True)
        
        # Save if a path is provided
        if save_path is not None:
            writer = animation.FFMpegWriter(fps=fps)
            anim.save(save_path, writer=writer)
            plt.close(fig)
            return None
        
        return anim
    
    @staticmethod
    def create_error_animation(
        X: np.ndarray,
        y: np.ndarray,
        model: Any,  # LinearRegressionModel
        title: str = "Regression Error Animation",
        xlabel: str = "X",
        ylabel: str = "y",
        figsize: Tuple[int, int] = (10, 6),
        save_path: Optional[str] = None,
        frames: int = 60,
        interval: int = 100,
        fps: int = 15
    ) -> Optional[animation.Animation]:
        """
        Create an animation showing the errors in regression predictions.
        
        Args:
            X: Feature matrix
            y: Target vector
            model: LinearRegressionModel instance (must be already fitted)
            title: Plot title
            xlabel: Label for x-axis
            ylabel: Label for y-axis
            figsize: Figure size (width, height) in inches
            save_path: Path to save the animation (if None, animation is not saved)
            frames: Number of animation frames
            interval: Delay between frames in milliseconds
            fps: Frames per second when saving
            
        Returns:
            Animation object (None if save_path is provided)
        """
        # Flatten input arrays for plotting (if they're not already 1D)
        X_flat = X.flatten() if X.ndim > 1 else X
        y_flat = y.flatten() if y.ndim > 1 else y
        
        # Get predictions from the model
        y_pred = model.predict(X)
        y_pred_flat = y_pred.flatten() if y_pred.ndim > 1 else y_pred
        
        # Calculate errors
        errors = y_flat - y_pred_flat
        
        # Create figure and axis
        fig, ax = plt.subplots(figsize=figsize)
        
        # Initial plot: just the data
        scatter_data = ax.scatter(X_flat, y_flat, alpha=0.7, s=50, edgecolor='k', label='Actual Data')
        
        # Predictions that will be revealed
        scatter_pred = ax.scatter([], [], alpha=0.7, s=50, color='red', edgecolor='k', label='Predictions')
        
        # Line plot that will be updated
        line, = ax.plot([], [], 'r-', linewidth=2, label='Regression Line')
        
        # Container for error lines
        error_lines = []
        
        # Set labels and title
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        
        # Set limits with some padding
        x_min, x_max = X_flat.min(), X_flat.max()
        y_min, y_max = min(y_flat.min(), y_pred_flat.min()), max(y_flat.max(), y_pred_flat.max())
        
        x_padding = (x_max - x_min) * 0.1
        y_padding = (y_max - y_min) * 0.1
        
        ax.set_xlim(x_min - x_padding, x_max + x_padding)
        ax.set_ylim(y_min - y_padding, y_max + y_padding)
        
        # Add grid and legend
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        # Sort data for line plotting
        sort_idx = np.argsort(X_flat)
        X_sorted = X_flat[sort_idx]
        y_pred_sorted = y_pred_flat[sort_idx]
        
        # Animation update function
        def update(frame):
            objects_to_return = []
            
            # Determine how much of the data to show (from 0% to 100%)
            progress = frame / (frames - 1)
            
            # Phase 1 (0-30%): Show regression line
            if progress <= 0.3:
                phase_progress = progress / 0.3
                n_points = max(2, int(phase_progress * len(X_sorted)))
                line.set_data(X_sorted[:n_points], y_pred_sorted[:n_points])
                scatter_pred.set_offsets(np.c_[[], []])
                objects_to_return.append(line)
                objects_to_return.append(scatter_pred)
                
            # Phase 2 (30-60%): Show predictions
            elif progress <= 0.6:
                phase_progress = (progress - 0.3) / 0.3
                n_points = max(2, int(phase_progress * len(X_flat)))
                
                # Show full regression line
                line.set_data(X_sorted, y_pred_sorted)
                
                # Gradually reveal predictions
                pred_points = np.c_[X_flat[:n_points], y_pred_flat[:n_points]]
                scatter_pred.set_offsets(pred_points)
                
                objects_to_return.append(line)
                objects_to_return.append(scatter_pred)
                
            # Phase 3 (60-100%): Show error lines
            else:
                # Show full regression line and all predictions
                line.set_data(X_sorted, y_pred_sorted)
                scatter_pred.set_offsets(np.c_[X_flat, y_pred_flat])
                
                # Calculate how many error lines to show
                phase_progress = (progress - 0.6) / 0.4
                n_errors = max(1, int(phase_progress * len(X_flat)))
                
                # Create or update error lines
                while len(error_lines) < n_errors:
                    idx = len(error_lines)
                    error_line, = ax.plot([X_flat[idx], X_flat[idx]], 
                                        [y_flat[idx], y_pred_flat[idx]],
                                        'g-', alpha=0.5, linewidth=1)
                    error_lines.append(error_line)
                
                # Update visibility of error lines
                for i, error_line in enumerate(error_lines):
                    if i < n_errors:
                        error_line.set_visible(True)
                    else:
                        error_line.set_visible(False)
                
                objects_to_return.append(line)
                objects_to_return.append(scatter_pred)
                objects_to_return.extend(error_lines)
            
            return objects_to_return
        
        # Create animation
        anim = FuncAnimation(fig, update, frames=frames, interval=interval, blit=True)
        
        # Save if a path is provided
        if save_path is not None:
            writer = animation.FFMpegWriter(fps=fps)
            anim.save(save_path, writer=writer)
            plt.close(fig)
            return None
        
        return anim
    
    @staticmethod
    def create_ablative_error_animation(
        X: np.ndarray, 
        y: np.ndarray,
        model: Any,  # LinearRegressionModel
        error_sources: Dict[str, float],
        save_path: str,
        frames: int = 150,
        interval: int = 50,
        fps: int = 20
    ) -> None:
        """
        Create an animation showing the errors originating from the model (ablative case).
        
        Args:
            X: Feature matrix
            y: Target vector
            model: LinearRegressionModel instance (must be already fitted)
            error_sources: Dictionary of error sources and their percentages
            save_path: Path to save the animation
            frames: Number of animation frames
            interval: Delay between frames in milliseconds
            fps: Frames per second when saving
        """
        # Ensure X and y are flattened for plotting
        X_flat = X.flatten() if X.ndim > 1 else X
        y_flat = y.flatten() if y.ndim > 1 else y
        
        # Get predictions and calculate errors
        y_pred = model.predict(X)
        y_pred_flat = y_pred.flatten() if y_pred.ndim > 1 else y_pred
        errors = y_flat - y_pred_flat
        
        # Sort data for smooth line plotting
        sort_idx = np.argsort(X_flat)
        X_sorted = X_flat[sort_idx]
        y_pred_sorted = y_pred_flat[sort_idx]
        
        # Create figure and axes setup
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
        fig.suptitle("Ablative Case: Errors FROM the Model", fontsize=16)
        
        # Left plot for regression visualization
        scatter_data = ax1.scatter([], [], alpha=0.7, s=50, edgecolor='k', label='Actual Data')
        scatter_pred = ax1.scatter([], [], alpha=0.7, s=50, color='red', edgecolor='k', label='Predictions')
        reg_line, = ax1.plot([], [], 'r-', linewidth=2, label='Regression Line')
        error_lines = []
        
        # Set labels and limits for regression plot
        ax1.set_xlabel("X")
        ax1.set_ylabel("y")
        ax1.set_title("Model Predictions and Errors")
        
        x_min, x_max = X_flat.min(), X_flat.max()
        y_min, y_max = min(y_flat.min(), y_pred_flat.min()), max(y_flat.max(), y_pred_flat.max())
        x_padding = (x_max - x_min) * 0.1
        y_padding = (y_max - y_min) * 0.1
        
        ax1.set_xlim(x_min - x_padding, x_max + x_padding)
        ax1.set_ylim(y_min - y_padding, y_max + y_padding)
        ax1.grid(True, alpha=0.3)
        ax1.legend()
        
        # Right plot for error attribution
        ax2.axis('equal')  # For pie chart
        ax2.set_title("Error Source Attribution")
        
        # MSE text display
        mse_text = ax1.text(0.05, 0.95, "", transform=ax1.transAxes, fontsize=10,
                           verticalalignment='top', 
                           bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        
        def update(frame):
            objects_to_return = []
            
            # Calculate progress (0 to 1)
            progress = frame / (frames - 1)
            
            # Phase 1 (0-20%): Show the data
            if progress <= 0.2:
                phase_progress = progress / 0.2
                n_points = max(1, int(phase_progress * len(X_flat)))
                
                # Gradually reveal data points
                scatter_data.set_offsets(np.c_[X_flat[:n_points], y_flat[:n_points]])
                reg_line.set_data([], [])  # Hide line
                scatter_pred.set_offsets(np.c_[[], []])  # Hide predictions
                
                # Clear right plot
                ax2.clear()
                ax2.set_title("Error Source Attribution")
                ax2.axis('equal')
                
                # Update MSE text
                mse_text.set_text("")
                
                objects_to_return.append(scatter_data)
                objects_to_return.append(reg_line)
                objects_to_return.append(scatter_pred)
                objects_to_return.append(mse_text)
            
            # Phase 2 (20-40%): Show regression line
            elif progress <= 0.4:
                phase_progress = (progress - 0.2) / 0.2
                n_points = max(2, int(phase_progress * len(X_sorted)))
                
                # Show all data points
                scatter_data.set_offsets(np.c_[X_flat, y_flat])
                
                # Gradually reveal regression line
                reg_line.set_data(X_sorted[:n_points], y_pred_sorted[:n_points])
                
                # Hide predictions
                scatter_pred.set_offsets(np.c_[[], []])
                
                objects_to_return.append(scatter_data)
                objects_to_return.append(reg_line)
                objects_to_return.append(scatter_pred)
                objects_to_return.append(mse_text)
            
            # Phase 3 (40-60%): Show predictions
            elif progress <= 0.6:
                phase_progress = (progress - 0.4) / 0.2
                n_points = max(1, int(phase_progress * len(X_flat)))
                
                # Show all data points and full regression line
                scatter_data.set_offsets(np.c_[X_flat, y_flat])
                reg_line.set_data(X_sorted, y_pred_sorted)
                
                # Gradually reveal predictions
                scatter_pred.set_offsets(np.c_[X_flat[:n_points], y_pred_flat[:n_points]])
                
                objects_to_return.append(scatter_data)
                objects_to_return.append(reg_line)
                objects_to_return.append(scatter_pred)
                
                # Calculate partial MSE
                if n_points > 0:
                    partial_errors = errors[:n_points]
                    partial_mse = np.mean(partial_errors**2)
                    mse_text.set_text(f"Current MSE: {partial_mse:.4f}")
                    objects_to_return.append(mse_text)
            
            # Phase 4 (60-80%): Show error lines
            elif progress <= 0.8:
                phase_progress = (progress - 0.6) / 0.2
                n_errors = max(1, int(phase_progress * len(X_flat)))
                
                # Show all data, predictions, and regression line
                scatter_data.set_offsets(np.c_[X_flat, y_flat])
                scatter_pred.set_offsets(np.c_[X_flat, y_pred_flat])
                reg_line.set_data(X_sorted, y_pred_sorted)
                
                # Create or update error lines
                while len(error_lines) < n_errors:
                    idx = len(error_lines)
                    error_line, = ax1.plot([X_flat[idx], X_flat[idx]], 
                                         [y_flat[idx], y_pred_flat[idx]],
                                         'g-', alpha=0.5, linewidth=1)
                    error_lines.append(error_line)
                
                # Update visibility of error lines
                for i, error_line in enumerate(error_lines):
                    if i < n_errors:
                        error_line.set_visible(True)
                    else:
                        error_line.set_visible(False)
                
                objects_to_return.append(scatter_data)
                objects_to_return.append(scatter_pred)
                objects_to_return.append(reg_line)
                objects_to_return.extend(error_lines)
                
                # Calculate final MSE
                mse = np.mean(errors**2)
                mse_text.set_text(f"MSE: {mse:.4f}")
                objects_to_return.append(mse_text)
            
            # Phase 5 (80-100%): Show error pie chart
            else:
                # Show all data, predictions, error lines, and regression line
                scatter_data.set_offsets(np.c_[X_flat, y_flat])
                scatter_pred.set_offsets(np.c_[X_flat, y_pred_flat])
                reg_line.set_data(X_sorted, y_pred_sorted)
                
                for error_line in error_lines:
                    error_line.set_visible(True)
                
                objects_to_return.append(scatter_data)
                objects_to_return.append(scatter_pred)
                objects_to_return.append(reg_line)
                objects_to_return.extend(error_lines)
                
                # Calculate final MSE
                mse = np.mean(errors**2)
                mse_text.set_text(f"MSE: {mse:.4f}")
                objects_to_return.append(mse_text)
                
                # Create the pie chart for error attribution
                ax2.clear()
                ax2.set_title("Error Source Attribution")
                
                # Phase progress for pie chart animation
                phase_progress = (progress - 0.8) / 0.2
                
                # Prepare pie chart data
                labels = list(error_sources.keys())
                sizes = list(error_sources.values())
                
                # Animated wedge sizes
                current_sizes = [s * phase_progress for s in sizes]
                
                # Calculate remaining portion if not full
                if phase_progress < 1.0:
                    current_sizes.append(100 - sum(current_sizes))
                    labels.append("Revealing...")
                
                # Create pie chart
                wedges, texts, autotexts = ax2.pie(
                    current_sizes, 
                    labels=labels if phase_progress > 0.5 else None,  # Only show labels after halfway
                    autopct='%1.1f%%' if phase_progress > 0.7 else None,  # Only show percentages near end
                    startangle=90,
                    colors=plt.cm.tab10.colors[:len(current_sizes)]
                )
                
                # Add legend
                if phase_progress > 0.9:
                    ax2.legend(
                        wedges, 
                        [f"{label}: {size:.1f}%" for label, size in zip(labels, sizes) if label != "Revealing..."],
                        loc="center left",
                        bbox_to_anchor=(1, 0, 0.5, 1)
                    )
                    
                ax2.axis('equal')
            
            return objects_to_return
        
        # Create animation
        anim = FuncAnimation(fig, update, frames=frames, interval=interval, blit=True)
        
        # Save the animation
        writer = animation.FFMpegWriter(fps=fps)
        anim.save(save_path, writer=writer)
        plt.close(fig) 