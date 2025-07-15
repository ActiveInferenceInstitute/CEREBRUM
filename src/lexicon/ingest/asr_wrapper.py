"""
LEXICON ASR Wrapper

Wrapper for audio transcription (ASR) services.
"""

import os
import subprocess
import json
import tempfile
from pathlib import Path
from typing import Optional, Union, Dict, Any, List
import logging

logger = logging.getLogger("lexicon.ingest.asr")

# Try to import whisper, but gracefully handle if not installed
try:
    import whisper
    WHISPER_AVAILABLE = True
except ImportError:
    WHISPER_AVAILABLE = False
    logger.warning("Whisper not available - some ASR features will be limited")


def transcribe_audio(audio_path: Union[str, Path], 
                    model: str = "base",
                    language: Optional[str] = None) -> str:
    """
    Transcribe audio file to text.
    
    Args:
        audio_path: Path to audio file
        model: Whisper model name (tiny, base, small, medium, large)
        language: Optional language code
        
    Returns:
        Transcription text
    """
    audio_path = Path(audio_path)
    
    if not audio_path.exists():
        raise FileNotFoundError(f"Audio file not found: {audio_path}")
    
    # Use whisper if available
    if WHISPER_AVAILABLE:
        logger.info(f"Transcribing audio using whisper: {audio_path}")
        return _transcribe_with_whisper(audio_path, model, language)
    
    # Fall back to external command (assumes whisper CLI is installed)
    logger.info(f"Whisper module not available, trying external whisper: {audio_path}")
    return _transcribe_with_external(audio_path, model, language)


def _transcribe_with_whisper(audio_path: Path, model: str, language: Optional[str]) -> str:
    """
    Transcribe audio using the whisper Python module.
    
    Args:
        audio_path: Path to audio file
        model: Whisper model name
        language: Optional language code
        
    Returns:
        Transcription text
    """
    try:
        # Load model
        model_obj = whisper.load_model(model)
        
        # Transcribe audio
        if language:
            result = model_obj.transcribe(str(audio_path), language=language)
        else:
            result = model_obj.transcribe(str(audio_path))
        
        # Return transcription text
        return result["text"]
        
    except Exception as e:
        logger.error(f"Whisper transcription failed: {str(e)}")
        raise RuntimeError(f"Audio transcription failed: {str(e)}")


def _transcribe_with_external(audio_path: Path, model: str, language: Optional[str]) -> str:
    """
    Transcribe audio using external whisper command.
    
    Args:
        audio_path: Path to audio file
        model: Whisper model name
        language: Optional language code
        
    Returns:
        Transcription text
    """
    try:
        # Create temporary file for output
        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as tmp:
            output_path = tmp.name
        
        # Build command
        cmd = ["whisper", str(audio_path), "--model", model, "--output_format", "json", "--output_dir", os.path.dirname(output_path)]
        
        if language:
            cmd.extend(["--language", language])
            
        # Run command
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        
        # Parse output
        output_file = Path(output_path)
        if output_file.exists():
            with open(output_file, "r") as f:
                data = json.load(f)
                
            # Clean up
            try:
                os.unlink(output_file)
            except:
                pass
                
            return data.get("text", "")
        else:
            # Extract text from stdout
            return result.stdout.strip()
            
    except subprocess.CalledProcessError as e:
        logger.error(f"External whisper failed: {e.stderr}")
        raise RuntimeError(f"Audio transcription failed: {e.stderr}")
    except Exception as e:
        logger.error(f"External whisper error: {str(e)}")
        raise RuntimeError(f"Audio transcription failed: {str(e)}")


def get_supported_languages() -> List[Dict[str, str]]:
    """
    Get list of supported languages.
    
    Returns:
        List of language dictionaries with code and name
    """
    # Most common languages supported by Whisper
    return [
        {"code": "en", "name": "English"},
        {"code": "es", "name": "Spanish"},
        {"code": "fr", "name": "French"},
        {"code": "de", "name": "German"},
        {"code": "it", "name": "Italian"},
        {"code": "pt", "name": "Portuguese"},
        {"code": "nl", "name": "Dutch"},
        {"code": "ru", "name": "Russian"},
        {"code": "zh", "name": "Chinese"},
        {"code": "ja", "name": "Japanese"}
    ] 