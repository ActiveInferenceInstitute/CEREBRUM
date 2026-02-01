"""
Ollama Client for Local LLM Inference

This module provides a comprehensive client for interacting with local Ollama instances,
supporting text generation, chat completions, embeddings, and model management.

Usage:
    from src.llm.ollama import OllamaClient
    
    client = OllamaClient()
    response = client.generate("What is active inference?")
    print(response)
"""

import json
import logging
import requests
from typing import Dict, Any, Optional, List, Generator, Union
from dataclasses import dataclass
from enum import Enum

logger = logging.getLogger(__name__)


class OllamaError(Exception):
    """Base exception for Ollama client errors."""
    pass


class OllamaConnectionError(OllamaError):
    """Raised when unable to connect to Ollama server."""
    pass


class OllamaModelError(OllamaError):
    """Raised when there's a model-related error."""
    pass


@dataclass
class OllamaResponse:
    """Structured response from Ollama API."""
    text: str
    model: str
    done: bool
    total_duration: Optional[int] = None
    load_duration: Optional[int] = None
    prompt_eval_count: Optional[int] = None
    eval_count: Optional[int] = None
    eval_duration: Optional[int] = None
    
    @property
    def tokens_per_second(self) -> Optional[float]:
        """Calculate tokens per second if metrics are available."""
        if self.eval_count and self.eval_duration:
            return self.eval_count / (self.eval_duration / 1e9)
        return None


@dataclass
class ChatMessage:
    """Represents a chat message."""
    role: str  # 'system', 'user', 'assistant'
    content: str
    images: Optional[List[str]] = None  # Base64 encoded images for vision models


class OllamaClient:
    """
    Client for interacting with local Ollama LLM instances.
    
    Ollama must be running locally (default: http://localhost:11434).
    Install Ollama from: https://ollama.ai
    
    Attributes:
        base_url: Base URL for Ollama API
        default_model: Default model to use for requests
        timeout: Request timeout in seconds
    
    Example:
        >>> client = OllamaClient(model="llama3.2")
        >>> response = client.generate("Explain CEREBRUM in one sentence.")
        >>> print(response.text)
    """
    
    DEFAULT_BASE_URL = "http://localhost:11434"
    DEFAULT_MODEL = "llama3.2"
    DEFAULT_TIMEOUT = 120
    
    def __init__(
        self,
        base_url: str = None,
        model: str = None,
        timeout: int = None
    ):
        """
        Initialize the Ollama client.
        
        Args:
            base_url: Ollama server URL (default: http://localhost:11434)
            model: Default model to use (default: llama3.2)
            timeout: Request timeout in seconds (default: 120)
        """
        self.base_url = base_url or self.DEFAULT_BASE_URL
        self.default_model = model or self.DEFAULT_MODEL
        self.timeout = timeout or self.DEFAULT_TIMEOUT
        
        logger.info(f"Initialized OllamaClient with model={self.default_model}")
    
    def _request(
        self,
        endpoint: str,
        method: str = "POST",
        data: Dict[str, Any] = None,
        stream: bool = False
    ) -> Union[Dict[str, Any], Generator]:
        """
        Make a request to the Ollama API.
        
        Args:
            endpoint: API endpoint (e.g., '/api/generate')
            method: HTTP method
            data: Request payload
            stream: Whether to stream the response
            
        Returns:
            Response data or generator for streaming responses
            
        Raises:
            OllamaConnectionError: If unable to connect to Ollama
            OllamaError: For other API errors
        """
        url = f"{self.base_url}{endpoint}"
        
        try:
            if method == "POST":
                response = requests.post(
                    url,
                    json=data,
                    timeout=self.timeout,
                    stream=stream
                )
            elif method == "GET":
                response = requests.get(url, timeout=self.timeout)
            elif method == "DELETE":
                response = requests.delete(url, json=data, timeout=self.timeout)
            else:
                raise ValueError(f"Unsupported HTTP method: {method}")
            
            response.raise_for_status()
            
            if stream:
                return self._stream_response(response)
            
            return response.json()
            
        except requests.exceptions.ConnectionError:
            raise OllamaConnectionError(
                f"Cannot connect to Ollama at {self.base_url}. "
                "Make sure Ollama is running: `ollama serve`"
            )
        except requests.exceptions.Timeout:
            raise OllamaError(f"Request timed out after {self.timeout}s")
        except requests.exceptions.HTTPError as e:
            raise OllamaError(f"HTTP error: {e}")
    
    def _stream_response(self, response: requests.Response) -> Generator:
        """Stream response line by line."""
        for line in response.iter_lines():
            if line:
                yield json.loads(line.decode('utf-8'))
    
    def is_available(self) -> bool:
        """
        Check if Ollama server is available.
        
        Returns:
            True if Ollama is running and accessible
        """
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=5)
            return response.status_code == 200
        except:
            return False
    
    def list_models(self) -> List[Dict[str, Any]]:
        """
        List all available models.
        
        Returns:
            List of model information dictionaries
        """
        response = self._request("/api/tags", method="GET")
        return response.get("models", [])
    
    def pull_model(self, model: str, stream: bool = True) -> Union[Dict, Generator]:
        """
        Pull (download) a model from Ollama registry.
        
        Args:
            model: Model name (e.g., 'llama3.2', 'mistral')
            stream: Stream progress updates
            
        Returns:
            Progress updates or final status
        """
        logger.info(f"Pulling model: {model}")
        return self._request(
            "/api/pull",
            data={"name": model},
            stream=stream
        )
    
    def delete_model(self, model: str) -> Dict[str, Any]:
        """
        Delete a model from local storage.
        
        Args:
            model: Model name to delete
            
        Returns:
            Deletion status
        """
        logger.info(f"Deleting model: {model}")
        return self._request("/api/delete", method="DELETE", data={"name": model})
    
    def generate(
        self,
        prompt: str,
        model: str = None,
        system: str = None,
        temperature: float = 0.7,
        max_tokens: int = None,
        stream: bool = False,
        **kwargs
    ) -> Union[OllamaResponse, Generator]:
        """
        Generate text completion.
        
        Args:
            prompt: Input prompt
            model: Model to use (defaults to client's default_model)
            system: System prompt for context
            temperature: Sampling temperature (0.0-1.0)
            max_tokens: Maximum tokens to generate
            stream: Stream the response
            **kwargs: Additional Ollama options
            
        Returns:
            OllamaResponse or generator for streaming
            
        Example:
            >>> response = client.generate(
            ...     "Explain case grammar in linguistics",
            ...     temperature=0.5
            ... )
            >>> print(response.text)
        """
        model = model or self.default_model
        
        data = {
            "model": model,
            "prompt": prompt,
            "stream": stream,
            "options": {
                "temperature": temperature,
                **kwargs
            }
        }
        
        if system:
            data["system"] = system
        
        if max_tokens:
            data["options"]["num_predict"] = max_tokens
        
        logger.debug(f"Generating with model={model}, prompt_length={len(prompt)}")
        
        if stream:
            return self._request("/api/generate", data=data, stream=True)
        
        response = self._request("/api/generate", data=data)
        
        return OllamaResponse(
            text=response.get("response", ""),
            model=response.get("model", model),
            done=response.get("done", False),
            total_duration=response.get("total_duration"),
            load_duration=response.get("load_duration"),
            prompt_eval_count=response.get("prompt_eval_count"),
            eval_count=response.get("eval_count"),
            eval_duration=response.get("eval_duration")
        )
    
    def chat(
        self,
        messages: List[ChatMessage],
        model: str = None,
        temperature: float = 0.7,
        max_tokens: int = None,
        stream: bool = False,
        **kwargs
    ) -> Union[OllamaResponse, Generator]:
        """
        Chat completion with conversation history.
        
        Args:
            messages: List of ChatMessage objects
            model: Model to use
            temperature: Sampling temperature
            max_tokens: Maximum tokens to generate
            stream: Stream the response
            **kwargs: Additional options
            
        Returns:
            OllamaResponse or generator
            
        Example:
            >>> messages = [
            ...     ChatMessage(role="system", content="You are a helpful assistant."),
            ...     ChatMessage(role="user", content="What is CEREBRUM?")
            ... ]
            >>> response = client.chat(messages)
            >>> print(response.text)
        """
        model = model or self.default_model
        
        # Convert ChatMessage objects to dicts
        message_dicts = []
        for msg in messages:
            msg_dict = {"role": msg.role, "content": msg.content}
            if msg.images:
                msg_dict["images"] = msg.images
            message_dicts.append(msg_dict)
        
        data = {
            "model": model,
            "messages": message_dicts,
            "stream": stream,
            "options": {
                "temperature": temperature,
                **kwargs
            }
        }
        
        if max_tokens:
            data["options"]["num_predict"] = max_tokens
        
        logger.debug(f"Chat with model={model}, messages={len(messages)}")
        
        if stream:
            return self._request("/api/chat", data=data, stream=True)
        
        response = self._request("/api/chat", data=data)
        
        return OllamaResponse(
            text=response.get("message", {}).get("content", ""),
            model=response.get("model", model),
            done=response.get("done", False),
            total_duration=response.get("total_duration"),
            load_duration=response.get("load_duration"),
            prompt_eval_count=response.get("prompt_eval_count"),
            eval_count=response.get("eval_count"),
            eval_duration=response.get("eval_duration")
        )
    
    def embeddings(
        self,
        text: Union[str, List[str]],
        model: str = None
    ) -> List[List[float]]:
        """
        Generate embeddings for text.
        
        Args:
            text: Single text or list of texts
            model: Embedding model (default uses client's model)
            
        Returns:
            List of embedding vectors
            
        Example:
            >>> embeddings = client.embeddings("CEREBRUM framework")
            >>> print(f"Embedding dimension: {len(embeddings[0])}")
        """
        model = model or self.default_model
        
        if isinstance(text, str):
            text = [text]
        
        embeddings = []
        for t in text:
            response = self._request(
                "/api/embeddings",
                data={"model": model, "prompt": t}
            )
            embeddings.append(response.get("embedding", []))
        
        return embeddings
    
    def analyze_cerebrum_model(
        self,
        model_description: str,
        analysis_type: str = "case_analysis"
    ) -> OllamaResponse:
        """
        Use LLM to analyze a CEREBRUM model description.
        
        Args:
            model_description: Description of the CEREBRUM model
            analysis_type: Type of analysis ('case_analysis', 'transformation', 'optimization')
            
        Returns:
            Analysis response
        """
        prompts = {
            "case_analysis": (
                "Analyze the following CEREBRUM model and identify which grammatical cases "
                "(NOMINATIVE, ACCUSATIVE, GENITIVE, DATIVE, INSTRUMENTAL, LOCATIVE, ABLATIVE, VOCATIVE) "
                "would be most appropriate for different aspects of its functionality:\n\n"
            ),
            "transformation": (
                "Given this CEREBRUM model, suggest optimal case transformations "
                "for different computational scenarios:\n\n"
            ),
            "optimization": (
                "Analyze this CEREBRUM model and suggest optimizations based on "
                "active inference principles:\n\n"
            )
        }
        
        system_prompt = (
            "You are an expert in the CEREBRUM framework, which integrates linguistic case systems "
            "with cognitive computational principles. Provide detailed, technical analysis."
        )
        
        prompt = prompts.get(analysis_type, prompts["case_analysis"]) + model_description
        
        return self.generate(
            prompt=prompt,
            system=system_prompt,
            temperature=0.3  # Lower temperature for analytical tasks
        )
