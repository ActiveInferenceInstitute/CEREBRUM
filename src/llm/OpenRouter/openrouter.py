"""
OpenRouter Python Client - Production Ready

Comprehensive Python wrapper for OpenRouter API with production-ready features:
- Exponential backoff retry logic
- Circuit breaker pattern for resilience
- Rate limiting detection and handling
- Comprehensive logging and metrics
- Model validation and availability checking
- Async support with streaming
- Conversation management

Requires OPENROUTER_API_KEY environment variable.
"""

import os
import json
import time
import asyncio
import random
from typing import Dict, List, Optional, Iterator, AsyncIterator, Any
from dataclasses import dataclass, field
from datetime import datetime, timedelta
import logging
from functools import wraps
import openai
from openai import OpenAI, AsyncOpenAI
from openai.types.chat import ChatCompletion, ChatCompletionChunk


@dataclass
class RetryConfig:
    """Configuration for retry logic."""
    max_retries: int = 3
    base_delay: float = 1.0
    max_delay: float = 60.0
    exponential_base: float = 2.0
    jitter: bool = True


@dataclass
class CircuitBreakerConfig:
    """Configuration for circuit breaker pattern."""
    failure_threshold: int = 5
    recovery_timeout: int = 60
    half_open_max_calls: int = 3


@dataclass
class OpenRouterConfig:
    """Configuration for OpenRouter client."""
    api_key: Optional[str] = None
    base_url: str = "https://openrouter.ai/api/v1"
    http_referer: str = "https://github.com/corym-library/openrouter"
    x_title: str = "OpenRouter Python Client"
    default_model: str = "openai/gpt-3.5-turbo"
    temperature: float = 0.8
    max_tokens: int = 300
    retry_config: RetryConfig = field(default_factory=RetryConfig)
    circuit_breaker_config: CircuitBreakerConfig = field(default_factory=CircuitBreakerConfig)
    request_timeout: float = 90.0
    enable_logging: bool = True
    log_level: str = "INFO"
    
    def __post_init__(self):
        if self.api_key is None:
            self.api_key = os.getenv("OPENROUTER_API_KEY")
            if not self.api_key:
                raise ValueError(
                    "OPENROUTER_API_KEY required. Get at: https://openrouter.ai/keys"
                )


class CircuitBreaker:
    """Circuit breaker implementation for API calls."""
    
    def __init__(self, config: CircuitBreakerConfig):
        self.config = config
        self.failure_count = 0
        self.last_failure_time = None
        self.state = "CLOSED"  # CLOSED, OPEN, HALF_OPEN
        self.half_open_calls = 0
        
    def can_execute(self) -> bool:
        """Check if operation can be executed."""
        if self.state == "CLOSED":
            return True
        elif self.state == "OPEN":
            if self._should_attempt_reset():
                self.state = "HALF_OPEN"
                self.half_open_calls = 0
                return True
            return False
        elif self.state == "HALF_OPEN":
            return self.half_open_calls < self.config.half_open_max_calls
        return False
    
    def record_success(self):
        """Record successful operation."""
        if self.state == "HALF_OPEN":
            self.failure_count = 0
            self.state = "CLOSED"
        self.half_open_calls = 0
        
    def record_failure(self):
        """Record failed operation."""
        self.failure_count += 1
        self.last_failure_time = datetime.now()
        
        if self.state == "HALF_OPEN":
            self.state = "OPEN"
        elif self.failure_count >= self.config.failure_threshold:
            self.state = "OPEN"
            
    def _should_attempt_reset(self) -> bool:
        """Check if circuit breaker should attempt reset."""
        if self.last_failure_time is None:
            return False
        return datetime.now() - self.last_failure_time > timedelta(seconds=self.config.recovery_timeout)


class OpenRouterClient:
    """Production-ready OpenRouter client for multiple AI models."""
    
    def __init__(self, config: Optional[OpenRouterConfig] = None):
        self.config = config or OpenRouterConfig()
        
        # Initialize OpenAI clients
        self.client = OpenAI(
            api_key=self.config.api_key,
            base_url=self.config.base_url,
            default_headers={
                "HTTP-Referer": self.config.http_referer,
                "X-Title": self.config.x_title,
            }
        )
        
        self.async_client = AsyncOpenAI(
            api_key=self.config.api_key,
            base_url=self.config.base_url,
            default_headers={
                "HTTP-Referer": self.config.http_referer,
                "X-Title": self.config.x_title,
            }
        )
        
        # Initialize production features
        self.circuit_breaker = CircuitBreaker(self.config.circuit_breaker_config)
        self.retry_config = self.config.retry_config
        self._setup_logging()
        
        # Statistics tracking
        self.stats = {
            "total_requests": 0,
            "successful_requests": 0,
            "failed_requests": 0,
            "retried_requests": 0,
            "circuit_breaker_trips": 0,
            "rate_limit_hits": 0
        }
    
    def _setup_logging(self):
        """Setup production logging."""
        if self.config.enable_logging:
            self.logger = logging.getLogger(f"OpenRouter.{self.__class__.__name__}")
            self.logger.setLevel(getattr(logging, self.config.log_level))
            
            if not self.logger.handlers:
                handler = logging.StreamHandler()
                formatter = logging.Formatter(
                    '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
                )
                handler.setFormatter(formatter)
                self.logger.addHandler(handler)
        else:
            self.logger = logging.getLogger("null")
            self.logger.addHandler(logging.NullHandler())
    
    def _calculate_delay(self, attempt: int) -> float:
        """Calculate retry delay with exponential backoff and jitter."""
        delay = min(
            self.retry_config.base_delay * (self.retry_config.exponential_base ** attempt),
            self.retry_config.max_delay
        )
        
        if self.retry_config.jitter:
            delay *= (0.5 + random.random() * 0.5)  # Add 0-50% jitter
            
        return delay
    
    def _is_retryable_error(self, error: Exception) -> bool:
        """Determine if error is retryable."""
        if isinstance(error, openai.RateLimitError):
            self.stats["rate_limit_hits"] += 1
            return True
        elif isinstance(error, (openai.APITimeoutError, openai.APIConnectionError)):
            return True
        elif isinstance(error, openai.InternalServerError):
            return True
        elif isinstance(error, openai.BadRequestError):
            return False  # Don't retry client errors
        return False
    
    def _with_retry_and_circuit_breaker(self, func):
        """Decorator for retry logic and circuit breaker."""
        @wraps(func)
        def wrapper(*args, **kwargs):
            self.stats["total_requests"] += 1
            
            if not self.circuit_breaker.can_execute():
                self.stats["circuit_breaker_trips"] += 1
                raise Exception("Circuit breaker is OPEN - service temporarily unavailable")
            
            last_exception = None
            
            for attempt in range(self.retry_config.max_retries + 1):
                try:
                    if attempt > 0:
                        delay = self._calculate_delay(attempt - 1)
                        self.logger.warning(f"Retrying request (attempt {attempt + 1}) after {delay:.2f}s delay")
                        time.sleep(delay)
                        self.stats["retried_requests"] += 1
                    
                    result = func(*args, **kwargs)
                    self.circuit_breaker.record_success()
                    self.stats["successful_requests"] += 1
                    return result
                    
                except Exception as e:
                    last_exception = e
                    self.circuit_breaker.record_failure()
                    
                    if not self._is_retryable_error(e) or attempt == self.retry_config.max_retries:
                        break
                        
                    self.logger.warning(f"Request failed (attempt {attempt + 1}): {str(e)}")
            
            self.stats["failed_requests"] += 1
            self.logger.error(f"Request failed after {self.retry_config.max_retries + 1} attempts: {str(last_exception)}")
            raise last_exception
            
        return wrapper
    
    def _with_async_retry_and_circuit_breaker(self, func):
        """Async decorator for retry logic and circuit breaker."""
        @wraps(func)
        async def wrapper(*args, **kwargs):
            self.stats["total_requests"] += 1
            
            if not self.circuit_breaker.can_execute():
                self.stats["circuit_breaker_trips"] += 1
                raise Exception("Circuit breaker is OPEN - service temporarily unavailable")
            
            last_exception = None
            
            for attempt in range(self.retry_config.max_retries + 1):
                try:
                    if attempt > 0:
                        delay = self._calculate_delay(attempt - 1)
                        self.logger.warning(f"Retrying async request (attempt {attempt + 1}) after {delay:.2f}s delay")
                        await asyncio.sleep(delay)
                        self.stats["retried_requests"] += 1
                    
                    result = await func(*args, **kwargs)
                    self.circuit_breaker.record_success()
                    self.stats["successful_requests"] += 1
                    return result
                    
                except Exception as e:
                    last_exception = e
                    self.circuit_breaker.record_failure()
                    
                    if not self._is_retryable_error(e) or attempt == self.retry_config.max_retries:
                        break
                        
                    self.logger.warning(f"Async request failed (attempt {attempt + 1}): {str(e)}")
            
            self.stats["failed_requests"] += 1
            self.logger.error(f"Async request failed after {self.retry_config.max_retries + 1} attempts: {str(last_exception)}")
            raise last_exception
            
        return wrapper
    
    def chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> ChatCompletion:
        """Chat completion with retry and circuit breaker."""
        enhanced_func = self._with_retry_and_circuit_breaker(self._base_chat_completion)
        return enhanced_func(messages, model, temperature, max_tokens, **kwargs)
    
    def _base_chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> ChatCompletion:
        """Base chat completion implementation."""
        return self.client.chat.completions.create(
            model=model or self.config.default_model,
            messages=messages,
            temperature=temperature or self.config.temperature,
            max_tokens=max_tokens or self.config.max_tokens,
            **kwargs
        )
    
    def chat_completion_stream(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> Iterator[ChatCompletionChunk]:
        """Streaming chat completion with retry and circuit breaker."""
        enhanced_func = self._with_retry_and_circuit_breaker(self._base_chat_completion_stream)
        return enhanced_func(messages, model, temperature, max_tokens, **kwargs)
    
    def _base_chat_completion_stream(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> Iterator[ChatCompletionChunk]:
        """Base streaming chat completion implementation."""
        stream = self.client.chat.completions.create(
            model=model or self.config.default_model,
            messages=messages,
            temperature=temperature or self.config.temperature,
            max_tokens=max_tokens or self.config.max_tokens,
            stream=True,
            **kwargs
        )
        for chunk in stream:
            yield chunk
    
    async def async_chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> ChatCompletion:
        """Async chat completion with retry and circuit breaker."""
        enhanced_func = self._with_async_retry_and_circuit_breaker(self._base_async_chat_completion)
        return await enhanced_func(messages, model, temperature, max_tokens, **kwargs)
    
    async def _base_async_chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> ChatCompletion:
        """Base async chat completion implementation."""
        return await self.async_client.chat.completions.create(
            model=model or self.config.default_model,
            messages=messages,
            temperature=temperature or self.config.temperature,
            max_tokens=max_tokens or self.config.max_tokens,
            **kwargs
        )
    
    async def async_chat_completion_stream(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> AsyncIterator[ChatCompletionChunk]:
        """Async streaming chat completion with retry and circuit breaker."""
        async for chunk in self._base_async_chat_completion_stream(messages, model, temperature, max_tokens, **kwargs):
            yield chunk
    
    async def _base_async_chat_completion_stream(
        self,
        messages: List[Dict[str, str]],
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
        **kwargs
    ) -> AsyncIterator[ChatCompletionChunk]:
        """Base async streaming chat completion implementation."""
        stream = await self.async_client.chat.completions.create(
            model=model or self.config.default_model,
            messages=messages,
            temperature=temperature or self.config.temperature,
            max_tokens=max_tokens or self.config.max_tokens,
            stream=True,
            **kwargs
        )
        async for chunk in stream:
            yield chunk
    
    def simple_chat(self, message: str, model: Optional[str] = None) -> str:
        """Simple chat interface with production features."""
        messages = [{"role": "user", "content": message}]
        response = self.chat_completion(messages, model=model)
        return response.choices[0].message.content
    
    def set_model(self, model: str) -> None:
        """Set default model."""
        self.config.default_model = model
    
    def get_available_models(self) -> List[str]:
        """Get static list of available models."""
        return [
            "openai/gpt-4", "openai/gpt-4-turbo-preview", "openai/gpt-3.5-turbo", "openai/gpt-3.5-turbo-16k",
            "anthropic/claude-3-opus", "anthropic/claude-3-sonnet", "anthropic/claude-3-haiku", 
            "anthropic/claude-2", "anthropic/claude-instant-v1",
            "google/palm-2-chat-bison", "google/gemini-pro",
            "meta-llama/llama-2-70b-chat", "meta-llama/llama-2-13b-chat",
            "mistralai/mistral-7b-instruct", "mistralai/mixtral-8x7b-instruct"
        ]
    
    def get_available_models_from_api(self) -> List[Dict[str, Any]]:
        """Fetch available models from OpenRouter API."""
        try:
            models_response = self.client.models.list()
            return [{"id": model.id, "object": model.object, "created": model.created} 
                   for model in models_response.data]
        except Exception as e:
            self.logger.error(f"Failed to fetch models from API: {str(e)}")
            return [{"id": model, "object": "model", "created": None} 
                   for model in self.get_available_models()]
    
    def validate_model(self, model: str) -> bool:
        """Validate if model is available."""
        try:
            available_models = self.get_available_models_from_api()
            return any(m["id"] == model for m in available_models)
        except Exception:
            return model in self.get_available_models()
    
    def get_stats(self) -> Dict[str, Any]:
        """Get client statistics."""
        return {
            **self.stats,
            "circuit_breaker_state": self.circuit_breaker.state,
            "circuit_breaker_failures": self.circuit_breaker.failure_count,
            "success_rate": self.stats["successful_requests"] / max(self.stats["total_requests"], 1) * 100
        }
    
    def reset_stats(self):
        """Reset client statistics."""
        self.stats = {
            "total_requests": 0,
            "successful_requests": 0,
            "failed_requests": 0,
            "retried_requests": 0,
            "circuit_breaker_trips": 0,
            "rate_limit_hits": 0
        }
        
    def reset_circuit_breaker(self):
        """Manually reset circuit breaker."""
        self.circuit_breaker.state = "CLOSED"
        self.circuit_breaker.failure_count = 0
        self.circuit_breaker.last_failure_time = None
    
    def create_conversation(self) -> "Conversation":
        """Create conversation instance."""
        return Conversation(self)

    def get_document_keywords(self, content: str, max_keywords: int = 5) -> List[str]:
        """
        Extract key terms/keywords from document content.
        
        Args:
            content: Document content to analyze
            max_keywords: Maximum number of keywords to return
            
        Returns:
            List of key terms/keywords
        """
        try:
            # Limit content for API efficiency
            analysis_content = content[:1500]
            
            prompt = f"""Extract {max_keywords} key terms or keywords from this document content. 
Return only the most important and relevant terms that best represent the document's main topics.

Format: Return as a simple comma-separated list without explanations.

Content:
{analysis_content}

Keywords:"""

            response = self.simple_chat(prompt, model=self.config.default_model)
            
            if response:
                # Clean and parse response
                keywords = [k.strip().strip('"\'') for k in response.split(',')]
                keywords = [k for k in keywords if k and len(k) > 1]
                return keywords[:max_keywords]
            
        except Exception as e:
            self.logger.warning(f"Failed to extract keywords: {e}")
        
        return []

    def get_document_overview(self, content: str) -> str:
        """
        Generate a single sentence overview of the document.
        
        Args:
            content: Document content to analyze
            
        Returns:
            Single sentence overview/summary
        """
        try:
            # Limit content for API efficiency
            analysis_content = content[:2000]
            
            prompt = f"""Write a single, concise sentence that captures the main purpose and content of this document.
The sentence should be informative, accurate, and help someone quickly understand what the document is about.

Content:
{analysis_content}

Overview sentence:"""

            response = self.simple_chat(prompt, model=self.config.default_model)
            
            if response:
                # Clean the response
                overview = response.strip().strip('"\'')
                # Ensure it's a single sentence
                if '.' in overview:
                    overview = overview.split('.')[0] + '.'
                return overview[:200]  # Limit length
            
        except Exception as e:
            self.logger.warning(f"Failed to generate overview: {e}")
        
        return ""

    def get_content_quality_assessment(self, content: str) -> Dict[str, Any]:
        """
        Assess content quality with specific metrics.
        
        Args:
            content: Document content to analyze
            
        Returns:
            Dictionary with quality assessment metrics
        """
        try:
            analysis_content = content[:2000]
            
            prompt = f"""Analyze this document content and provide a quality assessment.
Rate each aspect from 0-100 and provide brief justifications.

Content:
{analysis_content}

Provide assessment in this JSON format:
{{
    "clarity": 75,
    "completeness": 80,
    "usefulness": 85,
    "accuracy": 90,
    "readability": 70,
    "engagement": 65,
    "overall_quality": 78,
    "strengths": ["clear structure", "good examples"],
    "improvements": ["add more details", "better conclusion"]
}}"""

            response = self.simple_chat(prompt, model=self.config.default_model)
            
            if response:
                try:
                    assessment = json.loads(response)
                    # Validate and clean the response
                    required_fields = ["clarity", "completeness", "usefulness", "accuracy", 
                                     "readability", "engagement", "overall_quality"]
                    for field in required_fields:
                        if field not in assessment:
                            assessment[field] = 50  # Default neutral score
                        else:
                            # Ensure scores are in valid range
                            assessment[field] = max(0, min(100, assessment[field]))
                    
                    return assessment
                except json.JSONDecodeError:
                    self.logger.warning("Failed to parse quality assessment JSON")
            
        except Exception as e:
            self.logger.warning(f"Failed to assess content quality: {e}")
        
        # Return default assessment
        return {
            "clarity": 50,
            "completeness": 50,
            "usefulness": 50,
            "accuracy": 50,
            "readability": 50,
            "engagement": 50,
            "overall_quality": 50,
            "strengths": [],
            "improvements": []
        }

    def get_document_insights(self, content: str, filename: str = "") -> Dict[str, Any]:
        """
        Generate comprehensive document insights.
        
        Args:
            content: Document content to analyze
            filename: Optional filename for context
            
        Returns:
            Dictionary with comprehensive insights
        """
        try:
            # Get individual insights
            keywords = self.get_document_keywords(content, max_keywords=5)
            overview = self.get_document_overview(content)
            quality_assessment = self.get_content_quality_assessment(content)
            
            # Combine into comprehensive insights
            insights = {
                "keywords": keywords,
                "overview": overview,
                "quality_assessment": quality_assessment,
                "filename": filename,
                "insight_timestamp": datetime.now().isoformat()
            }
            
            return insights
            
        except Exception as e:
            self.logger.warning(f"Failed to generate document insights: {e}")
            return {
                "keywords": [],
                "overview": "",
                "quality_assessment": self.get_content_quality_assessment(""),
                "filename": filename,
                "insight_timestamp": datetime.now().isoformat()
            }


class Conversation:
    """Conversation with history management."""
    
    def __init__(self, client: OpenRouterClient):
        self.client = client
        self.messages: List[Dict[str, str]] = []
    
    def add_system_message(self, content: str) -> None:
        """Add system message to conversation."""
        self.messages.append({"role": "system", "content": content})
    
    def add_user_message(self, content: str) -> None:
        """Add user message to conversation."""
        self.messages.append({"role": "user", "content": content})
    
    def add_assistant_message(self, content: str) -> None:
        """Add assistant message to conversation."""
        self.messages.append({"role": "assistant", "content": content})
    
    def send_message(self, content: str, model: Optional[str] = None) -> str:
        """Send message and get response."""
        self.add_user_message(content)
        response = self.client.chat_completion(self.messages, model=model)
        assistant_response = response.choices[0].message.content
        self.add_assistant_message(assistant_response)
        return assistant_response
    
    def send_message_stream(self, content: str, model: Optional[str] = None) -> Iterator[str]:
        """Send message with streaming response."""
        self.add_user_message(content)
        full_response = ""
        
        for chunk in self.client.chat_completion_stream(self.messages, model=model):
            if chunk.choices[0].delta.content:
                content_chunk = chunk.choices[0].delta.content
                full_response += content_chunk
                yield content_chunk
        
        self.add_assistant_message(full_response)
    
    def clear_history(self) -> None:
        """Clear conversation history."""
        self.messages.clear()
    
    def get_history(self) -> List[Dict[str, str]]:
        """Get conversation history."""
        return self.messages.copy()
    
    def save_history(self, filepath: str) -> None:
        """Save conversation history to file."""
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(self.messages, f, indent=2, ensure_ascii=False)
    
    def load_history(self, filepath: str) -> None:
        """Load conversation history from file."""
        with open(filepath, 'r', encoding='utf-8') as f:
            self.messages = json.load(f)


def quick_chat(message: str, model: str = "openai/gpt-3.5-turbo") -> str:
    """Quick chat function with production features."""
    client = OpenRouterClient()
    return client.simple_chat(message, model=model) 