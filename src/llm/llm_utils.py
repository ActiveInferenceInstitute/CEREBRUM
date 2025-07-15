"""
LLM utilities and helper functions.
"""

import json
import re
import time
from typing import Dict, Any, List, Optional, Union
from datetime import datetime
import requests
from requests.exceptions import RequestException


class LLMUtils:
    """Utility functions for LLM operations."""
    
    def __init__(self, config_manager=None):
        """Initialize LLM utilities."""
        self.config = config_manager
    
    @staticmethod
    def count_tokens(text: str, model: str = "gpt-3.5") -> int:
        """
        Estimate token count for text.
        
        Args:
            text: Text to count tokens for
            model: Model type for estimation
            
        Returns:
            Estimated token count
        """
        # Simple estimation: ~4 characters per token for most models
        return len(text) // 4
    
    @staticmethod
    def count_words(text: str) -> int:
        """Count words in text."""
        return len(text.split())
    
    @staticmethod
    def count_lines(text: str) -> int:
        """Count lines in text."""
        return len(text.splitlines())
    
    @staticmethod
    def get_text_stats(text: str) -> Dict[str, int]:
        """
        Get comprehensive text statistics.
        
        Args:
            text: Text to analyze
            
        Returns:
            Dictionary with text statistics
        """
        return {
            "characters": len(text),
            "characters_no_spaces": len(text.replace(" ", "")),
            "words": LLMUtils.count_words(text),
            "lines": LLMUtils.count_lines(text),
            "paragraphs": len([p for p in text.split("\n\n") if p.strip()]),
            "sentences": len(re.findall(r'[.!?]+', text)),
            "estimated_tokens": LLMUtils.count_tokens(text)
        }
    
    def validate_api_response(self, response: Dict[str, Any]) -> Dict[str, Any]:
        """
        Validate and clean API response.
        
        Args:
            response: Raw API response
            
        Returns:
            Validated response with error handling
        """
        try:
            if "error" in response:
                return {
                    "status": "error",
                    "error": response["error"],
                    "timestamp": datetime.now().isoformat()
                }
            
            if "choices" not in response or not response["choices"]:
                return {
                    "status": "error", 
                    "error": "No choices in response",
                    "raw_response": response
                }
            
            choice = response["choices"][0]
            if "message" not in choice:
                return {
                    "status": "error",
                    "error": "No message in choice",
                    "raw_response": response
                }
            
            content = choice["message"].get("content", "")
            
            return {
                "status": "success",
                "content": content,
                "usage": response.get("usage", {}),
                "citations": response.get("citations", []),
                "model": response.get("model", "unknown"),
                "timestamp": datetime.now().isoformat(),
                "text_stats": self.get_text_stats(content)
            }
            
        except Exception as e:
            return {
                "status": "error",
                "error": f"Validation failed: {str(e)}",
                "raw_response": response
            }
    
    @staticmethod
    def extract_json_from_text(text: str) -> Optional[Dict[str, Any]]:
        """
        Extract JSON from text that may contain markdown code blocks.
        
        Args:
            text: Text that may contain JSON
            
        Returns:
            Parsed JSON or None if extraction fails
        """
        # Try to find JSON in code blocks
        json_patterns = [
            r'```json\s*(.*?)\s*```',
            r'```\s*(.*?)\s*```',
            r'\{.*\}',
            r'\[.*\]'
        ]
        
        for pattern in json_patterns:
            matches = re.findall(pattern, text, re.DOTALL)
            for match in matches:
                try:
                    return json.loads(match.strip())
                except json.JSONDecodeError:
                    continue
        
        # Try parsing the entire text as JSON
        try:
            return json.loads(text.strip())
        except json.JSONDecodeError:
            return None
    
    def format_citations(self, citations: List[Union[str, Dict[str, Any]]]) -> str:
        """
        Format citations into markdown.
        
        Args:
            citations: List of citations (strings or dicts)
            
        Returns:
            Formatted citations markdown
        """
        if not citations:
            return "No citations available."
        
        formatted = "## Citations\n\n"
        
        for i, citation in enumerate(citations, 1):
            if isinstance(citation, dict):
                title = citation.get('title', 'Unknown Title')
                url = citation.get('url', '#')
                text = citation.get('text', '')
                
                formatted += f"{i}. **[{title}]({url})**\n"
                if text:
                    formatted += f"   {text}\n"
            else:
                formatted += f"{i}. {citation}\n"
            
            formatted += "\n"
        
        return formatted
    
    def rate_limit_handler(self, func, *args, max_retries: int = 3, delay: float = 1.0, **kwargs):
        """
        Handle rate limiting with exponential backoff.
        
        Args:
            func: Function to call
            *args: Function arguments
            max_retries: Maximum retry attempts
            delay: Initial delay between retries
            **kwargs: Function keyword arguments
            
        Returns:
            Function result or raises exception
        """
        for attempt in range(max_retries):
            try:
                return func(*args, **kwargs)
            except RequestException as e:
                if attempt == max_retries - 1:
                    raise
                
                # Check if it's a rate limit error
                if hasattr(e, 'response') and e.response:
                    status_code = e.response.status_code
                    if status_code in [429, 503]:  # Rate limit or service unavailable
                        wait_time = delay * (2 ** attempt)
                        time.sleep(wait_time)
                        continue
                
                raise
    
    def chunk_text(self, text: str, max_chunk_size: int = 4000, overlap: int = 200) -> List[str]:
        """
        Split text into chunks for processing.
        
        Args:
            text: Text to chunk
            max_chunk_size: Maximum characters per chunk
            overlap: Character overlap between chunks
            
        Returns:
            List of text chunks
        """
        if len(text) <= max_chunk_size:
            return [text]
        
        chunks = []
        start = 0
        
        while start < len(text):
            end = start + max_chunk_size
            
            # Try to break at sentence boundaries
            if end < len(text):
                # Look for sentence ending near the chunk boundary
                sentence_end = text.rfind('.', start, end)
                if sentence_end > start + max_chunk_size // 2:
                    end = sentence_end + 1
                else:
                    # Fall back to word boundaries
                    word_end = text.rfind(' ', start, end)
                    if word_end > start + max_chunk_size // 2:
                        end = word_end
            
            chunk = text[start:end].strip()
            if chunk:
                chunks.append(chunk)
            
            start = end - overlap
            if start >= len(text):
                break
        
        return chunks
    
    def generate_prompt_template(
        self, 
        task_type: str,
        context: Optional[str] = None,
        examples: Optional[List[str]] = None,
        constraints: Optional[List[str]] = None
    ) -> str:
        """
        Generate a structured prompt template.
        
        Args:
            task_type: Type of task (research, analysis, summarization, etc.)
            context: Additional context for the task
            examples: Example inputs/outputs
            constraints: Task constraints or requirements
            
        Returns:
            Formatted prompt template
        """
        prompt_templates = {
            "research": "You are a professional researcher. Research the following topic thoroughly and provide comprehensive findings with sources.",
            "analysis": "You are an expert analyst. Analyze the following information and provide detailed insights.",
            "summarization": "You are a professional summarizer. Create a concise, accurate summary of the following content.",
            "fact_check": "You are a fact-checker. Verify the claims in the following text and identify any false or misleading information.",
            "writing": "You are a professional writer. Create high-quality content based on the following requirements."
        }
        
        base_prompt = prompt_templates.get(task_type, "You are a helpful AI assistant.")
        
        if context:
            base_prompt += f"\n\nContext: {context}"
        
        if constraints:
            base_prompt += f"\n\nConstraints:\n" + "\n".join(f"- {c}" for c in constraints)
        
        if examples:
            base_prompt += f"\n\nExamples:\n" + "\n".join(f"Example {i+1}: {ex}" for i, ex in enumerate(examples))
        
        return base_prompt
    
    def create_system_message(self, role: str, instructions: str, personality: Optional[str] = None) -> Dict[str, str]:
        """
        Create a system message for chat completion.
        
        Args:
            role: AI role (researcher, analyst, etc.)
            instructions: Specific instructions
            personality: Optional personality traits
            
        Returns:
            System message dictionary
        """
        content = f"You are a {role}. {instructions}"
        
        if personality:
            content += f" {personality}"
        
        return {"role": "system", "content": content}
    
    def log_llm_interaction(
        self,
        model: str,
        prompt: str,
        response: str,
        metadata: Optional[Dict[str, Any]] = None
    ):
        """
        Log LLM interaction for analysis and debugging.
        
        Args:
            model: Model used
            prompt: Input prompt
            response: Model response
            metadata: Additional metadata
        """
        interaction_log = {
            "timestamp": datetime.now().isoformat(),
            "model": model,
            "prompt_stats": self.get_text_stats(prompt),
            "response_stats": self.get_text_stats(response),
            "metadata": metadata or {}
        }
        
        # Log summary (without full content for brevity)
        print(
            f"LLM interaction - Model: {model}, "
            f"Prompt: {interaction_log['prompt_stats']['words']} words, "
            f"Response: {interaction_log['response_stats']['words']} words"
        ) 