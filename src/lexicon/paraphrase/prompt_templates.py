"""
LEXICON Paraphrase Prompt Templates

Prompt templates for paraphrase generation.
"""

from typing import Dict, List, Any, Optional


def get_paraphrase_prompt(text: str, key_entities: List[Dict[str, Any]] = None) -> str:
    """
    Generate prompt for paraphrase generation.
    
    Args:
        text: Text to paraphrase
        key_entities: List of key entities to focus on
        
    Returns:
        Prompt for paraphrase generation
    """
    # Create entity focus section if entities provided
    entity_focus = ""
    if key_entities:
        entity_focus = "Focus on accurately preserving the meaning of these key entities:\n"
        for entity in key_entities:
            entity_focus += f"- {entity['text']} ({entity['case']})\n"
        entity_focus += "\n"
    
    prompt = f"""Generate 3-5 concise micro-paraphrases of the following text. 
Each paraphrase should:
1. Preserve the core meaning of the original text
2. Be roughly the same length or shorter
3. Use different wording and sentence structure when possible
4. Be clear and fluent

{entity_focus}Original text:
"{text}"

Provide your paraphrases in this JSON format:
```json
[
  {{"text": "First paraphrase", "quality": 0.9}},
  {{"text": "Second paraphrase", "quality": 0.8}},
  {{"text": "Third paraphrase", "quality": 0.7}}
]
```

For each paraphrase, include a quality score (0.0-1.0) that reflects how well it preserves meaning while using different wording.
"""
    return prompt


def get_quality_prompt(text: str, paraphrases: List[Dict[str, Any]]) -> str:
    """
    Generate prompt for paraphrase quality assessment.
    
    Args:
        text: Original text
        paraphrases: List of paraphrases
        
    Returns:
        Prompt for quality assessment
    """
    paraphrase_list = "\n".join(f"{i+1}. {p['text']}" for i, p in enumerate(paraphrases))
    
    prompt = f"""Evaluate the quality of these paraphrases compared to the original text.

Original text:
"{text}"

Paraphrases:
{paraphrase_list}

For each paraphrase, assign a quality score (0.0-1.0) based on these criteria:
- Meaning preservation (does it maintain the key information?)
- Rephrasing quality (how well it uses different wording?)
- Fluency and clarity (is it well-written?)
- Conciseness (is it appropriately concise?)

Return a JSON assessment with this structure:
```json
{{
  "assessments": [
    {{"id": 1, "quality": 0.8, "strengths": ["preserves meaning", "concise"], "weaknesses": ["awkward phrasing"]}},
    {{"id": 2, "quality": 0.9, "strengths": ["excellent rephrasing", "maintains tone"], "weaknesses": []}},
    {{"id": 3, "quality": 0.5, "strengths": ["concise"], "weaknesses": ["loses key information", "unclear"]}}
  ]
}}
```

Only include high-quality paraphrases that achieve at least 0.6 quality score.
"""
    return prompt


def get_custom_paraphrase_prompt(text: str, focus: str, style: str = "standard") -> str:
    """
    Generate a custom paraphrase prompt with specific focus and style.
    
    Args:
        text: Text to paraphrase
        focus: Specific aspect to focus on
        style: Paraphrasing style
        
    Returns:
        Custom paraphrase prompt
    """
    # Style-specific instructions
    style_instructions = {
        "standard": "Use natural, fluent language with standard wording.",
        "simplified": "Use simpler vocabulary and shorter sentences.",
        "formal": "Use more formal, academic language and structure.",
        "creative": "Use more expressive and varied language.",
        "technical": "Emphasize technical precision and domain-specific terms."
    }
    
    instructions = style_instructions.get(style, style_instructions["standard"])
    
    prompt = f"""Generate 3-5 paraphrases of this text, focusing specifically on {focus}.
{instructions}

Original text:
"{text}"

Provide your paraphrases in this JSON format:
```json
[
  {{"text": "First paraphrase", "quality": 0.9, "notes": "Brief note on changes"}},
  {{"text": "Second paraphrase", "quality": 0.8, "notes": "Brief note on changes"}},
  {{"text": "Third paraphrase", "quality": 0.7, "notes": "Brief note on changes"}}
]
```
"""
    return prompt 