"""
LEXICON OpenRouter Client for Case Declension

Provides prompt engineering and response parsing for OpenRouter LLM case declension.
"""

import re
import json
from typing import Dict, List, Any, Optional

def declension_prompt(text: str) -> str:
    """
    Generate a prompt for the LLM to perform case declension.
    
    Args:
        text: Text to analyze
        
    Returns:
        Structured prompt for case declension
    """
    prompt = f"""Analyze the following text and identify entities that fulfill different grammatical cases according to CEREBRUM's 8-case system. For each case, extract the relevant text spans.

Text to analyze:
"{text}"

The 8 cases are:
1. NOMINATIVE (NOM): Agents/subjects that perform actions (Who/what is performing the action?)
2. ACCUSATIVE (ACC): Direct objects of actions (Who/what is receiving the action?)
3. GENITIVE (GEN): Possessive relationships or sources (Whose? From what source?)
4. DATIVE (DAT): Recipients or beneficiaries (To/for whom? For whose benefit?)
5. LOCATIVE (LOC): Locations or contextual settings (Where? In what context?)
6. INSTRUMENTAL (INS): Tools, means, or instruments (With/by what means?)
7. ABLATIVE (ABL): Origins or causes (From where? Because of what?)
8. VOCATIVE (VOC): Direct address or summons (Direct address to someone)

For each case, extract relevant text spans from the input. Include the span, its case, and your confidence level (0.0-1.0).

Format your response as JSON with this structure:
{{
  "nominative": [
    {{"text": "entity text", "confidence": 0.9}}
  ],
  "accusative": [
    {{"text": "entity text", "confidence": 0.8}}
  ],
  ... (and so on for all 8 cases)
}}

Only include entities that clearly belong to a specific case. If no entities exist for a case, provide an empty list.
"""
    return prompt


def extract_declensions(response: str) -> Dict[str, List[Dict[str, Any]]]:
    """
    Extract case declensions from OpenRouter response.
    
    Args:
        response: Raw response from OpenRouter
        
    Returns:
        Dictionary with case assignments
    """
    # Initialize default structure
    declensions = {
        "nominative": [],
        "accusative": [],
        "genitive": [],
        "dative": [],
        "locative": [],
        "instrumental": [],
        "ablative": [],
        "vocative": []
    }
    
    try:
        # Extract JSON from response
        json_match = re.search(r'```json\s*(.*?)\s*```', response, re.DOTALL)
        if json_match:
            json_str = json_match.group(1)
        else:
            # Try to find JSON without code blocks
            json_match = re.search(r'(\{\s*"nominative"\s*:.*\})', response, re.DOTALL)
            if json_match:
                json_str = json_match.group(1)
            else:
                # Fallback to whole response if it looks like JSON
                if response.strip().startswith('{') and response.strip().endswith('}'):
                    json_str = response
                else:
                    return declensions  # Return empty declensions
        
        # Parse JSON
        parsed = json.loads(json_str)
        
        # Process each case
        for case in declensions.keys():
            if case in parsed:
                for item in parsed[case]:
                    if isinstance(item, dict) and "text" in item:
                        # Add source information
                        if "confidence" not in item:
                            item["confidence"] = 0.7  # Default confidence
                        item["source"] = "llm"
                        declensions[case].append(item)
        
        return declensions
        
    except Exception as e:
        print(f"Error parsing declensions: {str(e)}")
        return declensions


def generate_clarification_prompt(text: str, ambiguous_entity: str) -> str:
    """
    Generate a prompt to clarify ambiguous case assignment.
    
    Args:
        text: Original text context
        ambiguous_entity: Entity with ambiguous case
        
    Returns:
        Prompt for case clarification
    """
    return f"""In the following text:
"{text}"

The entity "{ambiguous_entity}" could belong to multiple grammatical cases. 

Carefully analyze the role of "{ambiguous_entity}" in the text and determine its primary case from CEREBRUM's 8-case system:
- NOMINATIVE (NOM): Agent/subject that performs an action
- ACCUSATIVE (ACC): Direct object receiving an action
- GENITIVE (GEN): Possessive relationship or source
- DATIVE (DAT): Recipient or beneficiary
- LOCATIVE (LOC): Location or contextual setting
- INSTRUMENTAL (INS): Tool, means, or instrument
- ABLATIVE (ABL): Origin or cause
- VOCATIVE (VOC): Direct address or summons

Respond with a single JSON object containing:
1. The primary case
2. Your confidence level (0.0-1.0)
3. A brief explanation

Format:
{{"case": "CASE_NAME", "confidence": 0.9, "explanation": "Brief explanation"}}
"""


def extract_clarification(response: str) -> Dict[str, Any]:
    """
    Extract case clarification from LLM response.
    
    Args:
        response: Raw response from OpenRouter
        
    Returns:
        Dictionary with case clarification
    """
    try:
        # Extract JSON from response
        json_match = re.search(r'```json\s*(.*?)\s*```', response, re.DOTALL)
        if json_match:
            json_str = json_match.group(1)
        else:
            # Try to find JSON without code blocks
            json_match = re.search(r'(\{\s*"case"\s*:.*\})', response, re.DOTALL)
            if json_match:
                json_str = json_match.group(1)
            else:
                # Fallback to whole response if it looks like JSON
                if response.strip().startswith('{') and response.strip().endswith('}'):
                    json_str = response
                else:
                    return {"case": "unknown", "confidence": 0.0}
        
        # Parse JSON
        return json.loads(json_str)
        
    except Exception:
        # Try to extract case directly from text
        case_match = re.search(r'case["\s:]+(NOM|ACC|GEN|DAT|LOC|INS|ABL|VOC|NOMINATIVE|ACCUSATIVE|GENITIVE|DATIVE|LOCATIVE|INSTRUMENTAL|ABLATIVE|VOCATIVE)', 
                              response, re.IGNORECASE)
        if case_match:
            case = case_match.group(1).upper()
            # Map abbreviated forms to full names
            case_map = {
                "NOM": "nominative", "ACC": "accusative", "GEN": "genitive", 
                "DAT": "dative", "LOC": "locative", "INS": "instrumental",
                "ABL": "ablative", "VOC": "vocative"
            }
            case = case_map.get(case, case.lower())
            return {"case": case, "confidence": 0.6}
            
        return {"case": "unknown", "confidence": 0.0} 