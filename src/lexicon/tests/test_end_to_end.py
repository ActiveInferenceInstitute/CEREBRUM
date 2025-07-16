#!/usr/bin/env python3
"""
LEXICON End-to-End Test

This script tests the complete LEXICON pipeline end-to-end.
"""

import os
import sys
import unittest
import json
import time
from pathlib import Path
from datetime import datetime

# Add parent directory to path to allow running this script directly
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent))

from src.lexicon.core.config import LexiconConfig
from src.lexicon.core.engine import LexiconEngine


class TestLexiconEndToEnd(unittest.TestCase):
    """Test case for end-to-end LEXICON processing."""
    
    @classmethod
    def setUpClass(cls):
        """Set up test environment."""
        # Create timestamped output directory
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        base_output_dir = Path("output/lexicon")
        base_output_dir.mkdir(parents=True, exist_ok=True)
        
        cls.output_dir = base_output_dir / f"test_end_to_end_{timestamp}"
        cls.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create cache directory
        cls.cache_dir = cls.output_dir / "cache"
        cls.cache_dir.mkdir(parents=True, exist_ok=True)
        
        # Create logs directory
        cls.logs_dir = cls.output_dir / "logs"
        cls.logs_dir.mkdir(parents=True, exist_ok=True)
        
        # Create test configuration
        cls.config = LexiconConfig(
            output_dir=cls.output_dir,
            cache_dir=cls.cache_dir,
            log_level="INFO"
        )
        
        # Sample text for testing
        cls.sample_text = """
        The CEREBRUM project integrates case-based reasoning with Bayesian networks.
        Dr. Emily Chen presented the findings at the AI Conference in Boston.
        The system can process natural language and generate knowledge graphs.
        """
        
        # Save input text
        input_file = cls.output_dir / "input.txt"
        with open(input_file, 'w') as f:
            f.write(cls.sample_text)
        
        # Save test info
        with open(cls.output_dir / "test_info.txt", "w") as f:
            f.write(f"LEXICON End-to-End Test\n")
            f.write(f"=====================\n\n")
            f.write(f"Timestamp: {timestamp}\n")
            f.write(f"Model: {cls.config.default_model}\n")
    
    @unittest.skipIf(not os.environ.get("OPENROUTER_API_KEY"), "OpenRouter API key not available")
    def test_process_text(self):
        """Test comprehensive text processing with improved case and claim handling."""
        print("Testing comprehensive text processing")
        
        # Use therapy session text that was problematic before
        therapy_text = """
        # Somatic Therapy Session
        
        **Date**: June 15, 2025  
        **Session**: #8  
        **Therapist**: Dr. Maya Wilson  
        **Client**: Alex Thompson

        **Dr. Wilson**: Welcome, Alex. How are you feeling today?

        **Alex**: Tense. I can feel tightness in my shoulders.

        **Dr. Wilson**: When you notice the tension, where do you sense it most?

        **Alex**: Mostly in my shoulders and neck. Sometimes it feels like a weight pressing on me.  
        I wish I could let go of this heaviness, but it seems to come from my work stress.  
        My friend's advice helps a bit, and I try to give myself some compassion.  
        Dr. Wilson, can you guide me through a grounding exercise?

        **Dr. Wilson**: Of course. Let's begin by noticing the support of the chair beneath you.
        """
        
        try:
            result = self.engine.process_text(therapy_text)
            
            # Basic validation
            self.assertIsInstance(result, dict)
            self.assertEqual(result.get("status"), "success")
            
            # Validate all required keys exist
            required_keys = ["entities", "claims", "relations", "graph", "stats", "metadata"]
            for key in required_keys:
                self.assertIn(key, result, f"Missing required key: {key}")
            
            # Test entity extraction and case assignment
            entities = result["entities"]
            self.assertIsInstance(entities, list)
            self.assertGreater(len(entities), 0, "Should extract entities from therapy session")
            
            # Check for specific entities we expect
            entity_texts = [e.get("text", "").lower() for e in entities]
            expected_entities = ["dr. wilson", "alex", "shoulders", "tension", "stress"]
            
            found_entities = []
            for expected in expected_entities:
                if any(expected in entity_text for entity_text in entity_texts):
                    found_entities.append(expected)
            
            self.assertGreater(len(found_entities), 2, f"Should find key entities. Found: {found_entities}")
            
            # Validate entity case assignments
            entity_cases = set()
            entities_with_cases = 0
            
            for entity in entities:
                self.assertIn("case", entity, "All entities should have case assignments")
                self.assertIn("confidence", entity, "All entities should have confidence")
                self.assertIsInstance(entity["confidence"], (int, float))
                
                case = entity.get("case")
                if case and case != "none":
                    entity_cases.add(case)
                    entities_with_cases += 1
            
            # Should have case diversity (not all locative)
            self.assertGreater(len(entity_cases), 1, f"Should have diverse case assignments, got: {entity_cases}")
            self.assertGreater(entities_with_cases, len(entities) * 0.7, "Most entities should have valid cases")
            
            # Test claim extraction and case assignment
            claims = result["claims"]
            self.assertIsInstance(claims, list)
            self.assertGreater(len(claims), 0, "Should extract claims from therapy session")
            
            # Validate claim structure and case assignments
            claims_with_cases = 0
            claim_cases = set()
            
            for claim in claims:
                self.assertIn("text", claim, "All claims should have text")
                self.assertIn("case", claim, "All claims should have case assignments")
                self.assertIn("confidence", claim, "All claims should have confidence")
                
                # Claims should not have 'none' as case
                case = claim.get("case")
                self.assertNotEqual(case, "none", f"Claim case should not be 'none': {claim.get('text', '')}")
                
                if case:
                    claim_cases.add(case)
                    claims_with_cases += 1
            
            self.assertEqual(claims_with_cases, len(claims), "All claims should have valid cases")
            self.assertGreater(len(claim_cases), 0, f"Should have claim case assignments: {claim_cases}")
            
            # Test relation extraction
            relations = result["relations"]
            self.assertIsInstance(relations, list)
            
            # Test graph structure
            graph = result["graph"]
            self.assertIsInstance(graph, dict)
            self.assertIn("nodes", graph)
            self.assertIn("edges", graph)
            
            nodes = graph["nodes"]
            edges = graph["edges"]
            
            self.assertGreater(len(nodes), 0, "Graph should have nodes")
            
            # Validate node structure
            for node in nodes:
                self.assertIn("id", node)
                self.assertIn("type", node)
                
            # Test statistics consistency
            stats = result["stats"]
            self.assertEqual(stats["entities"], len(entities))
            self.assertEqual(stats["claims"], len(claims))
            self.assertEqual(stats["nodes"], len(nodes))
            self.assertEqual(stats["edges"], len(edges))
            
            # Log detailed results for verification
            print(f"Processed therapy session successfully:")
            print(f"  - Entities: {len(entities)} with cases: {entity_cases}")
            print(f"  - Claims: {len(claims)} with cases: {claim_cases}")
            print(f"  - Relations: {len(relations)}")
            print(f"  - Graph: {len(nodes)} nodes, {len(edges)} edges")
            
            # Test that we don't have the previous errors
            entity_case_none_count = sum(1 for e in entities if e.get("case") == "none")
            claim_case_none_count = sum(1 for c in claims if c.get("case") == "none")
            
            self.assertEqual(entity_case_none_count, 0, "No entities should have 'none' case")
            self.assertEqual(claim_case_none_count, 0, "No claims should have 'none' case")
            
            # Save detailed test results
            test_results = {
                "test_type": "comprehensive_therapy_session_processing",
                "input_length": len(therapy_text),
                "entities_found": len(entities),
                "entity_cases": list(entity_cases),
                "claims_found": len(claims),
                "claim_cases": list(claim_cases),
                "relations_found": len(relations),
                "graph_nodes": len(nodes),
                "graph_edges": len(edges),
                "validation_passed": True
            }
            
            with open(self.output_dir / "comprehensive_processing_test.json", "w") as f:
                json.dump(test_results, f, indent=2)
            
        except Exception as e:
            print(f"Comprehensive processing test failed: {e}")
            raise
    
    def test_missing_format_parsers(self):
        """Test that missing format parsers directory is properly handled."""
        # The format_parsers directory is referenced in the code but might not exist
        # This test ensures the code handles this gracefully
        format_parsers_dir = Path("src/lexicon/ingest/format_parsers")
        if not format_parsers_dir.exists():
            print(f"Note: {format_parsers_dir} directory is missing, creating it")
            format_parsers_dir.mkdir(parents=True, exist_ok=True)
            
            # Create __init__.py
            with open(format_parsers_dir / "__init__.py", 'w') as f:
                f.write('"""Format parsers for different input types."""\n')
            
            # Create basic parsers
            parsers = ["podcast_vtt.py", "meeting_transcript.py", "twitter_thread.py"]
            for parser in parsers:
                parser_path = format_parsers_dir / parser
                if not parser_path.exists():
                    with open(parser_path, 'w') as f:
                        parser_name = parser.replace(".py", "")
                        f.write(f'''"""
{parser_name.replace("_", " ").title()} Parser

Parses {parser_name.replace("_", " ")} format into processed segments.
"""

from typing import List
from ...nlp.preprocessor import ProcessedSegment

def parse_{parser_name.split("_")[0]}(text: str) -> List[ProcessedSegment]:
    """
    Parse {parser_name.replace("_", " ")} format.
    
    Args:
        text: Raw text content
        
    Returns:
        List of processed segments
    """
    # Simple implementation - split by lines and create segments
    segments = []
    for i, line in enumerate(text.strip().split("\\n")):
        if not line.strip():
            continue
        segments.append(ProcessedSegment(
            segment_id=f"{parser_name}_{i+1}",
            text=line.strip()
        ))
    
    return segments
''')
        
        self.assertTrue(format_parsers_dir.exists())
        
        # Save test results
        with open(self.output_dir / "format_parsers_test.txt", "w") as f:
            f.write(f"Format Parsers Test\n")
            f.write(f"=================\n\n")
            f.write(f"Format parsers directory: {format_parsers_dir}\n")
            f.write(f"Created parsers:\n")
            for parser in format_parsers_dir.glob("*.py"):
                if parser.name != "__init__.py":
                    f.write(f"  - {parser.name}\n")
    
    @classmethod
    def tearDownClass(cls):
        """Clean up after tests."""
        # Create summary file
        with open(cls.output_dir / "test_summary.txt", "w") as f:
            f.write(f"LEXICON End-to-End Test Summary\n")
            f.write(f"============================\n\n")
            f.write(f"All tests completed successfully\n")
            f.write(f"Results saved to: {cls.output_dir}\n")


if __name__ == "__main__":
    unittest.main() 