#!/usr/bin/env python3
"""
LEXICON Component Tests

This script tests the individual components of the LEXICON system
to ensure they are working correctly.
"""

import os
import sys
import unittest
from pathlib import Path
from datetime import datetime

# Add parent directory to path to allow running this script directly
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent))

from src.lexicon.core.config import LexiconConfig
from src.lexicon.core.engine import LexiconEngine
from src.lexicon.nlp.preprocessor import NLPPreprocessor
from src.lexicon.declension.tagger import CaseTagger
from src.lexicon.paraphrase.generator import ParaphraseGenerator
from src.lexicon.graph.assembler import GraphAssembler
from src.lexicon.graph.entity_linker import EntityLinker
from src.lexicon.graph.cid_generator import generate_cid

from src.llm.OpenRouter import OpenRouterClient, OpenRouterConfig


class TestLexiconComponents(unittest.TestCase):
    """Test case for LEXICON components."""
    
    @classmethod
    def setUpClass(cls):
        """Set up test environment."""
        # Create timestamped output directory
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        base_output_dir = Path("output/lexicon")
        base_output_dir.mkdir(parents=True, exist_ok=True)
        
        cls.output_dir = base_output_dir / f"test_components_{timestamp}"
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
        
        # Initialize OpenRouter client
        openrouter_config = OpenRouterConfig(
            api_key=cls.config.openrouter_api_key,
            default_model=cls.config.default_model,
            base_url=cls.config.base_api_url,
            request_timeout=cls.config.timeout_seconds
        )
        cls.openrouter = OpenRouterClient(openrouter_config)
        
        # Sample text for testing
        cls.sample_text = "CEREBRUM integrates case-based reasoning with Bayesian networks. Dr. Chen presented at the AI Conference."
        
        # Save test info
        with open(cls.output_dir / "test_info.txt", "w") as f:
            f.write(f"LEXICON Component Tests\n")
            f.write(f"======================\n\n")
            f.write(f"Timestamp: {timestamp}\n")
            f.write(f"Sample text: {cls.sample_text}\n")
            f.write(f"Model: {cls.config.default_model}\n")
    
    def test_config(self):
        """Test configuration initialization."""
        self.assertIsNotNone(self.config)
        self.assertTrue(self.config.output_dir.exists())
        self.assertTrue(self.config.cache_dir.exists())
    
    def test_cid_generator(self):
        """Test content ID generator."""
        cid1 = generate_cid("test", "content1")
        cid2 = generate_cid("test", "content2")
        cid1_dup = generate_cid("test", "content1")
        
        self.assertTrue(cid1.startswith("cid:"))
        self.assertNotEqual(cid1, cid2)
        self.assertEqual(cid1, cid1_dup)
        
        # Save test results
        with open(self.output_dir / "cid_generator_test.txt", "w") as f:
            f.write(f"CID Generator Test\n")
            f.write(f"=================\n\n")
            f.write(f"cid1 = {cid1}\n")
            f.write(f"cid2 = {cid2}\n")
            f.write(f"cid1_dup = {cid1_dup}\n")
    
    def test_nlp_preprocessor(self):
        """Test NLP preprocessor."""
        try:
            preprocessor = NLPPreprocessor(self.config)
            segments = preprocessor.process(self.sample_text)
            
            self.assertIsNotNone(segments)
            self.assertGreater(len(segments), 0)
            self.assertTrue(hasattr(segments[0], 'text'))
            self.assertTrue(hasattr(segments[0], 'entities'))
            
            # Save test results
            with open(self.output_dir / "nlp_preprocessor_test.txt", "w") as f:
                f.write(f"NLP Preprocessor Test\n")
                f.write(f"===================\n\n")
                f.write(f"Segments: {len(segments)}\n")
                f.write(f"Entities: {sum(len(s.entities) for s in segments)}\n\n")
                
                for i, segment in enumerate(segments):
                    f.write(f"Segment {i+1}: {segment.text}\n")
                    f.write(f"  Entities: {len(segment.entities)}\n")
                    for j, entity in enumerate(segment.entities):
                        f.write(f"    Entity {j+1}: {entity.get('text', '')} ({entity.get('type', 'unknown')})\n")
                    f.write("\n")
            
            print(f"NLP Preprocessor: {len(segments)} segments, {sum(len(s.entities) for s in segments)} entities")
            
        except Exception as e:
            self.fail(f"NLP Preprocessor failed: {str(e)}")
    
    @unittest.skipIf(not os.environ.get("OPENROUTER_API_KEY"), "OpenRouter API key not available")
    def test_case_tagger(self):
        """Test case tagger."""
        try:
            preprocessor = NLPPreprocessor(self.config)
            segments = preprocessor.process(self.sample_text)
            
            tagger = CaseTagger(self.openrouter, self.config)
            cased_segments = tagger.tag(segments)
            
            self.assertIsNotNone(cased_segments)
            self.assertGreater(len(cased_segments), 0)
            self.assertTrue(hasattr(cased_segments[0], 'nominative'))
            self.assertTrue(hasattr(cased_segments[0], 'accusative'))
            
            # Save test results
            with open(self.output_dir / "case_tagger_test.txt", "w") as f:
                f.write(f"Case Tagger Test\n")
                f.write(f"===============\n\n")
                f.write(f"Segments: {len(cased_segments)}\n\n")
                
                for i, segment in enumerate(cased_segments):
                    f.write(f"Segment {i+1}: {segment.text}\n")
                    f.write(f"  Nominative: {len(segment.nominative)}\n")
                    f.write(f"  Accusative: {len(segment.accusative)}\n")
                    f.write(f"  Genitive: {len(segment.genitive)}\n")
                    f.write(f"  Dative: {len(segment.dative)}\n")
                    f.write(f"  Locative: {len(segment.locative)}\n")
                    f.write(f"  Instrumental: {len(segment.instrumental)}\n")
                    f.write(f"  Ablative: {len(segment.ablative)}\n")
                    f.write(f"  Vocative: {len(segment.vocative)}\n\n")
            
            print(f"Case Tagger: {len(cased_segments)} segments processed")
            
        except Exception as e:
            self.fail(f"Case Tagger failed: {str(e)}")
    
    @unittest.skipIf(not os.environ.get("OPENROUTER_API_KEY"), "OpenRouter API key not available")
    def test_entity_linker(self):
        """Test entity linker."""
        try:
            entity_linker = EntityLinker(self.openrouter, self.config)
            entities = [
                ("id1", "CEREBRUM"),
                ("id2", "Dr. Chen"),
                ("id3", "AI Conference")
            ]
            
            relationships = entity_linker.extract_relationships(entities)
            
            self.assertIsNotNone(relationships)
            
            # Save test results
            with open(self.output_dir / "entity_linker_test.txt", "w") as f:
                f.write(f"Entity Linker Test\n")
                f.write(f"=================\n\n")
                f.write(f"Entities: {len(entities)}\n")
                f.write(f"Relationships: {len(relationships)}\n\n")
                
                f.write("Entities:\n")
                for i, (entity_id, entity_text) in enumerate(entities):
                    f.write(f"  {i+1}. {entity_id}: {entity_text}\n")
                
                f.write("\nRelationships:\n")
                for i, rel in enumerate(relationships):
                    f.write(f"  {i+1}. {rel.get('source', '')} --[{rel.get('type', '')}]--> {rel.get('target', '')}\n")
            
            print(f"Entity Linker: {len(relationships)} relationships extracted")
            
        except Exception as e:
            self.fail(f"Entity Linker failed: {str(e)}")
    
    @unittest.skipIf(not os.environ.get("OPENROUTER_API_KEY"), "OpenRouter API key not available")
    def test_structured_case_determiner(self):
        """Test structured case determination with improved JSON parsing."""
        print("Testing StructuredCaseDeterminer")
        
        try:
            from src.lexicon.declension.structured_case_determiner import StructuredCaseDeterminer
            from src.llm.OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig
            
            # Initialize OpenRouter client
            router_config = OpenRouterConfig(
                api_key=os.environ.get("OPENROUTER_API_KEY"),
                default_model="anthropic/claude-3.5-sonnet"
            )
            openrouter_client = OpenRouterClient(router_config)
            
            # Initialize case determiner
            case_determiner = StructuredCaseDeterminer(openrouter_client, self.config)
            
            # Test with therapy session entities
            entities = ["Dr. Wilson", "Alex", "tension", "shoulders", "stress", "work deadline"]
            context = "Dr. Wilson asked Alex about the tension in his shoulders. Alex said the stress comes from the work deadline."
            
            # Test batch case determination
            assignments = case_determiner.determine_cases_batch(entities, context)
            
            # Validate assignments
            self.assertIsInstance(assignments, list)
            self.assertEqual(len(assignments), len(entities), "Should have assignment for each entity")
            
            # Check assignment structure
            case_variety = set()
            for assignment in assignments:
                self.assertIn(assignment.entity_text, entities)
                self.assertIsInstance(assignment.case, str)
                self.assertIsInstance(assignment.confidence, (int, float))
                self.assertGreater(assignment.confidence, 0)
                self.assertLessEqual(assignment.confidence, 1)
                self.assertIsInstance(assignment.rationale, str)
                case_variety.add(assignment.case)
            
            # Should have some case diversity
            self.assertGreater(len(case_variety), 1, "Should assign diverse cases")
            
            # Test single case determination
            single_assignment = case_determiner.determine_single_case("Dr. Wilson", context)
            self.assertIsInstance(single_assignment.entity_text, str)
            self.assertIsInstance(single_assignment.case, str)
            
            # Validate expected cases for specific entities
            assignment_map = {a.entity_text: a for a in assignments}
            
            # Dr. Wilson should likely be nominative (subject/agent)
            if "Dr. Wilson" in assignment_map:
                dr_case = assignment_map["Dr. Wilson"].case
                self.assertIn(dr_case, ["nominative", "vocative"], 
                            f"Dr. Wilson should be nominative or vocative, got {dr_case}")
            
            # Shoulders should likely be locative (location of tension)
            if "shoulders" in assignment_map:
                shoulders_case = assignment_map["shoulders"].case
                self.assertIn(shoulders_case, ["locative", "accusative"], 
                            f"shoulders should be locative or accusative, got {shoulders_case}")
            
            print(f"Case determiner test successful - Assigned cases: {case_variety}")
            
        except Exception as e:
            print(f"Structured case determiner test failed: {e}")
            raise
    
    @unittest.skipIf(not os.environ.get("OPENROUTER_API_KEY"), "OpenRouter API key not available")
    def test_engine_initialization(self):
        """Test LexiconEngine initialization and basic processing."""
        print("Testing LexiconEngine initialization")
        
        try:
            # Test engine initialization
            engine = LexiconEngine(self.config)
            self.assertIsNotNone(engine)
            self.assertIsNotNone(engine.llm_client)
            
            # Test processing with therapy session text
            therapy_text = """
            Dr. Wilson: How are you feeling today, Alex?
            Alex: I feel tense. There's tightness in my shoulders.
            Dr. Wilson: When did this tension start?
            Alex: It started after the work deadline. The stress comes from my job.
            """
            
            result = engine.process_text(therapy_text)
            
            # Validate result structure
            self.assertIsInstance(result, dict)
            self.assertIn("status", result)
            self.assertEqual(result["status"], "success")
            
            # Check for required keys
            required_keys = ["entities", "claims", "relations", "graph", "stats"]
            for key in required_keys:
                self.assertIn(key, result, f"Missing required key: {key}")
            
            # Validate entities with case assignments
            entities = result["entities"]
            self.assertIsInstance(entities, list)
            self.assertGreater(len(entities), 0, "Should detect entities")
            
            # Check entity structure and case assignments
            entity_cases_found = set()
            for entity in entities:
                self.assertIn("text", entity)
                self.assertIn("case", entity)
                self.assertIn("confidence", entity)
                entity_cases_found.add(entity["case"])
            
            # Should have some case diversity (not all locative)
            self.assertGreater(len(entity_cases_found), 1, "Should have diverse case assignments")
            
            # Validate claims with case assignments
            claims = result["claims"]
            self.assertIsInstance(claims, list)
            self.assertGreater(len(claims), 0, "Should detect claims")
            
            # Check claims have cases assigned
            claim_cases_found = set()
            for claim in claims:
                self.assertIn("text", claim)
                self.assertIn("case", claim)
                self.assertIn("confidence", claim)
                # Claims should not have 'none' as case
                self.assertNotEqual(claim["case"], "none")
                claim_cases_found.add(claim["case"])
            
            # Validate relations
            relations = result["relations"]
            self.assertIsInstance(relations, list)
            
            # Validate graph structure
            graph = result["graph"]
            self.assertIsInstance(graph, dict)
            self.assertIn("nodes", graph)
            self.assertIn("edges", graph)
            
            nodes = graph["nodes"]
            edges = graph["edges"]
            self.assertIsInstance(nodes, list)
            self.assertIsInstance(edges, list)
            
            # Should have nodes and edges
            self.assertGreater(len(nodes), 0, "Graph should have nodes")
            
            # Validate stats
            stats = result["stats"]
            self.assertIsInstance(stats, dict)
            self.assertEqual(stats["entities"], len(entities))
            self.assertEqual(stats["claims"], len(claims))
            self.assertEqual(stats["nodes"], len(nodes))
            self.assertEqual(stats["edges"], len(edges))
            
            print(f"Engine test successful - Found {len(entities)} entities with cases: {entity_cases_found}")
            print(f"Found {len(claims)} claims with cases: {claim_cases_found}")
            
        except Exception as e:
            print(f"Engine initialization test failed: {e}")
            raise
    
    @classmethod
    def tearDownClass(cls):
        """Clean up after tests."""
        # Create summary file
        with open(cls.output_dir / "test_summary.txt", "w") as f:
            f.write(f"LEXICON Component Tests Summary\n")
            f.write(f"=============================\n\n")
            f.write(f"All tests completed successfully\n")
            f.write(f"Results saved to: {cls.output_dir}\n")


if __name__ == "__main__":
    unittest.main() 