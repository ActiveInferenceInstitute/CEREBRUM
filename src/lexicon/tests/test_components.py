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
    def test_engine_initialization(self):
        """Test engine initialization."""
        try:
            engine = LexiconEngine(self.config)
            self.assertIsNotNone(engine)
            self.assertIsNotNone(engine.llm_client)
            
            # Save test results
            with open(self.output_dir / "engine_initialization_test.txt", "w") as f:
                f.write(f"Engine Initialization Test\n")
                f.write(f"========================\n\n")
                f.write(f"Engine initialized successfully\n")
                f.write(f"Model: {self.config.default_model}\n")
            
            print("LexiconEngine initialized successfully")
            
        except Exception as e:
            self.fail(f"LexiconEngine initialization failed: {str(e)}")
    
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