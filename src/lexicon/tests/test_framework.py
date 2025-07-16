"""
LEXICON Comprehensive Testing Framework

Advanced testing system for all LEXICON components:
- Unit tests for individual components
- Integration tests for pipeline workflows
- Performance benchmarks
- Quality assurance checks
- Regression testing
- Mock data generation
"""

import unittest
import pytest
import time
import tempfile
import json
import logging
from typing import List, Dict, Any, Optional, Callable, Union
from pathlib import Path
from dataclasses import dataclass
from unittest.mock import Mock, patch, MagicMock
import random
import string
from collections import defaultdict

# Import LEXICON components
from ..core.engine import LexiconEngine
from ..core.config import LexiconConfig
from ..core.logging import get_logger
from ..nlp.entity_deduplicator import EntityDeduplicator
from ..graph.semantic_relation_detector import SemanticRelationDetector
from ..declension.enhanced_case_analyzer import EnhancedCaseAnalyzer
from ..visualization.interactive_graph_visualizer import InteractiveGraphVisualizer
from ..core.performance_optimizer import PerformanceOptimizer


@dataclass
class TestResult:
    """Test execution result."""
    test_name: str
    component: str
    passed: bool
    execution_time: float
    memory_usage_mb: float
    error_message: Optional[str] = None
    warnings: List[str] = None
    metrics: Dict[str, Any] = None


@dataclass
class BenchmarkResult:
    """Performance benchmark result."""
    component: str
    test_scenario: str
    items_processed: int
    execution_time: float
    throughput: float  # items per second
    memory_peak_mb: float
    cpu_usage_percent: float


class MockDataGenerator:
    """Generate realistic mock data for testing."""
    
    def __init__(self, seed: int = 42):
        """Initialize with random seed for reproducibility."""
        random.seed(seed)
        self.logger = get_logger("tests.mock_data_generator")
        
        # Therapeutic conversation templates
        self.therapeutic_templates = [
            "I feel {emotion} in my {body_part}",
            "The {stress_source} is causing {symptom}",
            "When I think about {trigger}, I notice {physical_response}",
            "Dr. {therapist_name} suggested that {advice}",
            "My {relationship} reminds me of {past_experience}",
        ]
        
        # Scientific text templates
        self.scientific_templates = [
            "The study of {n} participants showed {finding}",
            "Data analysis revealed {pattern} in {domain}",
            "The {method} approach yielded {result}",
            "Hypothesis {number} predicted {outcome}",
            "Statistical analysis indicated {significance}",
        ]
        
        # Word banks
        self.emotions = ["anxious", "tense", "calm", "worried", "relaxed", "stressed"]
        self.body_parts = ["shoulders", "chest", "stomach", "back", "neck", "head"]
        self.stress_sources = ["work", "deadline", "boss", "family", "financial pressure"]
        self.symptoms = ["tension", "headaches", "insomnia", "fatigue", "restlessness"]
        self.therapist_names = ["Wilson", "Smith", "Johnson", "Brown", "Davis"]
        self.relationships = ["father", "mother", "partner", "colleague", "friend"]
        
    def generate_therapeutic_text(self, sentence_count: int = 5) -> str:
        """Generate realistic therapeutic conversation text."""
        sentences = []
        
        for _ in range(sentence_count):
            template = random.choice(self.therapeutic_templates)
            
            # Fill template with random choices
            filled = template.format(
                emotion=random.choice(self.emotions),
                body_part=random.choice(self.body_parts),
                stress_source=random.choice(self.stress_sources),
                symptom=random.choice(self.symptoms),
                therapist_name=random.choice(self.therapist_names),
                trigger=random.choice(self.stress_sources),
                physical_response=random.choice(self.symptoms),
                advice="focusing on breathing exercises",
                relationship=random.choice(self.relationships),
                past_experience="childhood experiences"
            )
            
            sentences.append(filled)
        
        return ". ".join(sentences) + "."
    
    def generate_scientific_text(self, sentence_count: int = 5) -> str:
        """Generate realistic scientific text."""
        sentences = []
        
        for _ in range(sentence_count):
            template = random.choice(self.scientific_templates)
            
            filled = template.format(
                n=random.randint(50, 500),
                finding="significant correlations",
                pattern="clear patterns",
                domain="cognitive behavior",
                method="experimental",
                result="promising outcomes",
                number=random.randint(1, 5),
                outcome="improved performance",
                significance="p < 0.05"
            )
            
            sentences.append(filled)
        
        return ". ".join(sentences) + "."
    
    def generate_entities(self, count: int = 10, domain: str = "therapeutic") -> List[Dict[str, Any]]:
        """Generate mock entities with realistic properties."""
        entities = []
        cases = ["nominative", "accusative", "genitive", "dative", "ablative", "locative"]
        
        if domain == "therapeutic":
            entity_texts = (self.emotions + self.body_parts + self.stress_sources + 
                          self.symptoms + ["therapy", "session", "client", "therapist"])
        else:
            entity_texts = ["study", "data", "hypothesis", "method", "result", "analysis",
                          "participants", "research", "experiment", "outcome"]
        
        for i in range(count):
            text = random.choice(entity_texts)
            case = random.choice(cases)
            
            entity = {
                "text": text,
                "case": case,
                "confidence": random.uniform(0.5, 0.95),
                "case_rationale": f"Detected as {case} due to contextual analysis",
                "metadata": {
                    "domain": domain,
                    "generated": True,
                    "source_sentence": f"Generated sentence {i}"
                }
            }
            
            entities.append(entity)
        
        return entities
    
    def generate_relations(self, entities: List[Dict], count: int = 5) -> List[Dict[str, Any]]:
        """Generate mock semantic relations between entities."""
        if len(entities) < 2:
            return []
        
        relations = []
        relation_types = [
            "causal_relationship", "temporal_relationship", "spatial_relationship",
            "similarity_relationship", "part_whole_relationship"
        ]
        
        for i in range(min(count, len(entities) * (len(entities) - 1) // 2)):
            source = random.choice(entities)
            target = random.choice([e for e in entities if e != source])
            
            relation = {
                "source_entity": source["text"],
                "target_entity": target["text"],
                "relation_type": random.choice(relation_types),
                "confidence": random.uniform(0.6, 0.9),
                "evidence": [f"Generated evidence for {source['text']} -> {target['text']}"],
                "context": f"Context showing relationship between {source['text']} and {target['text']}",
                "source_case": source["case"],
                "target_case": target["case"]
            }
            
            relations.append(relation)
        
        return relations


class ComponentTester:
    """Test individual LEXICON components."""
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """Initialize component tester."""
        self.config = config or LexiconConfig()
        self.logger = get_logger("tests.component_tester")
        self.mock_data = MockDataGenerator()
        self.results: List[TestResult] = []
    
    def test_entity_deduplicator(self) -> TestResult:
        """Test entity deduplication functionality."""
        start_time = time.time()
        start_memory = self._get_memory_usage()
        
        try:
            # Create test entities with duplicates
            entities = [
                {"text": "anxiety", "case": "nominative", "confidence": 0.8},
                {"text": "anxiety", "case": "nominative", "confidence": 0.7},  # Duplicate
                {"text": "stress", "case": "accusative", "confidence": 0.9},
                {"text": "tension", "case": "locative", "confidence": 0.6},
                {"text": "stress", "case": "accusative", "confidence": 0.85},  # Duplicate
            ]
            
            deduplicator = EntityDeduplicator(self.config)
            deduplicated = deduplicator.deduplicate_entities(entities)
            
            # Verify results
            assert len(deduplicated) < len(entities), "Deduplication should reduce entity count"
            assert len(deduplicated) == 3, f"Expected 3 unique entities, got {len(deduplicated)}"
            
            # Check that high-confidence entities are preserved
            anxiety_entities = [e for e in deduplicated if e["text"] == "anxiety"]
            assert len(anxiety_entities) == 1, "Should have one anxiety entity after deduplication"
            assert anxiety_entities[0]["confidence"] >= 0.8, "Should preserve higher confidence"
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            
            return TestResult(
                test_name="test_entity_deduplicator",
                component="EntityDeduplicator",
                passed=True,
                execution_time=execution_time,
                memory_usage_mb=memory_usage,
                metrics=deduplicator.get_deduplication_metrics()
            )
            
        except Exception as e:
            return TestResult(
                test_name="test_entity_deduplicator",
                component="EntityDeduplicator", 
                passed=False,
                execution_time=time.time() - start_time,
                memory_usage_mb=self._get_memory_usage() - start_memory,
                error_message=str(e)
            )
    
    def test_semantic_relation_detector(self) -> TestResult:
        """Test semantic relation detection."""
        start_time = time.time()
        start_memory = self._get_memory_usage()
        
        try:
            entities = self.mock_data.generate_entities(8, "therapeutic")
            text = self.mock_data.generate_therapeutic_text(5)
            
            detector = SemanticRelationDetector(self.config)
            relations = detector.detect_relations(entities, text)
            
            # Verify results
            assert isinstance(relations, list), "Should return list of relations"
            assert all(hasattr(r, 'source_entity') for r in relations), "Relations should have source_entity"
            assert all(hasattr(r, 'target_entity') for r in relations), "Relations should have target_entity"
            assert all(hasattr(r, 'confidence') for r in relations), "Relations should have confidence"
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            
            return TestResult(
                test_name="test_semantic_relation_detector",
                component="SemanticRelationDetector",
                passed=True,
                execution_time=execution_time,
                memory_usage_mb=memory_usage,
                metrics=detector.get_relation_metrics()
            )
            
        except Exception as e:
            return TestResult(
                test_name="test_semantic_relation_detector",
                component="SemanticRelationDetector",
                passed=False,
                execution_time=time.time() - start_time,
                memory_usage_mb=self._get_memory_usage() - start_memory,
                error_message=str(e)
            )
    
    def test_enhanced_case_analyzer(self) -> TestResult:
        """Test enhanced case analysis."""
        start_time = time.time()
        start_memory = self._get_memory_usage()
        
        try:
            analyzer = EnhancedCaseAnalyzer(self.config)
            
            # Test cases with known patterns
            test_cases = [
                {
                    "entity": {"text": "anxiety"},
                    "sentence": "I feel anxiety in my chest",
                    "expected_case": "accusative"  # Direct object of "feel"
                },
                {
                    "entity": {"text": "therapist"},
                    "sentence": "The therapist helps clients",
                    "expected_case": "nominative"  # Subject
                },
                {
                    "entity": {"text": "session"},
                    "sentence": "We talked about stress during the session",
                    "expected_case": "locative"  # Temporal location
                }
            ]
            
            correct_predictions = 0
            total_predictions = len(test_cases)
            
            for test_case in test_cases:
                analysis = analyzer.analyze_entity_case(
                    test_case["entity"],
                    self.mock_data.generate_therapeutic_text(),
                    test_case["sentence"],
                    ["therapeutic"]
                )
                
                # Check if prediction matches expected (allow some flexibility)
                if analysis.primary_case == test_case["expected_case"]:
                    correct_predictions += 1
                elif any(case == test_case["expected_case"] for case, _ in analysis.alternative_cases):
                    correct_predictions += 0.5  # Partial credit for alternative
            
            accuracy = correct_predictions / total_predictions
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            
            return TestResult(
                test_name="test_enhanced_case_analyzer",
                component="EnhancedCaseAnalyzer",
                passed=accuracy >= 0.6,  # 60% accuracy threshold
                execution_time=execution_time,
                memory_usage_mb=memory_usage,
                metrics={
                    "accuracy": accuracy,
                    "correct_predictions": correct_predictions,
                    "total_predictions": total_predictions,
                    **analyzer.get_analysis_metrics()
                },
                warnings=["Low accuracy"] if accuracy < 0.8 else None
            )
            
        except Exception as e:
            return TestResult(
                test_name="test_enhanced_case_analyzer",
                component="EnhancedCaseAnalyzer",
                passed=False,
                execution_time=time.time() - start_time,
                memory_usage_mb=self._get_memory_usage() - start_memory,
                error_message=str(e)
            )
    
    def test_performance_optimizer(self) -> TestResult:
        """Test performance optimization functionality."""
        start_time = time.time()
        start_memory = self._get_memory_usage()
        
        try:
            optimizer = PerformanceOptimizer(self.config)
            
            # Test caching
            def dummy_process(text):
                time.sleep(0.01)  # Simulate processing
                return f"processed_{text}"
            
            # First call - should not be cached
            key1 = optimizer.create_processing_key("test_text", {"param": "value"})
            result1 = optimizer.cached_process(key1, dummy_process, "test_text")
            
            # Second call - should be cached
            result2 = optimizer.cached_process(key1, dummy_process, "test_text")
            
            assert result1 == result2, "Cached results should be identical"
            assert optimizer.metrics.cache_hits > 0, "Should have cache hits"
            
            # Test batch processing
            items = list(range(50))
            
            def batch_func(batch):
                return [x * 2 for x in batch]
            
            results = optimizer.batch_processor.process_batches(items, batch_func, batch_size=10)
            expected = [x * 2 for x in items]
            
            assert results == expected, "Batch processing should produce correct results"
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            
            return TestResult(
                test_name="test_performance_optimizer",
                component="PerformanceOptimizer",
                passed=True,
                execution_time=execution_time,
                memory_usage_mb=memory_usage,
                metrics=optimizer.get_performance_report()
            )
            
        except Exception as e:
            return TestResult(
                test_name="test_performance_optimizer",
                component="PerformanceOptimizer",
                passed=False,
                execution_time=time.time() - start_time,
                memory_usage_mb=self._get_memory_usage() - start_memory,
                error_message=str(e)
            )
    
    def _get_memory_usage(self) -> float:
        """Get current memory usage in MB."""
        try:
            import psutil
            process = psutil.Process()
            return process.memory_info().rss / (1024 * 1024)
        except:
            return 0.0
    
    def run_all_component_tests(self) -> List[TestResult]:
        """Run all component tests."""
        self.logger.info("Running comprehensive component tests...")
        
        test_methods = [
            self.test_entity_deduplicator,
            self.test_semantic_relation_detector,
            self.test_enhanced_case_analyzer,
            self.test_performance_optimizer
        ]
        
        results = []
        for test_method in test_methods:
            try:
                result = test_method()
                results.append(result)
                
                status = "PASSED" if result.passed else "FAILED"
                self.logger.info(f"{result.test_name}: {status} "
                               f"({result.execution_time:.3f}s, {result.memory_usage_mb:.1f}MB)")
                
                if result.error_message:
                    self.logger.error(f"Error in {result.test_name}: {result.error_message}")
                
            except Exception as e:
                self.logger.error(f"Test {test_method.__name__} crashed: {e}")
                results.append(TestResult(
                    test_name=test_method.__name__,
                    component="Unknown",
                    passed=False,
                    execution_time=0.0,
                    memory_usage_mb=0.0,
                    error_message=str(e)
                ))
        
        self.results.extend(results)
        return results


class IntegrationTester:
    """Test integrated LEXICON workflows."""
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """Initialize integration tester."""
        self.config = config or LexiconConfig()
        self.logger = get_logger("tests.integration_tester")
        self.mock_data = MockDataGenerator()
    
    def test_full_pipeline(self) -> TestResult:
        """Test complete LEXICON pipeline."""
        start_time = time.time()
        start_memory = self._get_memory_usage()
        
        try:
            # Create test input
            text = self.mock_data.generate_therapeutic_text(10)
            
            # Initialize LEXICON engine
            engine = LexiconEngine(self.config)
            
            # Process text through full pipeline
            result = engine.process_text(text)
            
            # Verify pipeline outputs
            assert "entities" in result, "Result should contain entities"
            assert "relations" in result, "Result should contain relations"
            assert isinstance(result["entities"], list), "Entities should be a list"
            assert isinstance(result["relations"], list), "Relations should be a list"
            
            # Check entity properties
            for entity in result["entities"]:
                assert "text" in entity, "Entity should have text"
                assert "case" in entity, "Entity should have case assignment"
                assert "confidence" in entity, "Entity should have confidence"
            
            # Check relation properties
            for relation in result["relations"]:
                assert "source_entity" in relation, "Relation should have source"
                assert "target_entity" in relation, "Relation should have target"
                assert "relation_type" in relation, "Relation should have type"
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            
            return TestResult(
                test_name="test_full_pipeline",
                component="LexiconEngine",
                passed=True,
                execution_time=execution_time,
                memory_usage_mb=memory_usage,
                metrics={
                    "entities_extracted": len(result["entities"]),
                    "relations_detected": len(result["relations"]),
                    "text_length": len(text)
                }
            )
            
        except Exception as e:
            return TestResult(
                test_name="test_full_pipeline",
                component="LexiconEngine",
                passed=False,
                execution_time=time.time() - start_time,
                memory_usage_mb=self._get_memory_usage() - start_memory,
                error_message=str(e)
            )
    
    def test_visualization_integration(self) -> TestResult:
        """Test visualization integration."""
        start_time = time.time()
        start_memory = self._get_memory_usage()
        
        try:
            # Generate test data
            entities = self.mock_data.generate_entities(15, "therapeutic")
            relations = self.mock_data.generate_relations(entities, 8)
            
            # Test visualization creation
            visualizer = InteractiveGraphVisualizer(self.config)
            
            # Create visualization (this should not fail)
            fig = visualizer.create_interactive_visualization(
                entities, relations, "Test Visualization"
            )
            
            # Basic checks
            if fig is not None:  # Plotly available
                assert hasattr(fig, 'data'), "Figure should have data"
                assert hasattr(fig, 'layout'), "Figure should have layout"
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            
            return TestResult(
                test_name="test_visualization_integration",
                component="InteractiveGraphVisualizer",
                passed=True,
                execution_time=execution_time,
                memory_usage_mb=memory_usage,
                metrics=visualizer.get_visualization_metrics()
            )
            
        except Exception as e:
            return TestResult(
                test_name="test_visualization_integration",
                component="InteractiveGraphVisualizer",
                passed=False,
                execution_time=time.time() - start_time,
                memory_usage_mb=self._get_memory_usage() - start_memory,
                error_message=str(e)
            )
    
    def _get_memory_usage(self) -> float:
        """Get current memory usage in MB."""
        try:
            import psutil
            process = psutil.Process()
            return process.memory_info().rss / (1024 * 1024)
        except:
            return 0.0


class PerformanceBenchmarker:
    """Benchmark LEXICON performance."""
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """Initialize performance benchmarker."""
        self.config = config or LexiconConfig()
        self.logger = get_logger("tests.performance_benchmarker")
        self.mock_data = MockDataGenerator()
    
    def benchmark_entity_processing(self, entity_counts: List[int] = None) -> List[BenchmarkResult]:
        """Benchmark entity processing performance."""
        entity_counts = entity_counts or [10, 50, 100, 500, 1000]
        results = []
        
        for count in entity_counts:
            self.logger.info(f"Benchmarking entity processing with {count} entities")
            
            entities = self.mock_data.generate_entities(count, "therapeutic")
            
            start_time = time.time()
            start_memory = self._get_memory_usage()
            
            # Process entities through deduplication
            deduplicator = EntityDeduplicator(self.config)
            deduplicated = deduplicator.deduplicate_entities(entities)
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            throughput = count / execution_time if execution_time > 0 else 0
            
            result = BenchmarkResult(
                component="EntityDeduplicator",
                test_scenario=f"deduplication_{count}_entities",
                items_processed=count,
                execution_time=execution_time,
                throughput=throughput,
                memory_peak_mb=memory_usage,
                cpu_usage_percent=0.0  # Would need more sophisticated monitoring
            )
            
            results.append(result)
            
            self.logger.info(f"Processed {count} entities in {execution_time:.3f}s "
                           f"({throughput:.1f} entities/s)")
        
        return results
    
    def benchmark_relation_detection(self, text_lengths: List[int] = None) -> List[BenchmarkResult]:
        """Benchmark relation detection performance."""
        text_lengths = text_lengths or [100, 500, 1000, 2000]
        results = []
        
        for length in text_lengths:
            self.logger.info(f"Benchmarking relation detection with {length} characters")
            
            # Generate text of approximately the target length
            sentence_count = max(1, length // 100)
            text = self.mock_data.generate_therapeutic_text(sentence_count)
            entities = self.mock_data.generate_entities(10, "therapeutic")
            
            start_time = time.time()
            start_memory = self._get_memory_usage()
            
            detector = SemanticRelationDetector(self.config)
            relations = detector.detect_relations(entities, text)
            
            execution_time = time.time() - start_time
            memory_usage = self._get_memory_usage() - start_memory
            throughput = len(text) / execution_time if execution_time > 0 else 0
            
            result = BenchmarkResult(
                component="SemanticRelationDetector",
                test_scenario=f"relation_detection_{length}_chars",
                items_processed=len(text),
                execution_time=execution_time,
                throughput=throughput,
                memory_peak_mb=memory_usage,
                cpu_usage_percent=0.0
            )
            
            results.append(result)
        
        return results
    
    def _get_memory_usage(self) -> float:
        """Get current memory usage in MB."""
        try:
            import psutil
            process = psutil.Process()
            return process.memory_info().rss / (1024 * 1024)
        except:
            return 0.0


class TestSuite:
    """Comprehensive LEXICON test suite."""
    
    def __init__(self, config: Optional[LexiconConfig] = None, output_dir: Optional[Path] = None):
        """Initialize test suite."""
        self.config = config or LexiconConfig()
        self.output_dir = Path(output_dir) if output_dir else Path("test_results")
        self.output_dir.mkdir(exist_ok=True)
        
        self.logger = get_logger("tests.test_suite")
        
        # Initialize testers
        self.component_tester = ComponentTester(self.config)
        self.integration_tester = IntegrationTester(self.config)
        self.benchmarker = PerformanceBenchmarker(self.config)
        
        # Results storage
        self.all_results: List[TestResult] = []
        self.benchmark_results: List[BenchmarkResult] = []
    
    def run_comprehensive_tests(self, include_benchmarks: bool = True) -> Dict[str, Any]:
        """Run comprehensive test suite."""
        self.logger.info("Starting comprehensive LEXICON test suite...")
        
        start_time = time.time()
        
        # Run component tests
        self.logger.info("Running component tests...")
        component_results = self.component_tester.run_all_component_tests()
        self.all_results.extend(component_results)
        
        # Run integration tests
        self.logger.info("Running integration tests...")
        integration_results = [
            self.integration_tester.test_full_pipeline(),
            self.integration_tester.test_visualization_integration()
        ]
        self.all_results.extend(integration_results)
        
        # Run benchmarks if requested
        if include_benchmarks:
            self.logger.info("Running performance benchmarks...")
            self.benchmark_results.extend(self.benchmarker.benchmark_entity_processing())
            self.benchmark_results.extend(self.benchmarker.benchmark_relation_detection())
        
        total_time = time.time() - start_time
        
        # Generate report
        report = self.generate_test_report(total_time)
        
        # Save results
        self.save_results(report)
        
        self.logger.info(f"Test suite completed in {total_time:.2f}s")
        
        return report
    
    def generate_test_report(self, total_execution_time: float) -> Dict[str, Any]:
        """Generate comprehensive test report."""
        passed_tests = [r for r in self.all_results if r.passed]
        failed_tests = [r for r in self.all_results if not r.passed]
        
        # Component-wise summary
        component_summary = defaultdict(lambda: {"passed": 0, "failed": 0, "total_time": 0})
        for result in self.all_results:
            status = "passed" if result.passed else "failed"
            component_summary[result.component][status] += 1
            component_summary[result.component]["total_time"] += result.execution_time
        
        # Performance summary
        performance_summary = {}
        if self.benchmark_results:
            for component in set(b.component for b in self.benchmark_results):
                component_benchmarks = [b for b in self.benchmark_results if b.component == component]
                performance_summary[component] = {
                    "avg_throughput": sum(b.throughput for b in component_benchmarks) / len(component_benchmarks),
                    "max_memory_mb": max(b.memory_peak_mb for b in component_benchmarks),
                    "total_items": sum(b.items_processed for b in component_benchmarks)
                }
        
        report = {
            "summary": {
                "total_tests": len(self.all_results),
                "passed": len(passed_tests),
                "failed": len(failed_tests),
                "success_rate": len(passed_tests) / len(self.all_results) if self.all_results else 0,
                "total_execution_time": total_execution_time,
                "avg_test_time": sum(r.execution_time for r in self.all_results) / len(self.all_results) if self.all_results else 0
            },
            "component_summary": dict(component_summary),
            "performance_summary": performance_summary,
            "failed_tests": [
                {
                    "test_name": r.test_name,
                    "component": r.component,
                    "error": r.error_message,
                    "execution_time": r.execution_time
                }
                for r in failed_tests
            ],
            "detailed_results": [
                {
                    "test_name": r.test_name,
                    "component": r.component,
                    "passed": r.passed,
                    "execution_time": r.execution_time,
                    "memory_usage_mb": r.memory_usage_mb,
                    "metrics": r.metrics,
                    "warnings": r.warnings
                }
                for r in self.all_results
            ],
            "benchmark_results": [
                {
                    "component": b.component,
                    "scenario": b.test_scenario,
                    "throughput": b.throughput,
                    "execution_time": b.execution_time,
                    "memory_peak_mb": b.memory_peak_mb
                }
                for b in self.benchmark_results
            ]
        }
        
        return report
    
    def save_results(self, report: Dict[str, Any]):
        """Save test results to files."""
        timestamp = time.strftime("%Y%m%d_%H%M%S")
        
        # Save JSON report
        json_file = self.output_dir / f"test_report_{timestamp}.json"
        with open(json_file, 'w') as f:
            json.dump(report, f, indent=2, default=str)
        
        # Save summary report
        summary_file = self.output_dir / f"test_summary_{timestamp}.txt"
        with open(summary_file, 'w') as f:
            f.write("LEXICON Test Suite Results\n")
            f.write("=" * 50 + "\n\n")
            
            summary = report["summary"]
            f.write(f"Total Tests: {summary['total_tests']}\n")
            f.write(f"Passed: {summary['passed']}\n")
            f.write(f"Failed: {summary['failed']}\n")
            f.write(f"Success Rate: {summary['success_rate']:.1%}\n")
            f.write(f"Total Time: {summary['total_execution_time']:.2f}s\n\n")
            
            # Component summary
            f.write("Component Summary:\n")
            f.write("-" * 20 + "\n")
            for component, stats in report["component_summary"].items():
                f.write(f"{component}: {stats['passed']} passed, {stats['failed']} failed\n")
            
            # Failed tests
            if report["failed_tests"]:
                f.write("\nFailed Tests:\n")
                f.write("-" * 15 + "\n")
                for test in report["failed_tests"]:
                    f.write(f"- {test['test_name']} ({test['component']}): {test['error']}\n")
        
        self.logger.info(f"Test results saved to {json_file} and {summary_file}")


# Pytest integration
class TestLexiconComponents(unittest.TestCase):
    """Pytest-compatible test class."""
    
    @classmethod
    def setUpClass(cls):
        """Set up test class."""
        cls.config = LexiconConfig()
        cls.mock_data = MockDataGenerator()
    
    def test_entity_deduplicator_functionality(self):
        """Test entity deduplicator with pytest."""
        tester = ComponentTester(self.config)
        result = tester.test_entity_deduplicator()
        self.assertTrue(result.passed, f"Entity deduplicator test failed: {result.error_message}")
    
    def test_semantic_relation_detector_functionality(self):
        """Test semantic relation detector with pytest."""
        tester = ComponentTester(self.config)
        result = tester.test_semantic_relation_detector()
        self.assertTrue(result.passed, f"Semantic relation detector test failed: {result.error_message}")
    
    def test_enhanced_case_analyzer_functionality(self):
        """Test enhanced case analyzer with pytest."""
        tester = ComponentTester(self.config)
        result = tester.test_enhanced_case_analyzer()
        self.assertTrue(result.passed, f"Enhanced case analyzer test failed: {result.error_message}")
    
    def test_full_pipeline_integration(self):
        """Test full pipeline integration with pytest."""
        tester = IntegrationTester(self.config)
        result = tester.test_full_pipeline()
        self.assertTrue(result.passed, f"Full pipeline test failed: {result.error_message}")


if __name__ == "__main__":
    # Run comprehensive test suite
    test_suite = TestSuite()
    report = test_suite.run_comprehensive_tests(include_benchmarks=True)
    
    print(f"\nTest Results Summary:")
    print(f"Total Tests: {report['summary']['total_tests']}")
    print(f"Passed: {report['summary']['passed']}")
    print(f"Failed: {report['summary']['failed']}")
    print(f"Success Rate: {report['summary']['success_rate']:.1%}")
    print(f"Total Time: {report['summary']['total_execution_time']:.2f}s") 