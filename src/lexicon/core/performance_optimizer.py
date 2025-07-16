"""
LEXICON Performance Optimizer

Advanced performance optimization system for handling large text datasets:
- Memory-efficient batch processing
- Intelligent caching strategies
- Resource monitoring and management
- Progressive processing capabilities
- Parallel execution optimization
"""

import gc
import os
import time
import psutil
import logging
import pickle
import hashlib
from typing import List, Dict, Any, Optional, Iterator, Callable, Tuple, Union
from dataclasses import dataclass, asdict
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, as_completed
import threading
from collections import defaultdict
import json

from .config import LexiconConfig
from .logging import get_logger

try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False

try:
    import pandas as pd
    PANDAS_AVAILABLE = True
except ImportError:
    PANDAS_AVAILABLE = False


@dataclass
class ResourceMetrics:
    """System resource usage metrics."""
    cpu_percent: float
    memory_percent: float
    memory_used_gb: float
    memory_available_gb: float
    process_memory_mb: float
    disk_usage_percent: float
    timestamp: float


@dataclass
class ProcessingMetrics:
    """Processing performance metrics."""
    total_items: int
    processed_items: int
    processing_rate: float  # items per second
    estimated_completion: float  # seconds
    cache_hits: int
    cache_misses: int
    memory_peak_mb: float
    batch_size_used: int


@dataclass
class CacheEntry:
    """Cache entry with metadata."""
    data: Any
    timestamp: float
    access_count: int
    size_bytes: int
    ttl: Optional[float] = None


class MemoryMonitor:
    """Monitor and manage memory usage."""
    
    def __init__(self, warning_threshold: float = 0.8, critical_threshold: float = 0.9):
        """
        Initialize memory monitor.
        
        Args:
            warning_threshold: Memory usage percentage to trigger warnings
            critical_threshold: Memory usage percentage to trigger aggressive cleanup
        """
        self.warning_threshold = warning_threshold
        self.critical_threshold = critical_threshold
        self.logger = get_logger("core.memory_monitor")
        
    def get_memory_info(self) -> ResourceMetrics:
        """Get current memory information."""
        memory = psutil.virtual_memory()
        process = psutil.Process()
        
        return ResourceMetrics(
            cpu_percent=psutil.cpu_percent(),
            memory_percent=memory.percent / 100,
            memory_used_gb=memory.used / (1024**3),
            memory_available_gb=memory.available / (1024**3),
            process_memory_mb=process.memory_info().rss / (1024**2),
            disk_usage_percent=psutil.disk_usage('/').percent / 100,
            timestamp=time.time()
        )
    
    def check_memory_pressure(self) -> str:
        """Check memory pressure level."""
        metrics = self.get_memory_info()
        
        if metrics.memory_percent >= self.critical_threshold:
            return "critical"
        elif metrics.memory_percent >= self.warning_threshold:
            return "warning"
        else:
            return "normal"
    
    def trigger_cleanup(self, aggressive: bool = False):
        """Trigger memory cleanup."""
        self.logger.info(f"Triggering {'aggressive' if aggressive else 'normal'} memory cleanup")
        
        # Force garbage collection
        for _ in range(3):
            gc.collect()
        
        if aggressive:
            # More aggressive cleanup strategies
            self._clear_module_caches()
    
    def _clear_module_caches(self):
        """Clear various module-level caches."""
        # Clear regex cache
        import re
        re._cache.clear()
        
        # Clear other caches as needed
        if NUMPY_AVAILABLE:
            # Clear numpy's internal caches if possible
            pass


class LRUCache:
    """Least Recently Used cache with size and TTL limits."""
    
    def __init__(self, max_size: int = 1000, max_memory_mb: float = 100, 
                 default_ttl: Optional[float] = None):
        """
        Initialize LRU cache.
        
        Args:
            max_size: Maximum number of entries
            max_memory_mb: Maximum memory usage in MB
            default_ttl: Default time-to-live in seconds
        """
        self.max_size = max_size
        self.max_memory_bytes = max_memory_mb * 1024 * 1024
        self.default_ttl = default_ttl
        
        self._cache: Dict[str, CacheEntry] = {}
        self._access_order: List[str] = []
        self._total_size_bytes = 0
        self._lock = threading.RLock()
        
        self.stats = {
            "hits": 0,
            "misses": 0,
            "evictions": 0,
            "expired": 0
        }
    
    def get(self, key: str) -> Optional[Any]:
        """Get value from cache."""
        with self._lock:
            if key not in self._cache:
                self.stats["misses"] += 1
                return None
            
            entry = self._cache[key]
            
            # Check TTL
            if entry.ttl and time.time() > entry.timestamp + entry.ttl:
                self._remove_entry(key)
                self.stats["expired"] += 1
                self.stats["misses"] += 1
                return None
            
            # Update access
            entry.access_count += 1
            self._update_access_order(key)
            
            self.stats["hits"] += 1
            return entry.data
    
    def put(self, key: str, value: Any, ttl: Optional[float] = None) -> bool:
        """Put value in cache."""
        with self._lock:
            # Calculate size
            size_bytes = self._estimate_size(value)
            
            # Check if it would exceed memory limit
            if size_bytes > self.max_memory_bytes:
                return False
            
            # Remove existing entry if present
            if key in self._cache:
                self._remove_entry(key)
            
            # Make space if needed
            while (len(self._cache) >= self.max_size or 
                   self._total_size_bytes + size_bytes > self.max_memory_bytes):
                if not self._evict_lru():
                    return False
            
            # Add new entry
            entry = CacheEntry(
                data=value,
                timestamp=time.time(),
                access_count=1,
                size_bytes=size_bytes,
                ttl=ttl or self.default_ttl
            )
            
            self._cache[key] = entry
            self._access_order.append(key)
            self._total_size_bytes += size_bytes
            
            return True
    
    def _remove_entry(self, key: str):
        """Remove entry from cache."""
        if key in self._cache:
            entry = self._cache[key]
            self._total_size_bytes -= entry.size_bytes
            del self._cache[key]
            
            if key in self._access_order:
                self._access_order.remove(key)
    
    def _evict_lru(self) -> bool:
        """Evict least recently used entry."""
        if not self._access_order:
            return False
        
        lru_key = self._access_order[0]
        self._remove_entry(lru_key)
        self.stats["evictions"] += 1
        return True
    
    def _update_access_order(self, key: str):
        """Update access order for key."""
        if key in self._access_order:
            self._access_order.remove(key)
        self._access_order.append(key)
    
    def _estimate_size(self, obj: Any) -> int:
        """Estimate object size in bytes."""
        try:
            return len(pickle.dumps(obj))
        except:
            # Fallback estimation
            if isinstance(obj, str):
                return len(obj.encode('utf-8'))
            elif isinstance(obj, (list, tuple)):
                return sum(self._estimate_size(item) for item in obj)
            elif isinstance(obj, dict):
                return sum(self._estimate_size(k) + self._estimate_size(v) 
                          for k, v in obj.items())
            else:
                return 1024  # Default estimate
    
    def clear(self):
        """Clear all cache entries."""
        with self._lock:
            self._cache.clear()
            self._access_order.clear()
            self._total_size_bytes = 0
    
    def get_stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        with self._lock:
            total_accesses = self.stats["hits"] + self.stats["misses"]
            hit_rate = self.stats["hits"] / total_accesses if total_accesses > 0 else 0
            
            return {
                **self.stats,
                "hit_rate": hit_rate,
                "size": len(self._cache),
                "memory_usage_mb": self._total_size_bytes / (1024 * 1024)
            }


class BatchProcessor:
    """Efficient batch processing for large datasets."""
    
    def __init__(self, batch_size: int = 100, max_workers: int = None,
                 use_multiprocessing: bool = False):
        """
        Initialize batch processor.
        
        Args:
            batch_size: Default batch size
            max_workers: Maximum worker threads/processes
            use_multiprocessing: Whether to use multiprocessing vs threading
        """
        self.default_batch_size = batch_size
        self.max_workers = max_workers or min(32, (os.cpu_count() or 1) + 4)
        self.use_multiprocessing = use_multiprocessing
        
        self.logger = get_logger("core.batch_processor")
        
    def process_batches(self, items: List[Any], 
                       processing_func: Callable,
                       batch_size: Optional[int] = None,
                       progress_callback: Optional[Callable] = None) -> List[Any]:
        """
        Process items in batches.
        
        Args:
            items: List of items to process
            processing_func: Function to apply to each batch
            batch_size: Batch size (uses default if None)
            progress_callback: Optional callback for progress updates
            
        Returns:
            List of processing results
        """
        batch_size = batch_size or self.default_batch_size
        batches = [items[i:i + batch_size] for i in range(0, len(items), batch_size)]
        
        self.logger.info(f"Processing {len(items)} items in {len(batches)} batches")
        
        results = []
        start_time = time.time()
        
        if self.max_workers == 1:
            # Sequential processing
            for i, batch in enumerate(batches):
                batch_result = processing_func(batch)
                results.extend(batch_result if isinstance(batch_result, list) else [batch_result])
                
                if progress_callback:
                    progress = (i + 1) / len(batches)
                    progress_callback(progress, i + 1, len(batches))
        else:
            # Parallel processing
            executor_class = ProcessPoolExecutor if self.use_multiprocessing else ThreadPoolExecutor
            
            with executor_class(max_workers=self.max_workers) as executor:
                future_to_batch = {executor.submit(processing_func, batch): i 
                                 for i, batch in enumerate(batches)}
                
                completed = 0
                for future in as_completed(future_to_batch):
                    batch_idx = future_to_batch[future]
                    try:
                        batch_result = future.result()
                        results.extend(batch_result if isinstance(batch_result, list) else [batch_result])
                        
                        completed += 1
                        if progress_callback:
                            progress = completed / len(batches)
                            progress_callback(progress, completed, len(batches))
                            
                    except Exception as e:
                        self.logger.error(f"Batch {batch_idx} failed: {e}")
        
        total_time = time.time() - start_time
        processing_rate = len(items) / total_time if total_time > 0 else 0
        
        self.logger.info(f"Processed {len(items)} items in {total_time:.2f}s "
                        f"({processing_rate:.1f} items/s)")
        
        return results
    
    def process_streaming(self, items: Iterator[Any],
                         processing_func: Callable,
                         batch_size: Optional[int] = None,
                         buffer_size: int = 1000) -> Iterator[Any]:
        """
        Process items in streaming fashion.
        
        Args:
            items: Iterator of items to process
            processing_func: Function to apply to each batch
            batch_size: Batch size
            buffer_size: Internal buffer size
            
        Yields:
            Processing results
        """
        batch_size = batch_size or self.default_batch_size
        
        buffer = []
        for item in items:
            buffer.append(item)
            
            if len(buffer) >= batch_size:
                # Process batch
                batch_results = processing_func(buffer[:batch_size])
                for result in (batch_results if isinstance(batch_results, list) else [batch_results]):
                    yield result
                
                # Remove processed items
                buffer = buffer[batch_size:]
                
                # Manage buffer size
                if len(buffer) > buffer_size:
                    self.logger.warning("Buffer overflow, reducing buffer size")
                    buffer = buffer[-buffer_size//2:]
        
        # Process remaining items
        if buffer:
            batch_results = processing_func(buffer)
            for result in (batch_results if isinstance(batch_results, list) else [batch_results]):
                yield result


class PerformanceOptimizer:
    """Main performance optimization coordinator."""
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """
        Initialize performance optimizer.
        
        Args:
            config: LEXICON configuration object
        """
        self.config = config
        self.logger = get_logger("core.performance_optimizer")
        
        # Initialize components
        self.memory_monitor = MemoryMonitor()
        self.cache = LRUCache(
            max_size=getattr(config, 'cache_max_size', 1000),
            max_memory_mb=getattr(config, 'cache_max_memory_mb', 100)
        )
        self.batch_processor = BatchProcessor(
            batch_size=getattr(config, 'batch_size', 100),
            max_workers=getattr(config, 'max_workers', None)
        )
        
        # Performance tracking
        self.metrics = ProcessingMetrics(
            total_items=0,
            processed_items=0,
            processing_rate=0.0,
            estimated_completion=0.0,
            cache_hits=0,
            cache_misses=0,
            memory_peak_mb=0.0,
            batch_size_used=0
        )
        
        # Adaptive batch sizing
        self._adaptive_batch_size = getattr(config, 'batch_size', 100)
        self._batch_performance_history = []
        
    def optimize_batch_size(self, items: List[Any], 
                           processing_func: Callable,
                           target_memory_mb: float = 50) -> int:
        """
        Determine optimal batch size based on memory usage and performance.
        
        Args:
            items: Sample items to process
            processing_func: Processing function
            target_memory_mb: Target memory usage per batch
            
        Returns:
            Optimal batch size
        """
        if len(items) < 10:
            return len(items)
        
        self.logger.info("Optimizing batch size...")
        
        # Test different batch sizes
        test_sizes = [10, 25, 50, 100, 200, 500]
        test_sizes = [size for size in test_sizes if size <= len(items)]
        
        performance_results = []
        
        for batch_size in test_sizes:
            # Test with a small sample
            test_items = items[:min(batch_size * 2, len(items))]
            test_batches = [test_items[i:i + batch_size] 
                          for i in range(0, len(test_items), batch_size)]
            
            if not test_batches:
                continue
            
            # Measure performance
            start_time = time.time()
            start_memory = self.memory_monitor.get_memory_info().process_memory_mb
            
            try:
                for batch in test_batches[:2]:  # Test only first 2 batches
                    processing_func(batch)
                
                end_time = time.time()
                end_memory = self.memory_monitor.get_memory_info().process_memory_mb
                
                processing_time = end_time - start_time
                memory_used = end_memory - start_memory
                throughput = len(test_items) / processing_time if processing_time > 0 else 0
                
                performance_results.append({
                    'batch_size': batch_size,
                    'throughput': throughput,
                    'memory_per_item': memory_used / len(test_items) if test_items else 0,
                    'score': throughput / max(1, memory_used / target_memory_mb)
                })
                
            except Exception as e:
                self.logger.warning(f"Batch size {batch_size} failed: {e}")
        
        if not performance_results:
            return self.batch_processor.default_batch_size
        
        # Select best batch size
        best_result = max(performance_results, key=lambda x: x['score'])
        optimal_size = best_result['batch_size']
        
        self.logger.info(f"Optimal batch size: {optimal_size} "
                        f"(throughput: {best_result['throughput']:.1f} items/s)")
        
        return optimal_size
    
    def create_processing_key(self, text: str, config_params: Dict[str, Any]) -> str:
        """Create cache key for processing results."""
        # Create deterministic hash from text and parameters
        text_hash = hashlib.md5(text.encode('utf-8')).hexdigest()[:16]
        params_str = json.dumps(config_params, sort_keys=True)
        params_hash = hashlib.md5(params_str.encode('utf-8')).hexdigest()[:16]
        
        return f"{text_hash}_{params_hash}"
    
    def cached_process(self, key: str, processing_func: Callable, 
                      *args, **kwargs) -> Any:
        """Process with caching."""
        # Check cache first
        cached_result = self.cache.get(key)
        if cached_result is not None:
            self.metrics.cache_hits += 1
            return cached_result
        
        # Process and cache result
        result = processing_func(*args, **kwargs)
        self.cache.put(key, result)
        self.metrics.cache_misses += 1
        
        return result
    
    def monitor_processing(self, total_items: int) -> 'ProcessingMonitor':
        """Create processing monitor context."""
        return ProcessingMonitor(self, total_items)
    
    def adaptive_process(self, items: List[Any], 
                        processing_func: Callable,
                        target_performance: Dict[str, float] = None) -> List[Any]:
        """
        Process items with adaptive optimization.
        
        Args:
            items: Items to process
            processing_func: Processing function
            target_performance: Target performance metrics
            
        Returns:
            Processing results
        """
        target_performance = target_performance or {
            'max_memory_mb': 200,
            'min_throughput': 10  # items per second
        }
        
        # Optimize batch size
        optimal_batch_size = self.optimize_batch_size(
            items[:min(100, len(items))], 
            processing_func,
            target_performance.get('max_memory_mb', 200) / 4
        )
        
        # Process with monitoring
        with self.monitor_processing(len(items)) as monitor:
            results = self.batch_processor.process_batches(
                items, 
                processing_func,
                batch_size=optimal_batch_size,
                progress_callback=monitor.update_progress
            )
        
        return results
    
    def get_performance_report(self) -> Dict[str, Any]:
        """Get comprehensive performance report."""
        memory_info = self.memory_monitor.get_memory_info()
        cache_stats = self.cache.get_stats()
        
        return {
            "processing_metrics": asdict(self.metrics),
            "memory_info": asdict(memory_info),
            "cache_stats": cache_stats,
            "memory_pressure": self.memory_monitor.check_memory_pressure(),
            "adaptive_batch_size": self._adaptive_batch_size,
            "recommendations": self._generate_recommendations(memory_info, cache_stats)
        }
    
    def _generate_recommendations(self, memory_info: ResourceMetrics, 
                                cache_stats: Dict[str, Any]) -> List[str]:
        """Generate performance optimization recommendations."""
        recommendations = []
        
        if memory_info.memory_percent > 0.8:
            recommendations.append("High memory usage detected. Consider reducing batch size.")
        
        if cache_stats["hit_rate"] < 0.3:
            recommendations.append("Low cache hit rate. Consider increasing cache size.")
        
        if memory_info.cpu_percent > 90:
            recommendations.append("High CPU usage. Consider reducing parallelism.")
        
        if self.metrics.processing_rate < 5:
            recommendations.append("Low processing rate. Consider optimizing processing function.")
        
        return recommendations
    
    def cleanup_resources(self):
        """Clean up resources and caches."""
        self.logger.info("Cleaning up performance optimizer resources")
        
        self.cache.clear()
        self.memory_monitor.trigger_cleanup(aggressive=True)
        
        # Reset metrics
        self.metrics = ProcessingMetrics(
            total_items=0,
            processed_items=0,
            processing_rate=0.0,
            estimated_completion=0.0,
            cache_hits=0,
            cache_misses=0,
            memory_peak_mb=0.0,
            batch_size_used=0
        )


class ProcessingMonitor:
    """Context manager for monitoring processing progress."""
    
    def __init__(self, optimizer: PerformanceOptimizer, total_items: int):
        self.optimizer = optimizer
        self.total_items = total_items
        self.start_time = None
        self.processed_count = 0
        
    def __enter__(self):
        self.start_time = time.time()
        self.optimizer.metrics.total_items = self.total_items
        self.optimizer.metrics.processed_items = 0
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.start_time:
            total_time = time.time() - self.start_time
            if total_time > 0:
                self.optimizer.metrics.processing_rate = self.processed_count / total_time
    
    def update_progress(self, progress: float, completed: int, total: int):
        """Update processing progress."""
        self.processed_count = completed
        self.optimizer.metrics.processed_items = completed
        
        if self.start_time and completed > 0:
            elapsed = time.time() - self.start_time
            rate = completed / elapsed
            remaining = total - completed
            
            self.optimizer.metrics.processing_rate = rate
            self.optimizer.metrics.estimated_completion = remaining / rate if rate > 0 else 0
            
            # Update memory peak
            current_memory = self.optimizer.memory_monitor.get_memory_info().process_memory_mb
            self.optimizer.metrics.memory_peak_mb = max(
                self.optimizer.metrics.memory_peak_mb, 
                current_memory
            )
            
            # Check memory pressure and trigger cleanup if needed
            pressure = self.optimizer.memory_monitor.check_memory_pressure()
            if pressure == "critical":
                self.optimizer.memory_monitor.trigger_cleanup(aggressive=True)
            elif pressure == "warning":
                self.optimizer.memory_monitor.trigger_cleanup(aggressive=False) 