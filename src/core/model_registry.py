#!/usr/bin/env python3
"""
Model Registry for CEREBRUM
Provides centralized management of model instances and their lifecycle
"""

import logging
import json
import pickle
import os
from datetime import datetime
from typing import Dict, Any, Optional, List, Union, Type
from dataclasses import dataclass, asdict, field
from pathlib import Path

from ..models.base import Model, Case

# Setup logging
logger = logging.getLogger("cerebrum-model-registry")

@dataclass
class ModelMetadata:
    """Metadata for registered models."""
    model_id: str
    model_type: str
    case: Case
    created_at: datetime
    last_accessed: datetime
    is_fitted: bool = False
    hyperparameters: Dict[str, Any] = field(default_factory=dict)
    performance_metrics: Dict[str, float] = field(default_factory=dict)
    file_path: Optional[str] = None
    tags: List[str] = field(default_factory=list)
    description: str = ""
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        result = asdict(self)
        result['case'] = self.case.value
        result['created_at'] = self.created_at.isoformat()
        result['last_accessed'] = self.last_accessed.isoformat()
        return result
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'ModelMetadata':
        """Create from dictionary."""
        data = data.copy()
        data['case'] = Case(data['case'])
        data['created_at'] = datetime.fromisoformat(data['created_at'])
        data['last_accessed'] = datetime.fromisoformat(data['last_accessed'])
        return cls(**data)

class ModelRegistry:
    """Registry for managing CEREBRUM model instances."""
    
    def __init__(self, registry_path: Optional[str] = None):
        """Initialize the model registry.
        
        Args:
            registry_path: Directory to store registry data and models
        """
        self.registry_path = Path(registry_path or "output/model_registry")
        self.registry_path.mkdir(parents=True, exist_ok=True)
        
        self.models_path = self.registry_path / "models"
        self.models_path.mkdir(exist_ok=True)
        
        self.metadata_file = self.registry_path / "registry.json"
        
        # In-memory registry
        self._models: Dict[str, Model] = {}
        self._metadata: Dict[str, ModelMetadata] = {}
        
        # Load existing registry
        self._load_registry()
        
        logger.info(f"Model registry initialized at {self.registry_path}")
    
    def register_model(
        self, 
        model: Model, 
        model_id: Optional[str] = None,
        tags: Optional[List[str]] = None,
        description: str = "",
        persist: bool = True
    ) -> str:
        """Register a model in the registry.
        
        Args:
            model: Model instance to register
            model_id: Unique identifier (uses model.name if not provided)
            tags: Tags for categorization
            description: Human-readable description
            persist: Whether to persist model to disk
            
        Returns:
            The model ID used for registration
        """
        model_id = model_id or model.name
        
        if model_id in self._models:
            logger.warning(f"Model {model_id} already registered, overwriting")
        
        # Create metadata
        metadata = ModelMetadata(
            model_id=model_id,
            model_type=type(model).__name__,
            case=model.case,
            created_at=datetime.now(),
            last_accessed=datetime.now(),
            is_fitted=getattr(model, '_is_fitted', False),
            hyperparameters=getattr(model, '_hyperparameters', {}),
            tags=tags or [],
            description=description
        )
        
        # Store in memory
        self._models[model_id] = model
        self._metadata[model_id] = metadata
        
        # Persist to disk if requested
        if persist:
            model_file = self.models_path / f"{model_id}.pkl"
            metadata.file_path = str(model_file)
            
            try:
                with open(model_file, 'wb') as f:
                    pickle.dump(model, f)
                logger.info(f"Model {model_id} persisted to {model_file}")
            except Exception as e:
                logger.error(f"Failed to persist model {model_id}: {e}")
                metadata.file_path = None
        
        # Save registry metadata
        self._save_registry()
        
        logger.info(f"Model {model_id} registered successfully")
        return model_id
    
    def get_model(self, model_id: str, load_if_needed: bool = True) -> Optional[Model]:
        """Retrieve a model from the registry.
        
        Args:
            model_id: Model identifier
            load_if_needed: Load from disk if not in memory
            
        Returns:
            Model instance or None if not found
        """
        # Check in-memory first
        if model_id in self._models:
            self._metadata[model_id].last_accessed = datetime.now()
            self._save_registry()
            return self._models[model_id]
        
        # Load from disk if needed
        if load_if_needed and model_id in self._metadata:
            metadata = self._metadata[model_id]
            if metadata.file_path and os.path.exists(metadata.file_path):
                try:
                    with open(metadata.file_path, 'rb') as f:
                        model = pickle.load(f)
                    
                    self._models[model_id] = model
                    metadata.last_accessed = datetime.now()
                    self._save_registry()
                    
                    logger.info(f"Model {model_id} loaded from disk")
                    return model
                    
                except Exception as e:
                    logger.error(f"Failed to load model {model_id}: {e}")
        
        logger.warning(f"Model {model_id} not found")
        return None
    
    def list_models(
        self, 
        case: Optional[Case] = None,
        model_type: Optional[str] = None,
        tags: Optional[List[str]] = None
    ) -> List[ModelMetadata]:
        """List models with optional filtering.
        
        Args:
            case: Filter by linguistic case
            model_type: Filter by model type
            tags: Filter by tags (any of the provided tags)
            
        Returns:
            List of model metadata
        """
        models = list(self._metadata.values())
        
        if case:
            models = [m for m in models if m.case == case]
        
        if model_type:
            models = [m for m in models if m.model_type == model_type]
        
        if tags:
            models = [m for m in models if any(tag in m.tags for tag in tags)]
        
        return sorted(models, key=lambda x: x.last_accessed, reverse=True)
    
    def update_model_metrics(self, model_id: str, metrics: Dict[str, float]) -> bool:
        """Update performance metrics for a model.
        
        Args:
            model_id: Model identifier
            metrics: Performance metrics to update
            
        Returns:
            True if successful, False otherwise
        """
        if model_id not in self._metadata:
            logger.error(f"Model {model_id} not found")
            return False
        
        self._metadata[model_id].performance_metrics.update(metrics)
        self._metadata[model_id].last_accessed = datetime.now()
        self._save_registry()
        
        logger.info(f"Updated metrics for model {model_id}: {metrics}")
        return True
    
    def remove_model(self, model_id: str, delete_files: bool = True) -> bool:
        """Remove a model from the registry.
        
        Args:
            model_id: Model identifier
            delete_files: Whether to delete persisted files
            
        Returns:
            True if successful, False otherwise
        """
        if model_id not in self._metadata:
            logger.error(f"Model {model_id} not found")
            return False
        
        metadata = self._metadata[model_id]
        
        # Remove from memory
        if model_id in self._models:
            del self._models[model_id]
        
        # Delete files if requested
        if delete_files and metadata.file_path and os.path.exists(metadata.file_path):
            try:
                os.remove(metadata.file_path)
                logger.info(f"Deleted model file {metadata.file_path}")
            except Exception as e:
                logger.error(f"Failed to delete model file: {e}")
        
        # Remove metadata
        del self._metadata[model_id]
        self._save_registry()
        
        logger.info(f"Model {model_id} removed from registry")
        return True
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get registry statistics.
        
        Returns:
            Dictionary of statistics
        """
        models = list(self._metadata.values())
        
        stats = {
            'total_models': len(models),
            'fitted_models': sum(1 for m in models if m.is_fitted),
            'unfitted_models': sum(1 for m in models if not m.is_fitted),
            'models_by_case': {},
            'models_by_type': {},
            'oldest_model': None,
            'newest_model': None,
            'most_recent_access': None
        }
        
        if models:
            # Group by case
            for case in Case:
                count = sum(1 for m in models if m.case == case)
                if count > 0:
                    stats['models_by_case'][case.value] = count
            
            # Group by type
            types = {}
            for m in models:
                types[m.model_type] = types.get(m.model_type, 0) + 1
            stats['models_by_type'] = types
            
            # Temporal stats
            stats['oldest_model'] = min(models, key=lambda x: x.created_at).model_id
            stats['newest_model'] = max(models, key=lambda x: x.created_at).model_id
            stats['most_recent_access'] = max(models, key=lambda x: x.last_accessed).model_id
        
        return stats
    
    def export_registry(self, export_path: str) -> bool:
        """Export registry metadata to a file.
        
        Args:
            export_path: Path to export file
            
        Returns:
            True if successful, False otherwise
        """
        try:
            export_data = {
                'registry_version': '1.0',
                'exported_at': datetime.now().isoformat(),
                'models': {
                    model_id: metadata.to_dict() 
                    for model_id, metadata in self._metadata.items()
                }
            }
            
            with open(export_path, 'w') as f:
                json.dump(export_data, f, indent=2)
            
            logger.info(f"Registry exported to {export_path}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to export registry: {e}")
            return False
    
    def _load_registry(self) -> None:
        """Load registry metadata from disk."""
        if not self.metadata_file.exists():
            logger.info("No existing registry found, starting fresh")
            return
        
        try:
            with open(self.metadata_file, 'r') as f:
                data = json.load(f)
            
            for model_id, metadata_dict in data.get('models', {}).items():
                metadata = ModelMetadata.from_dict(metadata_dict)
                self._metadata[model_id] = metadata
            
            logger.info(f"Loaded {len(self._metadata)} models from registry")
            
        except Exception as e:
            logger.error(f"Failed to load registry: {e}")
    
    def _save_registry(self) -> None:
        """Save registry metadata to disk."""
        try:
            registry_data = {
                'registry_version': '1.0',
                'last_updated': datetime.now().isoformat(),
                'models': {
                    model_id: metadata.to_dict() 
                    for model_id, metadata in self._metadata.items()
                }
            }
            
            with open(self.metadata_file, 'w') as f:
                json.dump(registry_data, f, indent=2)
                
        except Exception as e:
            logger.error(f"Failed to save registry: {e}")

# Global registry instance
_global_registry: Optional[ModelRegistry] = None

def get_global_registry() -> ModelRegistry:
    """Get or create the global model registry."""
    global _global_registry
    if _global_registry is None:
        _global_registry = ModelRegistry()
    return _global_registry

def register_model(model: Model, **kwargs) -> str:
    """Convenience function to register a model in the global registry."""
    return get_global_registry().register_model(model, **kwargs)

def get_model(model_id: str, **kwargs) -> Optional[Model]:
    """Convenience function to get a model from the global registry."""
    return get_global_registry().get_model(model_id, **kwargs) 