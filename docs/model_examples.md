# CEREBRUM Model Examples

This document provides concrete examples of CEREBRUM models in various domains, demonstrating how the case system can be applied to real-world problems.

## 1. Thermostat Model Example

The thermostat model from the CEREBRUM paper is implemented here in Python to demonstrate basic case transformations.

### 1.1 Base Model Definition

```python
import numpy as np
from cerebrum import GenerativeModel, Case, ParameterAccessPattern

class ThermostatModel(GenerativeModel):
    def __init__(self, model_id):
        super().__init__(model_id)
        # Core parameters
        self.parameters = {
            'target_temp': 72.0,
            'comfort_range': 2.0,
            'heating_rate': 0.5,
            'cooling_rate': 0.3,
            'sensor_precision': 0.1,
            'system_response_time': 10,
        }
        self.state = {
            'current_temp': 70.0,
            'system_mode': 'idle',  # 'idle', 'heating', 'cooling'
            'prediction_horizon': 30,  # minutes
        }
        # Define supported cases
        self.supported_cases = [
            Case('nominative', 'NOM', ParameterAccessPattern.FULL_ACCESS),
            Case('accusative', 'ACC', ParameterAccessPattern.UPDATE_FOCUSED),
            Case('dative', 'DAT', ParameterAccessPattern.INPUT_FOCUSED),
            Case('genitive', 'GEN', ParameterAccessPattern.OUTPUT_FOCUSED),
            Case('instrumental', 'INS', ParameterAccessPattern.METHOD_FOCUSED),
            Case('locative', 'LOC', ParameterAccessPattern.CONTEXT_FOCUSED),
            Case('ablative', 'ABL', ParameterAccessPattern.HISTORY_FOCUSED),
            Case('vocative', 'VOC', ParameterAccessPattern.IDENTITY_FOCUSED),
        ]
        self.current_case = self.supported_cases[0]  # Default to nominative
        
    def predict(self, inputs=None):
        """Generate temperature predictions based on current state."""
        if self.current_case.abbreviation == 'NOM':
            # Nominative case: full prediction generation
            return self._generate_temperature_prediction()
        elif self.current_case.abbreviation == 'GEN':
            # Genitive case: generate report
            return self._generate_performance_report()
        elif self.current_case.abbreviation == 'INS':
            # Instrumental case: provide control algorithm
            return self._provide_control_algorithm()
        # Other cases would have their specific prediction behaviors
        
    def _generate_temperature_prediction(self):
        """Nominative case prediction logic."""
        current_temp = self.state['current_temp']
        target = self.parameters['target_temp']
        heating_rate = self.parameters['heating_rate']
        cooling_rate = self.parameters['cooling_rate']
        horizon = self.state['prediction_horizon']
        
        # Simple temperature prediction model
        predictions = np.zeros(horizon)
        predictions[0] = current_temp
        
        for i in range(1, horizon):
            if predictions[i-1] < target - self.parameters['comfort_range']:
                # Heating mode
                predictions[i] = predictions[i-1] + heating_rate
            elif predictions[i-1] > target + self.parameters['comfort_range']:
                # Cooling mode
                predictions[i] = predictions[i-1] - cooling_rate
            else:
                # Idle mode - slight natural drift
                predictions[i] = predictions[i-1] + np.random.normal(0, 0.1)
                
        return {
            'temperature_predictions': predictions,
            'confidence_intervals': self._calculate_confidence(predictions),
            'control_signals': self._derive_control_signals(predictions),
        }
    
    def _generate_performance_report(self):
        """Genitive case: generating output artifacts."""
        # In genitive case, generate reports about system performance
        return {
            'efficiency_metrics': {
                'energy_usage': self._calculate_energy_usage(),
                'comfort_maintenance': self._evaluate_comfort_maintenance(),
            },
            'operational_summary': f"System operating at {self._calculate_efficiency()}% efficiency",
            'visualization_data': self._prepare_visualization_data(),
        }
    
    def _provide_control_algorithm(self):
        """Instrumental case: providing control methods."""
        # In instrumental case, provide the control algorithm itself
        return {
            'control_function': self._temperature_control_algorithm,
            'parameters': {
                'heating_rate': self.parameters['heating_rate'],
                'cooling_rate': self.parameters['cooling_rate'],
                'target_temp': self.parameters['target_temp'],
                'comfort_range': self.parameters['comfort_range'],
            }
        }
    
    def update(self, prediction, observation):
        """Update model based on observations."""
        if self.current_case.abbreviation == 'ACC':
            # Accusative case: focused on being updated
            return self._update_parameters(prediction, observation)
        elif self.current_case.abbreviation == 'DAT':
            # Dative case: focused on receiving data
            return self._process_incoming_data(observation)
        # Other cases would have their specific update behaviors
    
    def _update_parameters(self, prediction, observation):
        """Accusative case update logic."""
        # Calculate prediction error
        temp_error = observation['measured_temp'] - prediction['temperature_predictions'][0]
        
        # Update parameters based on error
        self.parameters['target_temp'] += 0.1 * temp_error
        self.parameters['heating_rate'] *= (1 + 0.01 * abs(temp_error))
        self.parameters['cooling_rate'] *= (1 + 0.01 * abs(temp_error))
        
        return {
            'parameter_updates': {
                'target_temp': self.parameters['target_temp'],
                'heating_rate': self.parameters['heating_rate'],
                'cooling_rate': self.parameters['cooling_rate'],
            },
            'error_magnitude': abs(temp_error),
        }
    
    def _process_incoming_data(self, observation):
        """Dative case update logic."""
        # In dative case, focus on receiving and processing incoming data
        self.state['current_temp'] = observation['measured_temp']
        self.state['system_mode'] = observation['system_status']
        
        # Process and categorize the incoming data
        return {
            'data_processed': True,
            'anomalies_detected': self._detect_anomalies(observation),
            'comfort_status': self._evaluate_comfort_status(observation['measured_temp']),
        }
    
    def transform_case(self, target_case_abbr):
        """Transform the model to a different case."""
        # Find the target case
        target_case = next((case for case in self.supported_cases 
                           if case.abbreviation == target_case_abbr), None)
        
        if not target_case:
            return {'success': False, 'error': 'Unsupported case'}
            
        # Perform the transformation
        old_case = self.current_case
        self.current_case = target_case
        
        # Adjust parameter access based on new case
        self._adjust_parameter_access()
        
        return {
            'success': True,
            'previous_case': old_case.abbreviation,
            'new_case': target_case.abbreviation,
            'transformation_time': '0.05s',
        }
    
    def _adjust_parameter_access(self):
        """Adjust parameter access patterns based on current case."""
        # Different parameter access patterns for different cases
        if self.current_case.abbreviation == 'NOM':
            # Full access for nominative case
            self._parameter_access = {param: 'read-write' for param in self.parameters}
        elif self.current_case.abbreviation == 'ACC':
            # Prioritize learning parameters for accusative case
            self._parameter_access = {
                'target_temp': 'read-write',
                'comfort_range': 'read-write',
                'heating_rate': 'read-write',
                'cooling_rate': 'read-write',
                'sensor_precision': 'read-only',
                'system_response_time': 'read-only',
            }
        elif self.current_case.abbreviation == 'DAT':
            # Prioritize input parameters for dative case
            self._parameter_access = {
                'sensor_precision': 'read-write',
                'system_response_time': 'read-write',
                'target_temp': 'read-only',
                'comfort_range': 'read-only',
                'heating_rate': 'read-only',
                'cooling_rate': 'read-only',
            }
        # Similarly for other cases
```

### 1.2 Using the Thermostat Model

```python
from cerebrum import ModelRegistry

# Create and register the model
thermostat = ThermostatModel('main_thermostat')
registry = ModelRegistry()
registry.register_model(thermostat, 'NOM')  # Register with nominative case

# Use the model in nominative case (active predictor)
predictions = thermostat.predict()
print(f"Temperature predictions: {predictions['temperature_predictions']}")

# Transform to accusative case (model to be updated)
thermostat.transform_case('ACC')
observation = {'measured_temp': 68.5, 'system_status': 'heating'}
update_result = thermostat.update(predictions, observation)
print(f"Parameter updates: {update_result['parameter_updates']}")

# Transform to genitive case (report generator)
thermostat.transform_case('GEN')
report = thermostat.predict()
print(f"Efficiency: {report['efficiency_metrics']['energy_usage']}")

# Transform to instrumental case (method provider)
thermostat.transform_case('INS')
control_algorithm = thermostat.predict()
print(f"Control algorithm parameters: {control_algorithm['parameters']}")
```

## 2. Language Model Example

This example demonstrates how a language model can take on different cases, changing its behavior based on its functional role.

### 2.1 Language Model Definition

```python
import numpy as np
from cerebrum import GenerativeModel, Case, ParameterAccessPattern

class LanguageModel(GenerativeModel):
    def __init__(self, model_id, vocabulary_size=10000, embedding_dim=256, context_length=100):
        super().__init__(model_id)
        # Core parameters
        self.parameters = {
            'vocabulary_size': vocabulary_size,
            'embedding_dimension': embedding_dim,
            'context_length': context_length,
            'temperature': 0.7,
            'top_p': 0.9,
            'frequency_penalty': 0.0,
            'presence_penalty': 0.0,
        }
        # Internal state
        self.state = {
            'token_embeddings': np.random.normal(0, 0.1, (vocabulary_size, embedding_dim)),
            'current_context': [],
            'generation_mode': 'greedy',  # 'greedy', 'sampling', 'beam'
        }
        # Define supported cases
        self.supported_cases = [
            Case('nominative', 'NOM', ParameterAccessPattern.FULL_ACCESS),
            Case('accusative', 'ACC', ParameterAccessPattern.UPDATE_FOCUSED),
            Case('dative', 'DAT', ParameterAccessPattern.INPUT_FOCUSED),
            Case('genitive', 'GEN', ParameterAccessPattern.OUTPUT_FOCUSED),
            Case('instrumental', 'INS', ParameterAccessPattern.METHOD_FOCUSED),
            Case('locative', 'LOC', ParameterAccessPattern.CONTEXT_FOCUSED),
            Case('vocative', 'VOC', ParameterAccessPattern.IDENTITY_FOCUSED),
        ]
        self.current_case = self.supported_cases[0]  # Default to nominative
    
    def predict(self, inputs=None):
        """Generate text predictions based on the model's current case."""
        if self.current_case.abbreviation == 'NOM':
            # Nominative case: generate text actively
            return self._generate_text(inputs or {'prompt': ''})
        elif self.current_case.abbreviation == 'GEN':
            # Genitive case: generate language artifacts
            return self._generate_language_artifacts(inputs or {'type': 'summary'})
        elif self.current_case.abbreviation == 'INS':
            # Instrumental case: provide language processing tools
            return self._provide_language_tools()
        elif self.current_case.abbreviation == 'LOC':
            # Locative case: provide context analysis
            return self._analyze_context(inputs or {'text': ''})
        # Other cases would have specific behaviors
    
    def _generate_text(self, inputs):
        """Nominative case: active text generation."""
        prompt = inputs.get('prompt', '')
        max_tokens = inputs.get('max_tokens', 100)
        
        # Simulated text generation
        generated_text = f"{prompt} [Generated text would appear here based on model parameters]"
        
        return {
            'generated_text': generated_text,
            'token_count': len(prompt.split()) + max_tokens,
            'stop_reason': 'max_tokens',
        }
    
    def _generate_language_artifacts(self, inputs):
        """Genitive case: generate language products."""
        artifact_type = inputs.get('type', 'summary')
        source_text = inputs.get('source', '')
        
        if artifact_type == 'summary':
            result = f"[Summary of '{source_text}' would appear here]"
        elif artifact_type == 'translation':
            target_language = inputs.get('target_language', 'French')
            result = f"[Translation of '{source_text}' to {target_language} would appear here]"
        elif artifact_type == 'analysis':
            result = f"[Semantic analysis of '{source_text}' would appear here]"
        
        return {
            'artifact_type': artifact_type,
            'result': result,
            'metadata': {
                'source_length': len(source_text.split()),
                'generated_at': '2025-04-07T12:34:56Z',
            }
        }
    
    def _provide_language_tools(self):
        """Instrumental case: provide language processing tools."""
        return {
            'available_tools': [
                {
                    'name': 'tokenizer',
                    'description': 'Split text into tokens',
                    'function': self._tokenize,
                    'parameters': ['text']
                },
                {
                    'name': 'sentiment_analyzer',
                    'description': 'Analyze sentiment of text',
                    'function': self._analyze_sentiment,
                    'parameters': ['text']
                },
                {
                    'name': 'entity_recognizer',
                    'description': 'Recognize named entities in text',
                    'function': self._recognize_entities,
                    'parameters': ['text', 'entity_types']
                }
            ],
            'usage_examples': {
                'tokenization': "model.tools['tokenizer']('Hello world')",
                'sentiment': "model.tools['sentiment_analyzer']('I love this product')",
            }
        }
    
    def _analyze_context(self, inputs):
        """Locative case: provide contextual analysis."""
        text = inputs.get('text', '')
        
        return {
            'context_analysis': {
                'topics': ['Topic 1', 'Topic 2'],  # Simulated topics
                'key_entities': ['Entity 1', 'Entity 2'],  # Simulated entities
                'time_references': ['2025', 'today'],  # Simulated time references
                'location_references': ['New York', 'Paris'],  # Simulated locations
            },
            'discourse_structure': {
                'paragraphs': len(text.split('\n\n')),
                'coherence_score': 0.85,  # Simulated coherence score
                'argumentative_structure': 'claim-evidence-conclusion',  # Simulated structure
            }
        }
    
    def update(self, prediction, observation):
        """Update model based on observations."""
        if self.current_case.abbreviation == 'ACC':
            # Accusative case: focus on being updated
            return self._fine_tune(prediction, observation)
        elif self.current_case.abbreviation == 'DAT':
            # Dative case: focus on receiving input
            return self._process_inputs(observation)
        # Other cases would have specific update behaviors
    
    def _fine_tune(self, prediction, observation):
        """Accusative case: parameter updates through fine-tuning."""
        expected_output = observation.get('expected_output', '')
        generated_output = prediction.get('generated_text', '')
        
        # Simulate fine-tuning update
        # In a real implementation, this would update the model weights
        loss = self._calculate_loss(generated_output, expected_output)
        
        # Update generation parameters based on loss
        self.parameters['temperature'] *= (1 - 0.01 * loss)
        self.parameters['top_p'] *= (1 - 0.005 * loss)
        
        return {
            'loss': loss,
            'updated_parameters': {
                'temperature': self.parameters['temperature'],
                'top_p': self.parameters['top_p'],
            },
            'training_progress': '23%',  # Simulated progress
        }
    
    def _process_inputs(self, observation):
        """Dative case: focus on processing incoming text data."""
        input_text = observation.get('input_text', '')
        
        # Process and categorize the input
        processed_tokens = self._tokenize(input_text)
        sentiment = self._analyze_sentiment(input_text)
        entities = self._recognize_entities(input_text)
        
        # Update internal context with processed input
        self.state['current_context'] = processed_tokens[-self.parameters['context_length']:]
        
        return {
            'processed_tokens': len(processed_tokens),
            'input_analysis': {
                'sentiment': sentiment,
                'entities': entities,
                'token_count': len(processed_tokens),
            },
            'context_updated': True,
        }
    
    def transform_case(self, target_case_abbr):
        """Transform the model to a different case."""
        # Find the target case
        target_case = next((case for case in self.supported_cases 
                           if case.abbreviation == target_case_abbr), None)
        
        if not target_case:
            return {'success': False, 'error': 'Unsupported case'}
            
        # Perform the transformation
        old_case = self.current_case
        self.current_case = target_case
        
        # Adjust internal state based on new case
        self._adjust_for_new_case()
        
        return {
            'success': True,
            'previous_case': old_case.abbreviation,
            'new_case': target_case.abbreviation,
        }
    
    def _adjust_for_new_case(self):
        """Adjust model behavior for the new case."""
        if self.current_case.abbreviation == 'NOM':
            # For nominative: optimize for generation quality
            self.state['generation_mode'] = 'beam'
        elif self.current_case.abbreviation == 'ACC':
            # For accusative: optimize for learning
            self.state['generation_mode'] = 'greedy'  # Simpler for learning
        elif self.current_case.abbreviation == 'GEN':
            # For genitive: optimize for product quality
            self.state['generation_mode'] = 'sampling'  # More diverse for creative outputs
        elif self.current_case.abbreviation == 'DAT':
            # For dative: optimize for input processing
            self.state['generation_mode'] = 'none'  # No generation, just input processing
        # Similarly for other cases
    
    # Helper methods (simplified implementations)
    def _tokenize(self, text):
        """Tokenize text into word tokens."""
        return text.split()
    
    def _analyze_sentiment(self, text):
        """Simple sentiment analysis."""
        positive_words = ['good', 'great', 'excellent', 'love']
        negative_words = ['bad', 'terrible', 'hate', 'dislike']
        
        tokens = self._tokenize(text.lower())
        positive_count = sum(1 for token in tokens if token in positive_words)
        negative_count = sum(1 for token in tokens if token in negative_words)
        
        if positive_count > negative_count:
            return 'positive'
        elif negative_count > positive_count:
            return 'negative'
        else:
            return 'neutral'
    
    def _recognize_entities(self, text, entity_types=None):
        """Simplified named entity recognition."""
        # This would be a complex NLP task in a real implementation
        # Here we just return a simplified simulation
        return [
            {'text': 'John', 'type': 'PERSON', 'position': text.find('John')},
            {'text': 'New York', 'type': 'LOCATION', 'position': text.find('New York')},
        ]
    
    def _calculate_loss(self, generated, expected):
        """Calculate loss between generated and expected outputs."""
        # Simplified loss calculation
        if not expected:
            return 0.0
        
        # Simulated loss based on length difference
        return abs(len(generated) - len(expected)) / max(len(expected), 1)
```

### 2.2 Using the Language Model

```python
from cerebrum import ModelRegistry

# Create and register the model
lm = LanguageModel('gpt_model', vocabulary_size=50000)
registry = ModelRegistry()
registry.register_model(lm, 'NOM')  # Register with nominative case

# Use in nominative case (text generator)
text_result = lm.predict({'prompt': 'Once upon a time'})
print(f"Generated text: {text_result['generated_text']}")

# Transform to genitive case (artifact generator)
lm.transform_case('GEN')
summary = lm.predict({
    'type': 'summary',
    'source': 'The quick brown fox jumps over the lazy dog.'
})
print(f"Generated summary: {summary['result']}")

# Transform to instrumental case (tool provider)
lm.transform_case('INS')
tools = lm.predict()
print(f"Available tools: {[tool['name'] for tool in tools['available_tools']]}")

# Transform to locative case (context analyzer)
lm.transform_case('LOC')
context = lm.predict({'text': 'Climate change is affecting global weather patterns.'})
print(f"Context analysis: {context['context_analysis']['topics']}")

# Transform to accusative case (model to be fine-tuned)
lm.transform_case('ACC')
fine_tuning = lm.update(
    {'generated_text': 'The weather tomorrow will be sunny.'},
    {'expected_output': 'The weather tomorrow will be rainy.'}
)
print(f"Fine-tuning loss: {fine_tuning['loss']}")
```

## 3. Intelligence Case Management Example

This example demonstrates how multiple CEREBRUM models can work together in an intelligence workflow.

```python
from cerebrum import ModelRegistry, Workflow, CaseManager

# Create registry and case manager
registry = ModelRegistry()
case_manager = CaseManager()

# Register intelligence models
data_collector = DataCollectionModel('field_collector')
analyzer = AnalyticalModel('pattern_analyzer')
report_generator = ReportModel('intelligence_reporter')
evaluation = EvaluationModel('quality_assessor')

# Register models with appropriate initial cases
registry.register_model(data_collector, 'INS')  # Instrumental: as a tool
registry.register_model(analyzer, 'NOM')        # Nominative: active processor
registry.register_model(report_generator, 'GEN') # Genitive: output generator
registry.register_model(evaluation, 'ACC')       # Accusative: receives updates

# Define intelligence workflow
workflow = Workflow('intelligence_cycle')

# Add workflow stages
workflow.add_stage({
    'name': 'data_collection',
    'models': [data_collector],
    'next': 'analysis',
})

workflow.add_stage({
    'name': 'analysis',
    'models': [analyzer],
    'next': 'reporting',
})

workflow.add_stage({
    'name': 'reporting',
    'models': [report_generator],
    'next': 'evaluation',
})

workflow.add_stage({
    'name': 'evaluation',
    'models': [evaluation],
    'next': 'refinement',
})

workflow.add_stage({
    'name': 'refinement',
    'models': [analyzer, report_generator],
    'next': 'deployment',
})

workflow.add_stage({
    'name': 'deployment',
    'models': [analyzer],
    'next': None,  # End of workflow
})

# Execute the workflow with initial input
result = workflow.execute({
    'raw_data': [
        {'source': 'field_agent_1', 'content': 'Observed activity at location Alpha'},
        {'source': 'field_agent_2', 'content': 'Confirmed movement at location Beta'},
        {'source': 'signals_intelligence', 'content': 'Intercepted communications between Alpha and Beta'},
    ],
    'priority': 'high',
    'timeframe': '24h',
})

print(f"Intelligence workflow completed with status: {result['status']}")
print(f"Generated intelligence report: {result['artifacts']['report']['title']}")
print(f"Quality assessment: {result['artifacts']['evaluation']['score']}/10")
```

These examples demonstrate how CEREBRUM models can take on different functional roles through case transformations while maintaining their core identity and purpose. 