# LLM Module Documentation

## Overview

The LLM module provides comprehensive integration with Large Language Model services, including OpenAI, Perplexity, and OpenRouter. It offers specialized researchers, analysis engines, and evaluation tools for AI-powered research, content generation, and strategic analysis.

## Architecture

```
LLM Module
├── Core Integrations
│   ├── OpenAI Integration - Direct OpenAI API access
│   ├── Perplexity Integration - Web-enhanced research
│   └── OpenRouter Integration - Multi-model access hub
├── Research Assistants
│   ├── Research Assistant - General research capabilities
│   ├── Web Researcher - Web-focused research
│   └── Strategic Analyzer - Business and strategic analysis
├── Analysis Engines
│   ├── OpenRouter Analysis Engine - Multi-model analysis
│   └── Product Evaluator - Product and team evaluation
├── Evaluation Systems
│   ├── Evaluation Results Manager - Results processing
│   └── Quality Assessment Tools
└── Utilities
    ├── LLM Utils - Common utilities and helpers
    └── Rate Limiting and Error Handling
```

## Key Features

### 🤖 Multi-Provider Support
- **OpenAI Integration**: Direct access to GPT models
- **Perplexity Integration**: Real-time web research with citations
- **OpenRouter Integration**: Access to 100+ AI models
- **Unified Interface**: Consistent API across all providers
- **Fallback Mechanisms**: Automatic provider switching

### 🔍 Research Capabilities
- **Web Research**: Real-time web search and analysis
- **Academic Research**: Citation-based research with sources
- **Multi-query Research**: Parallel research execution
- **Topic Deep Dives**: Comprehensive topic exploration
- **Source Validation**: Automatic source credibility checking

### 📊 Analysis Engines
- **Product Analysis**: Tech-product feasibility assessment
- **Market Analysis**: Product-sector value evaluation
- **Team Evaluation**: Team capability assessment
- **Strategic Analysis**: Business strategy evaluation
- **Competitive Analysis**: Market positioning analysis

### ⚡ Performance Optimization
- **Rate Limiting**: Intelligent request throttling
- **Caching**: Response caching for efficiency
- **Retry Logic**: Robust error recovery
- **Circuit Breakers**: Fault tolerance mechanisms
- **Performance Monitoring**: Detailed metrics tracking

### 🎯 Quality Assessment
- **Response Validation**: Automated quality checking
- **Citation Verification**: Source validation and scoring
- **Confidence Scoring**: Response confidence metrics
- **Bias Detection**: Content bias analysis
- **Factual Accuracy**: Fact-checking capabilities

## Module Structure

### Core Files
- **`openai_researcher.py`** - OpenAI API integration and research
- **`perplexity_researcher.py`** - Perplexity-powered web research
- **`openrouter_analysis_engine.py`** - Multi-model analysis framework
- **`research_assistant.py`** - General-purpose research assistant
- **`product_evaluator.py`** - Product and team evaluation engine
- **`strategic_analyzer.py`** - Strategic analysis and planning
- **`web_researcher.py`** - Web-focused research capabilities
- **`evaluation_results_manager.py`** - Results processing and reporting
- **`llm_utils.py`** - Common utilities and helpers

### OpenRouter Submodule
- **`OpenRouter/openrouter.py`** - Advanced OpenRouter client with circuit breakers

## Quick Start

### Basic Research

```python
from llm.perplexity_researcher import PerplexityResearcher
from llm.openai_researcher import OpenAIResearcher

# Initialize researchers
perplexity = PerplexityResearcher()
openai = OpenAIResearcher()

# Perform web research with citations
web_results = await perplexity.search(
    "latest developments in quantum computing 2024",
    model="sonar-large",
    return_citations=True,
    recency_filter="month"
)

print(f"Research Results: {web_results['answer']}")
print(f"Citations: {len(web_results['citations'])}")

# Perform general AI research
ai_results = await openai.search(
    "Explain transformer architecture improvements",
    model="gpt-4",
    temperature=0.2
)

print(f"AI Analysis: {ai_results['answer']}")
```

### OpenRouter Multi-Model Analysis

```python
from src.llm.OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig
from src.llm.config import get_model_name, get_model_config

# Configure client with environment-based config
config = OpenRouterConfig(
    default_model=get_model_name(),  # Uses config.py settings
    temperature=get_model_config()["temperature"],
    max_tokens=get_model_config()["max_tokens"]
)

client = OpenRouterClient(config)

# Simple chat with configured model
response = client.simple_chat(
    "Analyze the current state of AI safety research"
)

# Conversation management
conversation = client.create_conversation()
conversation.add_system_message("You are an expert AI researcher")

response = conversation.send_message(
    "What are the most promising approaches to AI alignment?"
)

# Get available models
models = client.get_available_models_from_api()
print(f"Available models: {len(models)}")

# Document analysis
keywords = client.get_document_keywords("AI safety research content...")
overview = client.get_document_overview("Long document content...")
quality = client.get_content_quality_assessment("Content to assess...")
```

### Product and Team Evaluation

```python
from llm.product_evaluator import ProductEvaluator

# Initialize evaluator
evaluator = ProductEvaluator()

# Load team members and products
team_members = evaluator.load_team_members([
    "Dr. Jane Smith", "John Doe", "Alice Johnson"
])

products = evaluator.load_products()

# Evaluate team for specific product
evaluation = await evaluator.evaluate_team_for_product(
    team_members=team_members,
    product=products[0]
)

print(f"Team Evaluation Score: {evaluation.get('overall_score', 'N/A')}")
print(f"Strengths: {evaluation.get('strengths', [])}")
print(f"Gaps: {evaluation.get('gaps', [])}")

# Evaluate audience response
audiences = evaluator.load_audience_entities()
audience_eval = await evaluator.evaluate_audience_response(
    audience=audiences[0],
    product=products[0]
)

print(f"Market Fit Score: {audience_eval.get('market_fit_score', 'N/A')}")
```

### Strategic Analysis

```python
from llm.strategic_analyzer import StrategicAnalyzer

# Initialize strategic analyzer
analyzer = StrategicAnalyzer()

# Analyze market positioning
market_analysis = await analyzer.analyze_market_positioning(
    company_data={
        'name': 'TechStartup Inc.',
        'products': ['AI Platform', 'Data Analytics'],
        'target_market': 'Enterprise AI'
    },
    competitors=['Competitor A', 'Competitor B'],
    market_trends=['AI adoption', 'Cloud migration']
)

print(f"Market Position: {market_analysis.get('position', 'Unknown')}")
print(f"Recommendations: {market_analysis.get('recommendations', [])}")

# SWOT Analysis
swot = await analyzer.perform_swot_analysis(
    organization_data={
        'strengths': ['Strong AI team', 'Innovative products'],
        'weaknesses': ['Limited market reach'],
        'opportunities': ['Growing AI market'],
        'threats': ['Established competitors']
    }
)

print(f"SWOT Analysis: {swot.get('summary', 'No summary')}")
```

## Advanced Usage

### Custom Research Pipeline

```python
from llm.research_assistant import ResearchAssistant
from llm.llm_utils import LLMUtils
from typing import List, Dict, Any

class CustomResearchPipeline:
    """Custom research pipeline with multi-stage analysis."""
    
    def __init__(self):
        self.research_assistant = ResearchAssistant()
        self.llm_utils = LLMUtils()
        
    async def comprehensive_research(
        self, 
        topic: str, 
        depth: str = "deep"
    ) -> Dict[str, Any]:
        """Perform comprehensive multi-stage research."""
        
        # Stage 1: Initial research
        initial_results = await self.research_assistant.research_topic(
            query=f"Overview of {topic}",
            model="sonar-pro",
            search_recency_filter="month"
        )
        
        # Stage 2: Deep dive into specific aspects
        if depth == "deep":
            # Extract key aspects from initial research
            aspects = self._extract_key_aspects(initial_results['summary'])
            
            detailed_results = []
            for aspect in aspects:
                detail_result = await self.research_assistant.research_topic(
                    query=f"{topic}: {aspect} detailed analysis",
                    model="sonar-pro"
                )
                detailed_results.append(detail_result)
        
        # Stage 3: Synthesis and validation
        synthesis = await self._synthesize_results(
            initial_results, 
            detailed_results if depth == "deep" else []
        )
        
        # Stage 4: Quality assessment
        quality_score = self._assess_research_quality(synthesis)
        
        return {
            'topic': topic,
            'initial_research': initial_results,
            'detailed_research': detailed_results if depth == "deep" else [],
            'synthesis': synthesis,
            'quality_score': quality_score,
            'total_sources': len(initial_results.get('citations', [])),
            'confidence': synthesis.get('confidence', 0.0)
        }
    
    def _extract_key_aspects(self, summary: str) -> List[str]:
        """Extract key aspects for deep dive research."""
        # Use LLM to identify key aspects
        prompt = f"""
        From this research summary, identify 3-5 key aspects that warrant deeper investigation:
        
        {summary}
        
        Return only a list of specific aspects, one per line.
        """
        
        # This would use an LLM to extract aspects
        aspects = ["Technical implementation", "Market applications", "Future trends"]
        return aspects
    
    async def _synthesize_results(
        self, 
        initial: Dict[str, Any], 
        detailed: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """Synthesize research results into coherent analysis."""
        
        all_content = [initial['summary']]
        all_citations = initial.get('citations', [])
        
        for detail in detailed:
            all_content.append(detail['summary'])
            all_citations.extend(detail.get('citations', []))
        
        # Synthesize using LLM
        synthesis_prompt = f"""
        Synthesize the following research findings into a comprehensive analysis:
        
        {' '.join(all_content)}
        
        Provide:
        1. Executive summary
        2. Key findings
        3. Implications
        4. Confidence assessment
        """
        
        # This would use an LLM for synthesis
        return {
            'executive_summary': "Synthesized summary...",
            'key_findings': ["Finding 1", "Finding 2"],
            'implications': ["Implication 1", "Implication 2"],
            'confidence': 0.85,
            'total_citations': len(set(all_citations))
        }
    
    def _assess_research_quality(self, synthesis: Dict[str, Any]) -> float:
        """Assess the quality of research results."""
        quality_factors = {
            'citation_count': min(synthesis.get('total_citations', 0) / 10, 1.0),
            'confidence': synthesis.get('confidence', 0.0),
            'completeness': 1.0 if synthesis.get('key_findings') else 0.5
        }
        
        return sum(quality_factors.values()) / len(quality_factors)

# Usage
pipeline = CustomResearchPipeline()
results = await pipeline.comprehensive_research(
    "Quantum computing applications in machine learning",
    depth="deep"
)
```

### Batch Analysis Engine

```python
from llm.openrouter_analysis_engine import OpenRouterAnalysisEngine
from pathlib import Path
import asyncio

class BatchAnalysisEngine:
    """Batch analysis engine for processing multiple documents."""
    
    def __init__(self, config):
        self.config = config
        
    async def analyze_tech_product_portfolio(
        self, 
        tech_dir: Path, 
        product_dir: Path
    ) -> Dict[str, Any]:
        """Analyze multiple tech-product combinations."""
        
        tech_files = list(tech_dir.glob("*.md"))
        product_files = list(product_dir.glob("*.md"))
        
        analyses = []
        
        # Create analysis engines for parallel processing
        engines = [
            OpenRouterAnalysisEngine(self.config, "tech_product_analysis")
            for _ in range(min(5, len(tech_files) * len(product_files)))
        ]
        
        # Prepare analysis tasks
        tasks = []
        for tech_file in tech_files:
            for product_file in product_files:
                task = self._analyze_tech_product_pair(
                    engines[len(tasks) % len(engines)],
                    tech_file,
                    product_file
                )
                tasks.append(task)
        
        # Execute analyses in parallel
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Process results
        successful_analyses = [r for r in results if not isinstance(r, Exception)]
        failed_analyses = [r for r in results if isinstance(r, Exception)]
        
        # Generate portfolio summary
        portfolio_summary = self._generate_portfolio_summary(successful_analyses)
        
        return {
            'total_combinations': len(tech_files) * len(product_files),
            'successful_analyses': len(successful_analyses),
            'failed_analyses': len(failed_analyses),
            'analyses': successful_analyses,
            'portfolio_summary': portfolio_summary,
            'errors': [str(e) for e in failed_analyses]
        }
    
    async def _analyze_tech_product_pair(
        self,
        engine: OpenRouterAnalysisEngine,
        tech_file: Path,
        product_file: Path
    ) -> Dict[str, Any]:
        """Analyze a single tech-product pair."""
        
        tech_content = engine.load_file_content(tech_file)
        product_content = engine.load_file_content(product_file)
        
        analysis = engine.analyze_tech_product_feasibility(
            tech_content=tech_content,
            product_content=product_content,
            tech_name=tech_file.stem,
            product_name=product_file.stem
        )
        
        return {
            'tech_name': tech_file.stem,
            'product_name': product_file.stem,
            'analysis': analysis,
            'timestamp': datetime.now().isoformat()
        }
    
    def _generate_portfolio_summary(
        self, 
        analyses: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """Generate summary of portfolio analysis."""
        
        if not analyses:
            return {'message': 'No successful analyses'}
        
        # Extract scores and metrics
        feasibility_scores = [
            a['analysis'].get('feasibility_score', 0) 
            for a in analyses
        ]
        
        market_potential_scores = [
            a['analysis'].get('market_potential', 0)
            for a in analyses
        ]
        
        # Find top combinations
        top_combinations = sorted(
            analyses,
            key=lambda x: x['analysis'].get('overall_score', 0),
            reverse=True
        )[:5]
        
        return {
            'average_feasibility': sum(feasibility_scores) / len(feasibility_scores),
            'average_market_potential': sum(market_potential_scores) / len(market_potential_scores),
            'top_combinations': [
                {
                    'tech': combo['tech_name'],
                    'product': combo['product_name'],
                    'score': combo['analysis'].get('overall_score', 0)
                }
                for combo in top_combinations
            ],
            'total_analyses': len(analyses)
        }

# Usage
batch_engine = BatchAnalysisEngine(config)
portfolio_results = await batch_engine.analyze_tech_product_portfolio(
    Path("docs/tech/"),
    Path("docs/products/")
)
```

### Error Handling and Retry Logic

```python
from llm.llm_utils import LLMUtils
import asyncio
from typing import Optional

class RobustLLMClient:
    """LLM client with comprehensive error handling."""
    
    def __init__(self):
        self.llm_utils = LLMUtils()
        self.max_retries = 3
        self.base_delay = 1.0
        
    async def robust_api_call(
        self,
        api_function,
        *args,
        **kwargs
    ) -> Optional[Dict[str, Any]]:
        """Make API call with robust error handling."""
        
        last_exception = None
        
        for attempt in range(self.max_retries):
            try:
                # Add jitter to delay
                if attempt > 0:
                    delay = self.base_delay * (2 ** attempt) + random.uniform(0, 1)
                    await asyncio.sleep(delay)
                
                # Make API call
                result = await api_function(*args, **kwargs)
                
                # Validate response
                validated_result = self.llm_utils.validate_api_response(result)
                
                return validated_result
                
            except RateLimitError as e:
                last_exception = e
                print(f"Rate limit exceeded (attempt {attempt + 1})")
                # Exponential backoff for rate limits
                await asyncio.sleep(60 * (2 ** attempt))
                
            except NetworkError as e:
                last_exception = e
                print(f"Network error (attempt {attempt + 1}): {e}")
                
            except ValidationError as e:
                last_exception = e
                print(f"Response validation failed (attempt {attempt + 1}): {e}")
                
            except Exception as e:
                last_exception = e
                print(f"Unexpected error (attempt {attempt + 1}): {e}")
        
        # All retries failed
        print(f"All {self.max_retries} attempts failed. Last error: {last_exception}")
        return None

# Usage
robust_client = RobustLLMClient()

async def safe_research(query: str) -> Optional[Dict[str, Any]]:
    """Safely perform research with error handling."""
    
    researcher = PerplexityResearcher()
    
    result = await robust_client.robust_api_call(
        researcher.search,
        query=query,
        model="sonar-large",
        return_citations=True
    )
    
    return result
```

## Configuration

### API Configuration

```python
# Environment variables
OPENAI_API_KEY=your_openai_key
PERPLEXITY_API_KEY=your_perplexity_key
OPENROUTER_API_KEY=your_openrouter_key

# Rate limiting configuration
OPENAI_RATE_LIMIT=100  # requests per minute
PERPLEXITY_RATE_LIMIT=60
OPENROUTER_RATE_LIMIT=200

# Default models
DEFAULT_OPENAI_MODEL=gpt-4
DEFAULT_PERPLEXITY_MODEL=sonar-large
DEFAULT_OPENROUTER_MODEL=anthropic/claude-3-haiku
```

### Client Configuration

```python
from llm.OpenRouter.openrouter import OpenRouterConfig, RetryConfig, CircuitBreakerConfig

# Configure retry behavior
retry_config = RetryConfig(
    max_retries=3,
    base_delay=1.0,
    max_delay=60.0,
    exponential_base=2.0,
    jitter=True
)

# Configure circuit breaker
circuit_config = CircuitBreakerConfig(
    failure_threshold=5,
    recovery_timeout=60,
    half_open_max_calls=3
)

# Main configuration
config = OpenRouterConfig(
    api_key="your-api-key",
    default_model="openai/gpt-4",
    temperature=0.7,
    max_tokens=1000,
    retry_config=retry_config,
    circuit_breaker_config=circuit_config,
    request_timeout=90.0,
    enable_logging=True
)
```

## Testing

The LLM module includes comprehensive testing with proper organization and output management.

### Test Structure

```
src/llm/
├── test_openrouter_functionality.py    # Full API functionality tests
├── test_openrouter_structure.py        # Code structure validation
├── openrouter_example.py               # Usage examples and demos
└── test_output/                        # Test results directory
    ├── openrouter_test_results.json
    └── openrouter_structure_test_results.json
```

### Running Tests

```bash
# Functionality tests (requires API key)
cd src/llm
python3 test_openrouter_functionality.py

# Structure tests (no API key required)
python3 test_openrouter_structure.py

# Run example script
python3 openrouter_example.py
```

### Test Coverage

**Functionality Tests (`test_openrouter_functionality.py`):**
- ✅ Client initialization and configuration
- ✅ Chat completion (synchronous and asynchronous)
- ✅ Streaming responses (sync and async)
- ✅ Conversation management with history
- ✅ Document analysis methods (keywords, overview, quality assessment)
- ✅ Model validation and availability checking
- ✅ Error handling and retry logic
- ✅ Performance monitoring and statistics
- ✅ Flexible prompt composition
- ✅ Analysis engine integration

**Structure Tests (`test_openrouter_structure.py`):**
- ✅ Import validation and module structure
- ✅ Class definitions and method signatures
- ✅ Configuration options and defaults
- ✅ Error handling structure
- ✅ Analysis engine integration
- ✅ Quick chat function validation

### Test Output Organization

All test results are automatically saved to the `test_output/` directory:

- **Functionality Test Results**: `test_output/openrouter_test_results.json`
  - Test execution summary
  - Performance metrics
  - Error details and timestamps
  - Success/failure statistics

- **Structure Test Results**: `test_output/openrouter_structure_test_results.json`
  - Code structure validation results
  - Import and class definition status
  - Configuration validation

### Environment Setup for Testing

1. **API Key Configuration:**
   ```bash
   # Create .env file in project root
   echo "OPENROUTER_API_KEY=your_api_key_here" > .env
   ```

2. **Dependencies:**
   ```bash
   pip install python-dotenv openai
   ```

3. **Test Execution:**
   ```bash
   # Run all tests
   python3 test_openrouter_functionality.py && python3 test_openrouter_structure.py
   ```

### Continuous Integration

The test suite is designed for CI/CD integration:

```yaml
# Example GitHub Actions workflow
- name: Run OpenRouter Tests
  run: |
    cd src/llm
    python3 test_openrouter_structure.py
    python3 test_openrouter_functionality.py
  env:
    OPENROUTER_API_KEY: ${{ secrets.OPENROUTER_API_KEY }}
```

## Best Practices

### API Usage
1. Implement proper rate limiting
2. Use appropriate models for each task
3. Monitor token usage and costs
4. Implement fallback mechanisms
5. Cache responses when appropriate

### Research Quality
1. Validate sources and citations
2. Cross-reference information
3. Use recent data for time-sensitive topics
4. Implement bias detection
5. Provide confidence scores

### Error Handling
1. Implement exponential backoff
2. Use circuit breakers for fault tolerance
3. Provide meaningful error messages
4. Log all API interactions
5. Implement graceful degradation

### Performance Optimization
1. Use batch processing for multiple requests
2. Implement intelligent caching
3. Monitor response times
4. Optimize prompt engineering
5. Use streaming for long responses

### Security
1. Secure API key management
2. Validate all inputs and outputs
3. Implement access controls
4. Monitor for unusual usage patterns
5. Regular security audits

## API Reference

For detailed API documentation, see:
- [OpenAI Researcher API](openai_researcher.md)
- [Perplexity Researcher API](perplexity_researcher.md)
- [OpenRouter Client API](OpenRouter/README.md)
- [Product Evaluator API](product_evaluator.md)
- [Strategic Analyzer API](strategic_analyzer.md)
- [LLM Utils API](llm_utils.md)

## Contributing

When contributing to the LLM module:
1. Follow established patterns for API integration
2. Add comprehensive error handling
3. Include rate limiting and retry logic
4. Update documentation for new features
5. Consider cost implications of new functionality
