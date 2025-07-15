"""
OpenRouter Analysis Engine

Core analysis engine for Tech-Product feasibility and Product-Sector value assessments
using OpenRouter LLM capabilities. Provides structured analysis workflows with 
comprehensive error handling, logging, and output management.
"""

import os
import json
import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass

# Local imports - using basic OpenRouter client only
from .OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig


@dataclass
class AnalysisResult:
    """Standard structure for analysis results."""
    analysis_type: str
    timestamp: str
    source_files: List[str]
    target_files: List[str]
    results: Dict[str, Any]
    metrics: Dict[str, float]
    recommendations: List[str]
    errors: List[str]


class OpenRouterAnalysisEngine:
    """Core engine for OpenRouter-based content analysis."""
    
    def __init__(self, config, analysis_type: str):
        """Initialize analysis engine with configuration."""
        self.config = config
        self.analysis_type = analysis_type
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Initialize OpenRouter client
        self._setup_openrouter_client()
        
        # Setup logging and output directories
        self._setup_logging_and_output()
        
        # Initialize result tracking
        self.results = []
        self.errors = []
        self.performance_metrics = {
            "total_llm_calls": 0,
            "successful_calls": 0,
            "failed_calls": 0,
            "analysis_start_time": datetime.now(),
            "analysis_end_time": None
        }
    
    def _setup_openrouter_client(self):
        """Initialize OpenRouter client."""
        try:
            # Use basic OpenRouter client
            basic_config = OpenRouterConfig(
                api_key=self.config.api_key,
                default_model=self.config.model,
                temperature=self.config.temperature,
                max_tokens=self.config.max_tokens
            )
            self.client = OpenRouterClient(basic_config)
            
        except Exception as e:
            print(f"❌ Failed to initialize OpenRouter client: {e}")
            raise
    
    def _setup_logging_and_output(self):
        """Setup logging and create output directories."""
        # Create timestamped output directory
        self.output_dir = Path("examples/output") / f"{self.analysis_type}_{self.timestamp}"
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Setup logging
        log_dir = self.output_dir / "logs"
        log_dir.mkdir(exist_ok=True)
        
        self.logger = logging.getLogger(f"OpenRouterAnalysis.{self.analysis_type}")
        self.logger.setLevel(logging.INFO)
        
        # Clear any existing handlers
        if self.logger.handlers:
            self.logger.handlers.clear()
        
        # File handler
        file_handler = logging.FileHandler(log_dir / f"{self.analysis_type}.log")
        file_handler.setFormatter(
            logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        )
        self.logger.addHandler(file_handler)
        
        # Error file handler
        error_handler = logging.FileHandler(log_dir / f"{self.analysis_type}_errors.log")
        error_handler.setLevel(logging.ERROR)
        error_handler.setFormatter(
            logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        )
        self.logger.addHandler(error_handler)
        
        self.logger.info(f"Initialized analysis engine for {self.analysis_type}")
    
    def load_file_content(self, file_path: Path) -> str:
        """Load and return file content with error handling."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            self.logger.info(f"Loaded file: {file_path}")
            return content
        except Exception as e:
            error_msg = f"Failed to load file {file_path}: {str(e)}"
            self.logger.error(error_msg)
            self.errors.append(error_msg)
            return ""
    
    def make_llm_call(self, prompt: str, context: str = "") -> Optional[str]:
        """Make OpenRouter LLM call with error handling."""
        try:
            self.performance_metrics["total_llm_calls"] += 1
            
            # Construct full prompt with context
            full_prompt = f"{context}\n\n{prompt}" if context else prompt
            
            # Make the LLM call using simple_chat
            response = self.client.simple_chat(
                message=full_prompt,
                model=self.config.model
            )
            
            self.performance_metrics["successful_calls"] += 1
            self.logger.info(f"Successful LLM call - Response length: {len(response) if response else 0}")
            return response
            
        except Exception as e:
            self.performance_metrics["failed_calls"] += 1
            error_msg = f"LLM call failed: {str(e)}"
            self.logger.error(error_msg)
            self.errors.append(error_msg)
            return None
    
    def analyze_tech_product_feasibility(self, tech_content: str, product_content: str, 
                                       tech_name: str, product_name: str) -> Dict[str, Any]:
        """Analyze technical feasibility of product given technology context."""
        
        context = f"""
        TECHNOLOGY CONTEXT:
        {tech_content}
        
        PRODUCT CONTEXT:
        {product_content}
        """
        
        prompt = f"""
        As an expert technology analyst, assess the feasibility of developing the product "{product_name}" 
        given the technology capabilities described in "{tech_name}".
        
        Please provide a comprehensive analysis covering:
        
        1. TECHNICAL FEASIBILITY (1-10 scale with detailed reasoning)
           - How well does the technology support the product requirements?
           - What are the technical gaps or challenges?
           - What additional technologies might be needed?
        
        2. DEVELOPMENT READINESS (1-10 scale with detailed reasoning)
           - How mature is the underlying technology?
           - What development work is required?
           - Timeline estimates for product development
        
        3. INTEGRATION COMPLEXITY (1-10 scale with detailed reasoning)
           - How complex would it be to integrate this technology into the product?
           - What are the main integration challenges?
           - Required expertise and resources
        
        4. MARKET READINESS (1-10 scale with detailed reasoning)
           - Is the technology ready for commercial deployment?
           - What are the regulatory or compliance considerations?
           - Competitive advantages and disadvantages
        
        5. STRATEGIC RECOMMENDATIONS
           - Key next steps for product development
           - Risk mitigation strategies
           - Investment priorities
        
        Provide specific, actionable insights based on the technical capabilities and product requirements.
        Format your response with clear sections and numerical scores where requested.
        """
        
        response = self.make_llm_call(prompt, context)
        
        if response:
            return {
                "tech_name": tech_name,
                "product_name": product_name,
                "analysis": response,
                "analysis_timestamp": datetime.now().isoformat()
            }
        else:
            return {
                "tech_name": tech_name,
                "product_name": product_name,
                "analysis": "Analysis failed due to LLM call error",
                "error": True,
                "analysis_timestamp": datetime.now().isoformat()
            }
    
    def analyze_product_sector_value(self, product_content: str, sector_content: str,
                                   product_name: str, sector_name: str) -> Dict[str, Any]:
        """Analyze value proposition of product for specific sector."""
        
        context = f"""
        PRODUCT CONTEXT:
        {product_content}
        
        SECTOR CONTEXT:
        {sector_content}
        """
        
        prompt = f"""
        As an expert business analyst, assess the value proposition and market fit of the product 
        "{product_name}" for the "{sector_name}" sector.
        
        Please provide a comprehensive analysis covering:
        
        1. MARKET FIT (1-10 scale with detailed reasoning)
           - How well does the product address sector needs?
           - What specific pain points does it solve?
           - Target customer segments within the sector
        
        2. VALUE PROPOSITION (1-10 scale with detailed reasoning)
           - What unique value does the product provide?
           - How does it compare to existing solutions?
           - Quantifiable benefits for sector customers
        
        3. COMPETITIVE ADVANTAGE (1-10 scale with detailed reasoning)
           - What differentiates this product in the sector?
           - Barriers to entry for competitors
           - Sustainable competitive advantages
        
        4. REVENUE POTENTIAL (1-10 scale with detailed reasoning)
           - Market size and growth potential in this sector
           - Pricing strategies and revenue models
           - Customer acquisition and retention prospects
        
        5. STRATEGIC ALIGNMENT (1-10 scale with detailed reasoning)
           - How well does this align with sector trends?
           - Regulatory and compliance considerations
           - Partnership and ecosystem opportunities
        
        6. STRATEGIC RECOMMENDATIONS
           - Go-to-market strategies for this sector
           - Product positioning and messaging
           - Key partnerships and channel strategies
           - Investment and resource allocation priorities
        
        Provide specific, actionable insights based on sector characteristics and product capabilities.
        Format your response with clear sections and numerical scores where requested.
        """
        
        response = self.make_llm_call(prompt, context)
        
        if response:
            return {
                "product_name": product_name,
                "sector_name": sector_name,
                "analysis": response,
                "analysis_timestamp": datetime.now().isoformat()
            }
        else:
            return {
                "product_name": product_name, 
                "sector_name": sector_name,
                "analysis": "Analysis failed due to LLM call error",
                "error": True,
                "analysis_timestamp": datetime.now().isoformat()
            }
    
    def analyze_product_explanation(self, product_content: str, entity_content: str,
                                  product_name: str, entity_name: str) -> Dict[str, Any]:
        """Generate tailored explanation of product for specific audience/entity."""
        
        context = f"""
        PRODUCT CONTEXT:
        {product_content}
        
        AUDIENCE/ENTITY CONTEXT:
        {entity_content}
        """
        
        prompt = f"""
        As an expert communication strategist, create a compelling and tailored explanation of the product 
        "{product_name}" specifically for the audience represented by "{entity_name}".
        
        Please provide a comprehensive explanation covering:
        
        1. AUDIENCE ALIGNMENT (1-10 scale with detailed reasoning)
           - How well does this product align with the audience's interests and expertise?
           - What aspects would resonate most with their background and experience?
           - Connection points between the product and the audience's known work/interests
        
        2. VALUE RELEVANCE (1-10 scale with detailed reasoning)
           - What specific value would this product provide to this audience?
           - How does it solve problems or create opportunities relevant to them?
           - Personal or professional benefits that would matter most
        
        3. TECHNICAL APPROPRIATENESS (1-10 scale with detailed reasoning)
           - What level of technical detail is appropriate for this audience?
           - How should complex concepts be presented or simplified?
           - What technical background can we assume they have?
        
        4. ENGAGEMENT POTENTIAL (1-10 scale with detailed reasoning)
           - How likely is this audience to be interested and engaged?
           - What aspects would capture their attention most effectively?
           - Potential objections or skepticism to address
        
        5. CUSTOMIZED EXPLANATION
           Create a compelling, audience-specific explanation that:
           - Uses language and terminology appropriate for their background
           - Emphasizes benefits most relevant to their interests and expertise
           - Connects to their known work, projects, or areas of focus
           - Addresses likely questions or concerns they might have
           - Suggests specific ways they might engage with or benefit from the product
        
        6. STRATEGIC RECOMMENDATIONS
           - Best approaches for presenting this product to this audience
           - Key messaging points to emphasize
           - Potential collaboration or partnership opportunities
           - Follow-up strategies for deeper engagement
        
        Provide specific, actionable insights based on the audience's background and the product's capabilities.
        Format your response with clear sections and numerical scores where requested.
        """
        
        response = self.make_llm_call(prompt, context)
        
        if response:
            return {
                "product_name": product_name,
                "entity_name": entity_name,
                "analysis": response,
                "analysis_timestamp": datetime.now().isoformat()
            }
        else:
            return {
                "product_name": product_name,
                "entity_name": entity_name,
                "analysis": "Analysis failed due to LLM call error",
                "error": True,
                "analysis_timestamp": datetime.now().isoformat()
            }
    
    def save_results(self, results: List[Dict[str, Any]], filename: str):
        """Save analysis results to JSON and markdown files."""
        try:
            # Save JSON results
            json_path = self.output_dir / f"{filename}.json"
            with open(json_path, 'w', encoding='utf-8') as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            
            # Create reports directory
            reports_dir = self.output_dir / "reports"
            reports_dir.mkdir(exist_ok=True)
            
            # Save individual markdown reports
            for result in results:
                if not result.get('error', False):
                    if self.analysis_type == "tech_product_analysis":
                        report_name = f"{result['tech_name']}_{result['product_name']}_analysis.md"
                    elif self.analysis_type == "product_explanation_analysis":
                        report_name = f"{result['product_name']}_{result['entity_name']}_explanation.md"
                    else:  # product_sector_analysis
                        report_name = f"{result['product_name']}_{result['sector_name']}_analysis.md"
                    
                    report_path = reports_dir / report_name.replace(' ', '_').replace('/', '_')
                    
                    with open(report_path, 'w', encoding='utf-8') as f:
                        if self.analysis_type == "product_explanation_analysis":
                            f.write(f"# Product Explanation Report\n\n")
                        else:
                            f.write(f"# Analysis Report\n\n")
                        f.write(f"**Generated**: {result['analysis_timestamp']}\n\n")
                        if self.analysis_type == "tech_product_analysis":
                            f.write(f"**Technology**: {result['tech_name']}\n")
                            f.write(f"**Product**: {result['product_name']}\n\n")
                        elif self.analysis_type == "product_explanation_analysis":
                            f.write(f"**Product**: {result['product_name']}\n")
                            f.write(f"**Audience**: {result['entity_name']}\n\n")
                        else:
                            f.write(f"**Product**: {result['product_name']}\n")
                            f.write(f"**Sector**: {result['sector_name']}\n\n")
                        f.write(f"{result['analysis']}\n")
            
            self.logger.info(f"Results saved to {json_path}")
            
        except Exception as e:
            error_msg = f"Failed to save results: {str(e)}"
            self.logger.error(error_msg)
            self.errors.append(error_msg)
    
    def save_performance_metrics(self):
        """Save performance metrics and session summary."""
        try:
            self.performance_metrics["analysis_end_time"] = datetime.now()
            
            # Calculate duration
            duration = (self.performance_metrics["analysis_end_time"] - 
                       self.performance_metrics["analysis_start_time"]).total_seconds()
            self.performance_metrics["total_duration_seconds"] = duration
            
            # Calculate success rate
            total_calls = self.performance_metrics["total_llm_calls"]
            if total_calls > 0:
                success_rate = (self.performance_metrics["successful_calls"] / total_calls) * 100
                self.performance_metrics["success_rate"] = success_rate
            
            # Convert datetime objects to strings for JSON serialization
            metrics_for_json = self.performance_metrics.copy()
            metrics_for_json["analysis_start_time"] = self.performance_metrics["analysis_start_time"].isoformat()
            metrics_for_json["analysis_end_time"] = self.performance_metrics["analysis_end_time"].isoformat()
            
            # Save metrics
            metrics_path = self.output_dir / "performance_metrics.json"
            with open(metrics_path, 'w', encoding='utf-8') as f:
                json.dump(metrics_for_json, f, indent=2, ensure_ascii=False)
            
            # Save session summary
            summary = {
                "analysis_type": self.analysis_type,
                "timestamp": self.timestamp,
                "total_results": len(self.results),
                "total_errors": len(self.errors),
                "performance_metrics": metrics_for_json,  # Use the converted version
                "errors": self.errors
            }
            
            summary_path = self.output_dir / "session_summary.json"
            with open(summary_path, 'w', encoding='utf-8') as f:
                json.dump(summary, f, indent=2, ensure_ascii=False)
            
            self.logger.info(f"Performance metrics saved to {metrics_path}")
            
        except Exception as e:
            self.logger.error(f"Failed to save performance metrics: {str(e)}")
    
    def generate_error_report(self):
        """Generate comprehensive error report if there were failures."""
        if self.errors:
            try:
                reports_dir = self.output_dir / "reports"
                reports_dir.mkdir(exist_ok=True)
                
                error_report = {
                    "analysis_type": self.analysis_type,
                    "timestamp": self.timestamp,
                    "total_errors": len(self.errors),
                    "errors": self.errors,
                    "performance_impact": {
                        "total_calls": self.performance_metrics["total_llm_calls"],
                        "failed_calls": self.performance_metrics["failed_calls"],
                        "failure_rate": (self.performance_metrics["failed_calls"] / 
                                       max(self.performance_metrics["total_llm_calls"], 1)) * 100
                    }
                }
                
                # Save JSON error report
                error_json_path = self.output_dir / "error_report.json"
                with open(error_json_path, 'w', encoding='utf-8') as f:
                    json.dump(error_report, f, indent=2)
                
                # Save markdown error report
                error_md_path = reports_dir / "error_report.md"
                with open(error_md_path, 'w', encoding='utf-8') as f:
                    f.write(f"# Error Report - {self.analysis_type}\n\n")
                    f.write(f"**Generated**: {self.timestamp}\n")
                    f.write(f"**Total Errors**: {len(self.errors)}\n\n")
                    f.write("## Errors\n\n")
                    for i, error in enumerate(self.errors, 1):
                        f.write(f"{i}. {error}\n")
                
                self.logger.info(f"Error report generated: {error_md_path}")
                
            except Exception as e:
                self.logger.error(f"Failed to generate error report: {str(e)}") 