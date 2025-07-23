#!/usr/bin/env python3
"""
CEREBRUM Diagram Enhancement Tool

This script automatically enhances existing Mermaid diagrams with modern styling,
interactive features, and CEREBRUM case annotations. It can also generate new
diagram types based on templates.

Usage:
    python3 tools/enhance_diagrams.py --input docs/diagrams --output docs/diagrams/enhanced
    python3 tools/enhance_diagrams.py --generate-new --output docs/diagrams/new
    python3 tools/enhance_diagrams.py --apply-templates --template cognitive
"""

import argparse
import os
import re
import shutil
import yaml
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class DiagramEnhancer:
    """Enhanced diagram processing and generation tool."""
    
    def __init__(self, config_path: str = "docs/diagrams/config.yaml"):
        """Initialize the diagram enhancer with configuration."""
        self.config_path = config_path
        self.config = self._load_config()
        self.templates_dir = Path("docs/diagrams/templates")
        self.cerebrum_colors = {
            'primary': '#4ECDC4',
            'secondary': '#45B7D1',
            'accent': '#96CEB4',
            'warning': '#FFE66D',
            'error': '#FF6B6B',
            'success': '#27AE60',
            'info': '#74B9FF'
        }
        
    def _load_config(self) -> Dict:
        """Load configuration from YAML file."""
        try:
            with open(self.config_path, 'r') as f:
                return yaml.safe_load(f)
        except FileNotFoundError:
            logger.warning(f"Config file {self.config_path} not found, using defaults")
            return {}
        except yaml.YAMLError as e:
            logger.error(f"Error parsing config file: {e}")
            return {}
    
    def enhance_existing_diagrams(self, input_dir: str, output_dir: str) -> None:
        """Enhance existing Mermaid diagrams with modern styling."""
        input_path = Path(input_dir)
        output_path = Path(output_dir)
        
        if not input_path.exists():
            logger.error(f"Input directory {input_dir} does not exist")
            return
        
        output_path.mkdir(parents=True, exist_ok=True)
        
        # Process all .mermaid files
        mermaid_files = list(input_path.glob("*.mermaid"))
        logger.info(f"Found {len(mermaid_files)} Mermaid files to enhance")
        
        for file_path in mermaid_files:
            self._enhance_single_diagram(file_path, output_path)
    
    def _enhance_single_diagram(self, file_path: Path, output_path: Path) -> None:
        """Enhance a single Mermaid diagram file."""
        logger.info(f"Enhancing {file_path.name}")
        
        try:
            with open(file_path, 'r') as f:
                content = f.read()
            
            # Apply enhancements
            enhanced_content = self._apply_enhancements(content, file_path.name)
            
            # Write enhanced file
            output_file = output_path / file_path.name
            with open(output_file, 'w') as f:
                f.write(enhanced_content)
            
            logger.info(f"Enhanced {file_path.name} -> {output_file}")
            
        except Exception as e:
            logger.error(f"Error enhancing {file_path.name}: {e}")
    
    def _apply_enhancements(self, content: str, filename: str) -> str:
        """Apply various enhancements to diagram content."""
        enhanced = content
        
        # Add configuration header if not present
        if not enhanced.startswith('---'):
            enhanced = self._add_config_header(enhanced)
        
        # Enhance styling
        enhanced = self._enhance_styling(enhanced)
        
        # Add interactive elements
        enhanced = self._add_interactive_elements(enhanced, filename)
        
        # Add case annotations
        enhanced = self._add_case_annotations(enhanced)
        
        # Enhance layout
        enhanced = self._enhance_layout(enhanced)
        
        return enhanced
    
    def _add_config_header(self, content: str) -> str:
        """Add modern configuration header to diagram."""
        config_header = """---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

"""
        return config_header + content
    
    def _enhance_styling(self, content: str) -> str:
        """Enhance diagram styling with modern colors and fonts."""
        # Add style definitions if not present
        if 'classDef' not in content:
            style_defs = self._get_style_definitions()
            # Insert after config header
            if content.startswith('---'):
                parts = content.split('\n\n', 1)
                if len(parts) > 1:
                    content = parts[0] + '\n\n' + style_defs + '\n\n' + parts[1]
                else:
                    content = content + '\n\n' + style_defs
            else:
                content = style_defs + '\n\n' + content
        
        return content
    
    def _get_style_definitions(self) -> str:
        """Get modern style definitions for diagrams."""
        return f"""%% Style definitions
classDef framework fill:{self.cerebrum_colors['primary']},stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
classDef engine fill:{self.cerebrum_colors['secondary']},stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
classDef library fill:{self.cerebrum_colors['accent']},stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
classDef transformation fill:{self.cerebrum_colors['warning']},stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
classDef visualization fill:{self.cerebrum_colors['error']},stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
classDef integration fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
classDef process fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
classDef decision fill:#FDCB6E,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600"""
    
    def _add_interactive_elements(self, content: str, filename: str) -> str:
        """Add interactive clickable elements to diagram."""
        # Add interactive elements based on diagram type
        if 'graph' in content or 'flowchart' in content:
            interactive_elements = self._get_interactive_elements(filename)
            if interactive_elements:
                content += '\n\n' + interactive_elements
        
        return content
    
    def _get_interactive_elements(self, filename: str) -> str:
        """Get interactive elements for specific diagram types."""
        base_name = filename.replace('.mermaid', '')
        
        elements = []
        
        # Add common interactive elements
        if 'architecture' in filename:
            elements.extend([
                '%% Interactive elements',
                'click CEREBRUM "https://github.com/ActiveInferenceInstitute/CEREBRUM" "View project repository"',
                'click ActiveInference "src/core/active_inference.py" "View active inference implementation"',
                'click BayesianRep "src/core/bayesian_representation.py" "View Bayesian representation"',
                'click CaseManagement "src/cases/" "View case management system"'
            ])
        elif 'workflow' in filename:
            elements.extend([
                '%% Interactive elements',
                'click Start "docs/getting-started.md" "View getting started guide"',
                'click Process1 "src/processing/input.py" "View input processing code"',
                'click Process3 "src/core/engine.py" "View core processing logic"',
                'click Data2 "src/models/" "View model implementations"'
            ])
        elif 'data' in filename:
            elements.extend([
                '%% Interactive elements',
                'click UserInput "docs/data-sources/user-input.md" "View user input documentation"',
                'click Validation "src/data/validation.py" "View validation logic"',
                'click FeatureExtraction "src/processing/feature_extraction.py" "View feature extraction"',
                'click ActiveInference "src/core/active_inference.py" "View active inference"'
            ])
        
        # Add enhanced styling
        if elements:
            elements.extend([
                '',
                '%% Enhanced styling',
                'linkStyle default stroke:#7F8C8D,stroke-width:2px',
                'linkStyle 0 stroke:#4ECDC4,stroke-width:3px',
                'linkStyle 1 stroke:#45B7D1,stroke-width:3px',
                'linkStyle 2 stroke:#96CEB4,stroke-width:3px'
            ])
        
        return '\n'.join(elements)
    
    def _add_case_annotations(self, content: str) -> str:
        """Add CEREBRUM case annotations to diagram elements."""
        # Add case annotations to key components
        case_mappings = {
            'input': '[DAT]',
            'data': '[DAT]',
            'source': '[DAT]',
            'process': '[ACC]',
            'analysis': '[ACC]',
            'model': '[NOM]',
            'engine': '[NOM]',
            'core': '[NOM]',
            'storage': '[GEN]',
            'database': '[GEN]',
            'transform': '[INS]',
            'adapt': '[INS]',
            'output': '[LOC]',
            'result': '[LOC]',
            'cause': '[ABL]',
            'origin': '[ABL]'
        }
        
        # Simple case annotation addition (more sophisticated logic could be added)
        for keyword, case in case_mappings.items():
            # Add case annotations to component labels
            pattern = rf'(\w+\["([^"]*{keyword}[^"]*)"\])'
            replacement = rf'\1 :::case'
            content = re.sub(pattern, replacement, content, flags=re.IGNORECASE)
        
        return content
    
    def _enhance_layout(self, content: str) -> str:
        """Enhance diagram layout with subgraphs and grouping."""
        # Add subgraph grouping for better organization
        if 'graph' in content and 'subgraph' not in content:
            # This is a simplified enhancement - more sophisticated layout logic could be added
            content = self._add_subgraph_grouping(content)
        
        return content
    
    def _add_subgraph_grouping(self, content: str) -> str:
        """Add subgraph grouping to improve layout."""
        # This is a placeholder for more sophisticated subgraph logic
        # In practice, this would analyze the diagram structure and add appropriate subgraphs
        return content
    
    def generate_new_diagrams(self, output_dir: str, diagram_types: List[str] = None) -> None:
        """Generate new diagram types based on templates."""
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)
        
        if diagram_types is None:
            diagram_types = ['state', 'sequence', 'data-flow', 'user-journey']
        
        for diagram_type in diagram_types:
            self._generate_diagram_type(diagram_type, output_path)
    
    def _generate_diagram_type(self, diagram_type: str, output_path: Path) -> None:
        """Generate a specific type of diagram."""
        logger.info(f"Generating {diagram_type} diagram")
        
        if diagram_type == 'state':
            self._generate_state_diagram(output_path)
        elif diagram_type == 'sequence':
            self._generate_sequence_diagram(output_path)
        elif diagram_type == 'data-flow':
            self._generate_data_flow_diagram(output_path)
        elif diagram_type == 'user-journey':
            self._generate_user_journey_diagram(output_path)
        else:
            logger.warning(f"Unknown diagram type: {diagram_type}")
    
    def _generate_state_diagram(self, output_path: Path) -> None:
        """Generate a model lifecycle state diagram."""
        content = """---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

stateDiagram-v2
    [*] --> ModelCreated
    
    state ModelCreated {
        [*] --> Draft
        Draft --> Validated: Validate Configuration [DAT]
        Validated --> Draft: Validation Failed
        Validated --> Ready: Validation Passed
    }
    
    ModelCreated --> ModelTraining: Start Training [ACC]
    ModelCreated --> [*]: Delete Model
    
    state ModelTraining {
        [*] --> Initializing
        Initializing --> Training: Begin Training [NOM]
        Training --> Evaluating: Training Complete
        Evaluating --> Training: Evaluation Failed
        Evaluating --> Validated: Evaluation Passed
    }
    
    ModelTraining --> ModelDeployed: Deploy Model [GEN]
    ModelTraining --> ModelArchived: Archive Model
    
    state ModelDeployed {
        [*] --> Active
        Active --> Monitoring: Monitor Performance [INS]
        Monitoring --> Active: Performance OK
        Monitoring --> Degraded: Performance Degraded
        Degraded --> Retraining: Retrain Model
        Retraining --> Active: Retraining Complete
    }
    
    ModelDeployed --> ModelRetired: Retire Model [LOC]
    ModelRetired --> ModelArchived: Archive Model
    ModelArchived --> [*]: Permanent Deletion
"""
        
        output_file = output_path / "model_lifecycle_states.mermaid"
        with open(output_file, 'w') as f:
            f.write(content)
        logger.info(f"Generated {output_file}")
    
    def _generate_sequence_diagram(self, output_path: Path) -> None:
        """Generate an API interaction sequence diagram."""
        content = """---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

sequenceDiagram
    participant U as 👤 User
    participant F as 🌐 Frontend
    participant A as 🔌 API Gateway
    participant C as 🧠 CEREBRUM Core
    participant M as 📊 Model Service
    participant D as 🗄️ Database
    
    %% User authentication [DAT]
    U->>F: Login Request [DAT]
    F->>A: Forward Login
    A->>C: Authenticate & Route
    
    %% Model configuration [ACC]
    C->>M: Load Model Configuration [ACC]
    M->>D: Retrieve Model Data
    D-->>M: Model Configuration
    M-->>C: Config Response
    
    %% Model execution [NOM]
    C->>C: Execute Active Inference [NOM]
    C->>C: Apply Case Transformations
    
    %% Data persistence [GEN]
    C->>D: Save Results [GEN]
    D-->>C: Confirmation
    
    %% Response delivery [LOC]
    C-->>A: Processed Results [LOC]
    A-->>F: Formatted Response
    F-->>U: Display Results
"""
        
        output_file = output_path / "api_interaction_sequence.mermaid"
        with open(output_file, 'w') as f:
            f.write(content)
        logger.info(f"Generated {output_file}")
    
    def _generate_data_flow_diagram(self, output_path: Path) -> None:
        """Generate a data flow diagram."""
        content = """---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

flowchart TD
    %% Style definitions
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef process fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef storage fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef output fill:#FF6B6B,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    %% Data sources [DAT]
    subgraph "Data Sources [DAT]"
        direction LR
        UserInput["👤 User Input<br/><small>Manual data entry</small>"]:::data
        SensorData["📡 Sensor Data<br/><small>Real-time feeds</small>"]:::data
        ExternalAPI["🌐 External APIs<br/><small>Third-party data</small>"]:::data
    end
    
    %% Data processing [ACC]
    subgraph "Data Processing [ACC]"
        direction TB
        Validation["✅ Data Validation<br/><small>Quality checks</small>"]:::process
        Transformation["🔄 Data Transformation<br/><small>Format conversion</small>"]:::process
        Analysis["📊 Data Analysis<br/><small>Pattern recognition</small>"]:::process
    end
    
    %% Data storage [GEN]
    subgraph "Data Storage [GEN]"
        direction LR
        RawStorage["🗄️ Raw Data Storage<br/><small>Original data</small>"]:::storage
        ProcessedStorage["📊 Processed Data Storage<br/><small>Cleaned data</small>"]:::storage
        ModelStorage["🧠 Model Storage<br/><small>Model artifacts</small>"]:::storage
    end
    
    %% Data output [INS]
    subgraph "Data Output [INS]"
        direction LR
        Reports["📋 Reports<br/><small>Analytical reports</small>"]:::output
        Visualizations["📊 Visualizations<br/><small>Data visualizations</small>"]:::output
        APIs["🔌 APIs<br/><small>Data interfaces</small>"]:::output
    end
    
    %% Data flow connections
    UserInput --> Validation
    SensorData --> Validation
    ExternalAPI --> Validation
    
    Validation --> Transformation
    Transformation --> Analysis
    
    Analysis --> RawStorage
    RawStorage --> ProcessedStorage
    ProcessedStorage --> ModelStorage
    
    ModelStorage --> Reports
    ModelStorage --> Visualizations
    ModelStorage --> APIs
    
    %% Interactive elements
    click UserInput "docs/data-sources/user-input.md" "View user input documentation"
    click Validation "src/data/validation.py" "View validation logic"
    click Analysis "src/processing/analysis.py" "View analysis logic"
    click Reports "docs/reports/" "View report templates"
    
    %% Enhanced styling
    linkStyle default stroke:#7F8C8D,stroke-width:2px
    linkStyle 0 stroke:#FD79A8,stroke-width:3px
    linkStyle 1 stroke:#00B894,stroke-width:3px
    linkStyle 2 stroke:#A29BFE,stroke-width:3px
    linkStyle 3 stroke:#FF6B6B,stroke-width:3px
"""
        
        output_file = output_path / "data_flow_pipeline.mermaid"
        with open(output_file, 'w') as f:
            f.write(content)
        logger.info(f"Generated {output_file}")
    
    def _generate_user_journey_diagram(self, output_path: Path) -> None:
        """Generate a user journey diagram."""
        content = """---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

journey
    title CEREBRUM User Journey: Model Development Workflow
    
    section Initial Setup [DAT]
        Define Problem: 5: User, System
        Configure Environment: 4: User, System
        Import Data: 3: User, System
        Validate Input: 4: System
    
    section Model Development [ACC]
        Select Case Structure: 5: User
        Configure Bayesian Model: 4: User, System
        Define Transformations: 4: User, System
        Set Parameters: 3: User
    
    section Training & Validation [NOM]
        Start Training: 5: System
        Monitor Progress: 4: User, System
        Validate Results: 4: User, System
        Adjust Parameters: 3: User
    
    section Deployment [GEN]
        Export Model: 4: User, System
        Deploy to Production: 5: System
        Monitor Performance: 4: System
        Generate Reports: 3: System
    
    section Maintenance [INS]
        Update Model: 4: User, System
        Retrain with New Data: 3: System
        Optimize Performance: 4: System
        Archive Old Versions: 2: System
"""
        
        output_file = output_path / "user_journey_workflow.mermaid"
        with open(output_file, 'w') as f:
            f.write(content)
        logger.info(f"Generated {output_file}")
    
    def apply_templates(self, template_name: str, output_dir: str) -> None:
        """Apply a specific template to generate diagrams."""
        template_file = self.templates_dir / f"{template_name}.md"
        
        if not template_file.exists():
            logger.error(f"Template {template_name} not found")
            return
        
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)
        
        # Extract template content and generate diagrams
        with open(template_file, 'r') as f:
            template_content = f.read()
        
        # Parse template and extract diagram examples
        diagrams = self._extract_diagrams_from_template(template_content)
        
        for i, diagram in enumerate(diagrams):
            output_file = output_path / f"{template_name}_example_{i+1}.mermaid"
            with open(output_file, 'w') as f:
                f.write(diagram)
            logger.info(f"Generated {output_file}")
    
    def _extract_diagrams_from_template(self, template_content: str) -> List[str]:
        """Extract diagram examples from template content."""
        diagrams = []
        
        # Find code blocks with mermaid content
        pattern = r'```markdown\s*\n(.*?)\n```'
        matches = re.findall(pattern, template_content, re.DOTALL)
        
        for match in matches:
            if 'config:' in match or 'graph' in match or 'flowchart' in match:
                diagrams.append(match.strip())
        
        return diagrams

def main():
    """Main function to run the diagram enhancement tool."""
    parser = argparse.ArgumentParser(description='CEREBRUM Diagram Enhancement Tool')
    parser.add_argument('--input', help='Input directory containing Mermaid files')
    parser.add_argument('--output', required=True, help='Output directory for enhanced diagrams')
    parser.add_argument('--generate-new', action='store_true', help='Generate new diagram types')
    parser.add_argument('--apply-templates', help='Apply specific template (e.g., cognitive, workflow)')
    parser.add_argument('--diagram-types', nargs='+', help='Specific diagram types to generate')
    parser.add_argument('--config', default='docs/diagrams/config.yaml', help='Configuration file path')
    
    args = parser.parse_args()
    
    enhancer = DiagramEnhancer(args.config)
    
    if args.input:
        logger.info(f"Enhancing diagrams from {args.input} to {args.output}")
        enhancer.enhance_existing_diagrams(args.input, args.output)
    
    if args.generate_new:
        logger.info(f"Generating new diagrams in {args.output}")
        enhancer.generate_new_diagrams(args.output, args.diagram_types)
    
    if args.apply_templates:
        logger.info(f"Applying template {args.apply_templates} to {args.output}")
        enhancer.apply_templates(args.apply_templates, args.output)
    
    logger.info("Diagram enhancement complete!")

if __name__ == '__main__':
    main() 