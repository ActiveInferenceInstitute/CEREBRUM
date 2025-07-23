# CEREBRUM Diagram Templates

This directory contains standardized templates for creating consistent, modern diagrams across the CEREBRUM project.

## Template Categories

### 1. Architecture Diagrams
- **System Architecture**: High-level system overview
- **Component Architecture**: Detailed component relationships
- **Integration Architecture**: API and service interactions

### 2. Process Diagrams
- **Workflow Diagrams**: Step-by-step processes
- **State Diagrams**: System state transitions
- **Sequence Diagrams**: Component interactions over time

### 3. Data Diagrams
- **Entity-Relationship**: Data model relationships
- **Data Flow**: Information movement through systems
- **Knowledge Graphs**: Conceptual relationships

### 4. User Experience
- **User Journey**: User interaction flows
- **Interface Mockups**: UI/UX visualizations
- **Navigation Maps**: Site structure and navigation

## Template Usage

### Basic Template Structure
```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

%% Template-specific content here
```

### Available Templates

1. **[architecture.md](architecture.md)** - System and component architecture
2. **[workflow.md](workflow.md)** - Process and workflow diagrams
3. **[data-model.md](data-model.md)** - Data and entity relationships
4. **[user-journey.md](user-journey.md)** - User experience flows
5. **[integration.md](integration.md)** - API and service integration
6. **[cognitive.md](cognitive.md)** - Cognitive and neural architecture
7. **[state.md](state.md)** - State diagrams and lifecycle management

## Best Practices

### 1. Naming Conventions
- Use descriptive, action-oriented names for nodes
- Include case annotations where relevant (e.g., `[NOM]`, `[ACC]`)
- Use consistent terminology across diagrams

### 2. Visual Hierarchy
- Primary components: `:::framework` or `:::engine`
- Secondary components: `:::library` or `:::transformation`
- Supporting elements: `:::integration` or `:::data`

### 3. Interactive Elements
- Add clickable links to source code: `click Component "url" "tooltip"`
- Include documentation references
- Link to related diagrams and resources

### 4. Accessibility
- Use high contrast colors
- Include descriptive alt text
- Ensure keyboard navigation support
- Test with screen readers

### 5. Responsive Design
- Use relative sizing where possible
- Test on different screen sizes
- Optimize for mobile viewing

## Template Examples

### Architecture Template
```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    classDef framework fill:#4ECDC4,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    classDef engine fill:#45B7D1,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef library fill:#96CEB4,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
    
    %% Main system
    System["🏛️ System Name"]:::framework
    
    %% Core components
    Component1["Component 1"]:::engine
    Component2["Component 2"]:::engine
    
    %% Supporting components
    Support1["Support 1"]:::library
    Support2["Support 2"]:::library
    
    %% Relationships
    System --> Component1
    System --> Component2
    Component1 --> Support1
    Component2 --> Support2
    
    %% Interactive elements
    click System "https://github.com/ActiveInferenceInstitute/CEREBRUM" "View project"
    click Component1 "path/to/source" "View source code"
```

### Workflow Template
```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

flowchart LR
    classDef process fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef decision fill:#FDCB6E,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    Start([Start]):::process
    Process1["Process Step 1"]:::process
    Decision{"Decision Point"}:::decision
    Process2["Process Step 2"]:::process
    Data[("Data Store")]:::data
    End([End]):::process
    
    Start --> Process1
    Process1 --> Decision
    Decision -->|Yes| Process2
    Decision -->|No| Data
    Process2 --> End
    Data --> End
```

## Customization Guidelines

### Color Schemes
- **Primary**: `#4ECDC4` (CEREBRUM teal)
- **Secondary**: `#45B7D1` (CEREBRUM blue)
- **Accent**: `#96CEB4` (CEREBRUM green)
- **Warning**: `#FFE66D` (CEREBRUM yellow)
- **Error**: `#FF6B6B` (CEREBRUM red)

### Typography
- **Font Family**: Inter, system fonts
- **Font Sizes**: 12px (small), 14px (normal), 16px (large)
- **Font Weights**: 400 (normal), 500 (medium), 600 (semibold), 700 (bold)

### Spacing and Layout
- **Node Padding**: 8px minimum
- **Edge Spacing**: 16px minimum
- **Subgraph Padding**: 24px
- **Border Radius**: 4px for modern look

## Quality Assurance

### Pre-Submission Checklist
- [ ] Uses appropriate template
- [ ] Follows naming conventions
- [ ] Includes interactive elements where relevant
- [ ] Tested for accessibility
- [ ] Responsive design verified
- [ ] Links are functional
- [ ] Color contrast meets WCAG standards

### Review Process
1. **Technical Review**: Accuracy and completeness
2. **Design Review**: Visual appeal and consistency
3. **Accessibility Review**: WCAG 2.1 compliance
4. **User Experience Review**: Clarity and usability

## Contributing

### Adding New Templates
1. Create template file in appropriate category
2. Include comprehensive example
3. Document usage guidelines
4. Add to this README
5. Submit for review

### Template Updates
1. Maintain backward compatibility
2. Update documentation
3. Notify users of changes
4. Provide migration guide if needed

## Resources

- [Mermaid Documentation](https://mermaid-js.github.io/mermaid/)
- [CEREBRUM Style Guide](../style-guide.md)
- [Accessibility Guidelines](https://www.w3.org/WAI/WCAG21/quickref/)
- [Color Contrast Checker](https://webaim.org/resources/contrastchecker/) 