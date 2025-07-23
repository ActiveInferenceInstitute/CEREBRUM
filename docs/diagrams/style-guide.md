# CEREBRUM Diagram Style Guide

## Overview

This style guide establishes consistent visual design principles for all CEREBRUM diagrams, ensuring clarity, accessibility, and professional appearance across the project.

## Design Principles

### 1. Clarity First
- **Clear Hierarchy**: Establish visual hierarchy through size, color, and positioning
- **Readable Text**: Use legible fonts and appropriate sizing
- **Logical Flow**: Guide the eye through natural reading patterns
- **Minimal Clutter**: Remove unnecessary elements that don't add value

### 2. Consistency
- **Unified Color Palette**: Use consistent colors across all diagrams
- **Standardized Icons**: Use consistent iconography and emojis
- **Uniform Styling**: Apply consistent stroke weights and styling
- **Predictable Layouts**: Use familiar patterns and arrangements

### 3. Accessibility
- **High Contrast**: Ensure sufficient contrast ratios (WCAG 2.1 AA)
- **Color Independence**: Don't rely solely on color to convey information
- **Clear Labels**: Use descriptive, unambiguous labels
- **Screen Reader Friendly**: Structure content for assistive technologies

## Color Palette

### Primary Colors
```css
/* CEREBRUM Teal - Primary Brand Color */
--cerebrum-primary: #4ECDC4;
--cerebrum-primary-dark: #2C3E50;
--cerebrum-primary-light: #A8E6CF;

/* CEREBRUM Blue - Secondary Brand Color */
--cerebrum-secondary: #45B7D1;
--cerebrum-secondary-dark: #2980B9;
--cerebrum-secondary-light: #74B9FF;

/* CEREBRUM Green - Accent Color */
--cerebrum-accent: #96CEB4;
--cerebrum-accent-dark: #27AE60;
--cerebrum-accent-light: #00B894;
```

### Semantic Colors
```css
/* Success States */
--success-primary: #27AE60;
--success-light: #51CF66;

/* Warning States */
--warning-primary: #FDCB6E;
--warning-dark: #E17055;

/* Error States */
--error-primary: #FF6B6B;
--error-dark: #E74C3C;

/* Information States */
--info-primary: #74B9FF;
--info-dark: #0984E3;
```

### Neutral Colors
```css
/* Text Colors */
--text-primary: #2C3E50;
--text-secondary: #7F8C8D;
--text-muted: #95A5A6;

/* Background Colors */
--background-primary: #FFFFFF;
--background-secondary: #F8F9FA;
--background-tertiary: #E9ECEF;

/* Border Colors */
--border-primary: #BDC3C7;
--border-secondary: #E9ECEF;
```

## Typography

### Font Stack
```css
font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
```

### Font Sizes
```css
/* Small Text (captions, metadata) */
font-size: 12px;
line-height: 1.4;

/* Normal Text (body content) */
font-size: 14px;
line-height: 1.5;

/* Large Text (headings, emphasis) */
font-size: 16px;
line-height: 1.4;

/* Extra Large Text (titles) */
font-size: 18px;
line-height: 1.3;
```

### Font Weights
```css
/* Normal weight for body text */
font-weight: 400;

/* Medium weight for emphasis */
font-weight: 500;

/* Semibold weight for subheadings */
font-weight: 600;

/* Bold weight for headings and important elements */
font-weight: 700;
```

## Component Styling

### Framework Components
```css
/* Main system components */
classDef framework {
    fill: #4ECDC4;
    stroke: #2C3E50;
    stroke-width: 3px;
    color: #FFFFFF;
    font-weight: bold;
    font-size: 16px;
}
```

### Engine Components
```css
/* Core processing components */
classDef engine {
    fill: #45B7D1;
    stroke: #2C3E50;
    stroke-width: 2px;
    color: #FFFFFF;
    font-weight: 600;
    font-size: 14px;
}
```

### Library Components
```css
/* Model and data components */
classDef library {
    fill: #96CEB4;
    stroke: #2C3E50;
    stroke-width: 2px;
    color: #2C3E50;
    font-weight: 500;
    font-size: 14px;
}
```

### Process Components
```css
/* Workflow and process components */
classDef process {
    fill: #00B894;
    stroke: #2C3E50;
    stroke-width: 2px;
    color: #FFFFFF;
    font-weight: 500;
    font-size: 14px;
}
```

### Decision Components
```css
/* Decision points and logic */
classDef decision {
    fill: #FDCB6E;
    stroke: #2C3E50;
    stroke-width: 2px;
    color: #2C3E50;
    font-weight: 600;
    font-size: 14px;
}
```

### Data Components
```css
/* Storage and data components */
classDef data {
    fill: #FD79A8;
    stroke: #2C3E50;
    stroke-width: 2px;
    color: #FFFFFF;
    font-weight: 500;
    font-size: 14px;
}
```

## Iconography

### System Icons
- 🏛️ **Framework**: Main system components
- 🧠 **Cognitive**: Brain and neural components
- 📊 **Data**: Analytics and metrics
- 🔄 **Process**: Workflow and transformation
- 🎯 **Target**: Goals and objectives
- ⚙️ **Configuration**: Settings and parameters

### Case Icons
- 👤 **Nominative [NOM]**: Agent/subject processing
- 🎯 **Accusative [ACC]**: Object/patient processing
- 📦 **Genitive [GEN]**: Source/possession
- 📤 **Dative [DAT]**: Recipient/beneficiary
- 🛠️ **Instrumental [INS]**: Means/method
- 📍 **Locative [LOC]**: Location/context
- 🌊 **Ablative [ABL]**: Origin/cause

### Status Icons
- ✅ **Success**: Completed processes
- ⚠️ **Warning**: Caution or attention needed
- ❌ **Error**: Failed or problematic states
- 🔄 **Processing**: Active or ongoing processes
- ⏸️ **Paused**: Suspended or waiting states

## Layout Guidelines

### Spacing
```css
/* Minimum spacing between elements */
--spacing-xs: 4px;
--spacing-sm: 8px;
--spacing-md: 16px;
--spacing-lg: 24px;
--spacing-xl: 32px;

/* Node padding */
--node-padding: 8px;
--node-padding-large: 12px;

/* Subgraph padding */
--subgraph-padding: 24px;
```

### Alignment
- **Left-to-Right**: Use for sequential processes
- **Top-to-Bottom**: Use for hierarchical structures
- **Center Alignment**: Use for balanced layouts
- **Consistent Spacing**: Maintain uniform distances

### Grouping
- **Logical Clusters**: Group related components
- **Clear Boundaries**: Use subgraphs for organization
- **Visual Separation**: Use spacing and borders
- **Hierarchical Structure**: Show relationships clearly

## Interactive Elements

### Clickable Components
```css
/* Interactive elements */
classDef clickable {
    fill: #74B9FF;
    stroke: #2C3E50;
    stroke-width: 2px;
    color: #FFFFFF;
    font-weight: 500;
    cursor: pointer;
    transition: all 0.2s ease;
}

/* Hover effects */
.clickable:hover {
    fill: #0984E3;
    stroke-width: 3px;
}
```

### Link Styling
```css
/* Default link style */
linkStyle default {
    stroke: #7F8C8D;
    stroke-width: 2px;
}

/* Primary connections */
linkStyle 0 {
    stroke: #4ECDC4;
    stroke-width: 3px;
}

/* Secondary connections */
linkStyle 1 {
    stroke: #45B7D1;
    stroke-width: 2px;
}

/* Feedback loops */
linkStyle feedback {
    stroke: #95A5A6;
    stroke-width: 1px;
    stroke-dasharray: 5,5;
}
```

## Accessibility Guidelines

### Color Contrast
- **Normal Text**: Minimum 4.5:1 contrast ratio
- **Large Text**: Minimum 3:1 contrast ratio
- **UI Components**: Minimum 3:1 contrast ratio
- **Interactive Elements**: Minimum 4.5:1 contrast ratio

### Color Independence
- **Don't rely solely on color**: Use patterns, shapes, or text
- **Provide alternatives**: Use icons, labels, or descriptions
- **Test in grayscale**: Ensure information is still clear

### Text Accessibility
- **Readable Fonts**: Use sans-serif fonts with good legibility
- **Adequate Size**: Minimum 12px for body text
- **Line Spacing**: Use 1.4-1.6 line height for readability
- **Font Weight**: Use medium (500) or higher for important text

### Screen Reader Support
- **Descriptive Labels**: Use meaningful, descriptive text
- **Logical Structure**: Organize content in logical order
- **Alternative Text**: Provide alt text for complex diagrams
- **Navigation**: Ensure logical tab order for interactive elements

## Best Practices

### 1. Planning
- **Define Purpose**: Clearly identify the diagram's goal
- **Identify Audience**: Consider the viewer's expertise level
- **Choose Type**: Select the most appropriate diagram type
- **Plan Layout**: Sketch the structure before implementation

### 2. Implementation
- **Use Templates**: Start with established templates
- **Follow Standards**: Apply consistent styling and naming
- **Test Iteratively**: Review and refine throughout creation
- **Document Decisions**: Note any deviations from standards

### 3. Review
- **Self-Review**: Check for clarity and completeness
- **Peer Review**: Get feedback from colleagues
- **User Testing**: Test with target audience
- **Accessibility Audit**: Verify accessibility compliance

### 4. Maintenance
- **Version Control**: Track changes and updates
- **Documentation**: Keep style guide current
- **Training**: Educate team on standards
- **Feedback Loop**: Continuously improve based on usage

## Quality Checklist

### Pre-Submission Review
- [ ] Uses appropriate template
- [ ] Follows naming conventions
- [ ] Includes interactive elements where relevant
- [ ] Tested for accessibility
- [ ] Responsive design verified
- [ ] Links are functional
- [ ] Color contrast meets WCAG standards
- [ ] Text is readable and clear
- [ ] Layout is logical and intuitive
- [ ] Icons are consistent and meaningful

### Technical Review
- [ ] Mermaid syntax is correct
- [ ] Configuration is properly set
- [ ] Styling is consistent
- [ ] Performance is optimized
- [ ] Cross-browser compatibility verified
- [ ] Mobile responsiveness tested

### Content Review
- [ ] Information is accurate and complete
- [ ] Relationships are correctly represented
- [ ] Labels are clear and descriptive
- [ ] Case annotations are appropriate
- [ ] Documentation links are current
- [ ] Examples are relevant and helpful

## Resources

### Tools
- [Mermaid Live Editor](https://mermaid.live/)
- [Color Contrast Checker](https://webaim.org/resources/contrastchecker/)
- [Accessibility Validator](https://www.w3.org/WAI/WCAG21/quickref/)
- [Font Pairing Guide](https://fontpair.co/)

### Documentation
- [Mermaid Documentation](https://mermaid-js.github.io/mermaid/)
- [WCAG 2.1 Guidelines](https://www.w3.org/WAI/WCAG21/quickref/)
- [CEREBRUM Project](https://github.com/ActiveInferenceInstitute/CEREBRUM)
- [Template Library](../templates/)

### Support
- [CEREBRUM Discord](https://discord.gg/cerebrum)
- [GitHub Issues](https://github.com/ActiveInferenceInstitute/CEREBRUM/issues)
- [Documentation Channel](https://discord.gg/cerebrum-docs) 