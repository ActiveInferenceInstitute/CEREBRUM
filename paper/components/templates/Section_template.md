## Section Title

### Subsection

This is example text for a section template. Replace this with the actual content of your section.

#### Key Points to Cover in This Section

- Point 1
- Point 2
- Point 3

### Another Subsection

Reference figures using pandoc-crossref syntax: @fig:figX. Make sure to reference figures before including them.

![Caption for the figure.](Figure_X.png){#fig:figX}

### Tables

**Table X: Example Table Title**

| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Data 1   | Data 2   | Data 3   |
| Data 4   | Data 5   | Data 6   |

### Mathematical Content

Inline equations use single dollar signs: $E = mc^2$

Display equations use double dollar signs:

$$
F = D_{KL}[q(s|T(m))||p(s|m)] - \mathbb{E}_{p}[\log p(o|s,T(m))]  \tag{X}
$$

Reference equations by their tag number: As shown in equation (X), the free energy...

<!-- Template Notes:
1. Replace "Section Title" with your actual section title
2. Start with level 2 headers (##) for main sections
3. Use level 3 headers (###) for subsections
4. Update figure and equation references to use the correct numbers
5. Save the file with the appropriate numeric prefix (e.g., 9_section_title.md) -->