# Visualization Context

## Overview

Visualization logic often decouples from the core model logic to keep `src/core` clean.

## Architecture

- Visualizers often subscribe to model updates or take a model snapshot as input.
- **`insect/`**: Contains visualizers specific to the insect models.
