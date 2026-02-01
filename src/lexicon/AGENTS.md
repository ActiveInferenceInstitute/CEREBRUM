# Lexicon Context

## Overview

This module handles the linguistic processing capabilities of CEREBRUM.

## Architecture

- **NLP**: `nlp/` contains integration with libraries like spaCy.
- **Ingest**: `ingest/` handles loading text data.
- **Declension**: `declension/` manages grammatical case transformations.

## Usage

- Agents should use `lexicon` to analyze text inputs or generate linguistically structured outputs.
- Key entry point: `run.py` (for CLI or high-level execution).
