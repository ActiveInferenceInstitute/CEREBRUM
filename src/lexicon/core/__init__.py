"""
LEXICON Core Engine Components

Core functionality for the LEXICON pipeline orchestration.
"""

from .engine import LexiconEngine
from .config import LexiconConfig
from .exceptions import LexiconError
from .logging import setup_logging
from .session import (
    create_output_dir,
    save_input_text,
    save_metadata,
    copy_input_file,
    save_result,
    save_error_info,
    create_batch_subdirectories,
    create_batch_input_subdirectories,
    save_batch_summary
)
from .environment import (
    configure_cuda_environment,
    configure_protobuf_environment,
    configure_transformers_environment,
    test_transformers_import,
    check_environment_dependencies,
    print_environment_status,
    validate_environment,
    setup_environment_logging
) 