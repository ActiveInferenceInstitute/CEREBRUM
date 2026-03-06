"""
LEXICON Core Engine Components

Core functionality for the LEXICON pipeline orchestration.
"""

from .engine import LexiconEngine as LexiconEngine
from .config import LexiconConfig as LexiconConfig
from .exceptions import LexiconError as LexiconError
from .logging import setup_logging as setup_logging
from .session import (
    create_output_dir as create_output_dir,
    save_input_text as save_input_text,
    save_metadata as save_metadata,
    copy_input_file as copy_input_file,
    save_result as save_result,
    save_error_info as save_error_info,
    create_batch_subdirectories as create_batch_subdirectories,
    create_batch_input_subdirectories as create_batch_input_subdirectories,
    save_batch_summary as save_batch_summary,
)
from .environment import (
    configure_cuda_environment as configure_cuda_environment,
    configure_protobuf_environment as configure_protobuf_environment,
    configure_transformers_environment as configure_transformers_environment,
    test_transformers_import as test_transformers_import,
    check_environment_dependencies as check_environment_dependencies,
    print_environment_status as print_environment_status,
    validate_environment as validate_environment,
    setup_environment_logging as setup_environment_logging,
)