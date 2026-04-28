import logging
import sys


def get_logger():
    """
    Configure a Python logger that emits all levels to stderr.

    Verbosity filtering happens R-side: presentifyr's run_python_script
    passes a stderr_callback that parses [LEVEL] tags and filters by
    PRFY_VERBOSE. Python therefore stays dumb and emits everything.
    """
    logger = logging.getLogger("py_logger")
    logger.setLevel(logging.DEBUG)

    handler = logging.StreamHandler(sys.stderr)
    handler.setLevel(logging.DEBUG)
    handler.setFormatter(logging.Formatter(
        "%(asctime)s [%(levelname)s] %(message)s",
        "%Y-%m-%d %H:%M:%S"
    ))

    if logger.hasHandlers():
        logger.handlers.clear()
    logger.addHandler(handler)

    return logger
