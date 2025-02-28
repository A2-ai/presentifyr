import logging
import sys
import os

def get_logger():
    LOG_LEVELS = {
        "DEBUG": logging.DEBUG,
        "INFO": logging.INFO,
        "WARN": logging.WARNING,
        "ERROR": logging.ERROR,
        "FATAL": logging.CRITICAL
    }

    py_log_level = os.getenv("PY_LOG_LEVEL", "WARN")

    # Convert string to numeric logging level, default to WARN if invalid key
    numeric_level = LOG_LEVELS.get(py_log_level, logging.WARNING)

    logger = logging.getLogger("py_logger")

    logger.setLevel(numeric_level)

    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setLevel(logging.NOTSET)  # Let the logger decide
    formatter = logging.Formatter(
        "%(asctime)s [%(levelname)s] %(message)s",
        "%Y-%m-%d %H:%M:%S"
    )
    console_handler.setFormatter(formatter)

    if logger.hasHandlers():
        logger.handlers.clear()

    logger.addHandler(console_handler)

    return logger
