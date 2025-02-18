import logging
import sys

def get_logger(log_level="DEBUG"):
  
    LOG_LEVELS = {
        "DEBUG": logging.DEBUG,
        "INFO": logging.INFO,
        "WARN": logging.WARNING,
        "ERROR": logging.ERROR,
        "FATAL": logging.CRITICAL
    }

    if log_level not in LOG_LEVELS:
        raise ValueError(f"Invalid log level: {log_level}. Choose from {list(LOG_LEVELS.keys())}")
    
    log_level = LOG_LEVELS[log_level]
    
    logger = logging.getLogger("py_logger")
    logger.setLevel(logging.DEBUG)

    ## Log message format (matches R implementation)
    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s", "%Y-%m-%d %H:%M:%S")

    console_handler = logging.StreamHandler(sys.stdout) 
    console_handler.setLevel(log_level)
    console_handler.setFormatter(formatter)
    
    
    if logger.hasHandlers():
        logger.handlers.clear()

    logger.addHandler(console_handler)

    return logger
