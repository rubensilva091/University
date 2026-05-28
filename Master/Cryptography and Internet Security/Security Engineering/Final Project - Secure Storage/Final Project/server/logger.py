import logging
import re
import os
from contextvars import ContextVar
from logging.handlers import RotatingFileHandler

request_id_context = ContextVar("request_id", default="N/A")

class RequestIdFilter(logging.Filter):
    def filter(self, record):
        record.request_id = request_id_context.get()
        return True

class RedactingFormatter(logging.Formatter):
    PATTERNS = [
        (re.compile(r'Bearer\s+[A-Za-z0-9\-\._~\+\/]+'), "Bearer [REDACTED]"),
        (re.compile(r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,7}\b'), "[EMAIL_REDACTED]")
    ]

    def format(self, record):
        original_message = super().format(record)
        for pattern, replacement in self.PATTERNS:
            original_message = pattern.sub(replacement, original_message)
        return original_message

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LOG_DIR = os.path.join(BASE_DIR, "data")
LOG_FILE = os.path.join(LOG_DIR, "server_audit.log")

if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)

logger = logging.getLogger("SecureApp")
logger.setLevel(logging.INFO)

formatter = RedactingFormatter('%(asctime)s - [%(levelname)s] - ReqID: %(request_id)s - %(message)s')

ch = logging.StreamHandler()
ch.setFormatter(formatter)
logger.addHandler(ch)

fh = RotatingFileHandler(LOG_FILE, maxBytes=5*1024*1024, backupCount=3)
fh.setFormatter(formatter)
logger.addHandler(fh)

logger.addFilter(RequestIdFilter())