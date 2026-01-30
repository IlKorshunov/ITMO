import os
from dataclasses import dataclass, field
from typing import Optional
from enum import Enum
from dotenv import load_dotenv

load_dotenv()


class LLMProvider(Enum):
    HUGGINGFACE_API = "huggingface_api"
    LOCAL = "local"


@dataclass
class LLMConfig:
    provider: LLMProvider = field(default_factory=lambda: LLMProvider(
        os.getenv("LLM_PROVIDER", "local")
    ))
    
    hf_api_token: Optional[str] = field(default_factory=lambda: os.getenv("HF_API_TOKEN"))
    hf_model: str = field(default_factory=lambda: os.getenv(
        "HF_MODEL", "Qwen/Qwen2.5-7B-Instruct"
    ))
    
    local_model_url: str = field(default_factory=lambda: os.getenv(
        "LOCAL_MODEL_URL", "http://localhost:11434/v1"
    ))
    local_model: str = field(default_factory=lambda: os.getenv(
        "LOCAL_MODEL", "qwen2.5:7b"
    ))
    
    temperature: float = 0.7
    max_tokens: int = 1024


@dataclass
class InterviewConfig:
    min_questions: int = 5
    max_questions: int = 15
    initial_difficulty: int = 3 
    stop_command: str = "стоп"


@dataclass
class Config:
    llm: LLMConfig = field(default_factory=LLMConfig)
    interview: InterviewConfig = field(default_factory=InterviewConfig)
    output_dir: str = field(default_factory=lambda: os.getenv("OUTPUT_DIR", "./output"))
    debug: bool = field(default_factory=lambda: os.getenv("DEBUG", "false").lower() == "true")


config = Config()
