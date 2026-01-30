import requests
from typing import List
from .base_llm import BaseLLMProvider, Message
from ..config import config


class LocalLLMProvider(BaseLLMProvider):
    def __init__(self, base_url: str = None, model: str = None, api_key: str = "custom"):
        self.base_url = (base_url or config.llm.local_model_url).rstrip("/")
        self.model = model or config.llm.local_model
        self.api_key = api_key

    async def generate(
        self, messages: List[Message], temperature: float = 0.7,
        max_tokens: int = 1024, json_mode: bool = False,
    ) -> str:
        return self.generate_sync(messages, temperature, max_tokens, json_mode)

    def generate_sync(
        self, messages: List[Message], temperature: float = 0.7,
        max_tokens: int = 1024, json_mode: bool = False,
    ) -> str:
        try:
            result = self._generate_openai(messages, temperature, max_tokens, json_mode)
        except requests.exceptions.HTTPError as e:
            if e.response.status_code == 404:
                try:
                    result = self._generate_ollama_native(messages, temperature, max_tokens, json_mode)
                except Exception as e_ollama:
                    raise RuntimeError(f"OpenAI API ({e}) и Ollama API ({e_ollama}) недоступны")
            else:
                raise e
        except Exception as e:
            raise RuntimeError(f"Ошибка локальной LLM: {e}")
        
        return BaseLLMProvider.clean_response(result)

    def _generate_openai(self, messages, temperature, max_tokens, json_mode) -> str:
        url = f"{self.base_url}/chat/completions"
        headers = {"Content-Type": "application/json", "Authorization": f"Bearer {self.api_key}"}
        
        payload = {
            "model": self.model,
            "messages": self.format_messages(messages),
            "temperature": temperature,
            "max_tokens": max_tokens,
        }
        if json_mode:
            payload["response_format"] = {"type": "json_object"}

        response = requests.post(url, headers=headers, json=payload)
        response.raise_for_status()
        return response.json()["choices"][0]["message"]["content"]

    def _generate_ollama_native(self, messages, temperature, max_tokens, json_mode) -> str:
        base_url = self.base_url.replace("/v1", "")
        url = f"{base_url}/api/chat"
        
        payload = {
            "model": self.model,
            "messages": self.format_messages(messages),
            "options": {"temperature": temperature, "num_predict": max_tokens},
            "stream": False
        }
        if json_mode:
            payload["format"] = "json"

        response = requests.post(url, json=payload, timeout=120)
        response.raise_for_status()
        return response.json()["message"]["content"]
