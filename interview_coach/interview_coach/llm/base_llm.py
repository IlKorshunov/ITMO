from abc import ABC, abstractmethod
from typing import List, Dict
from dataclasses import dataclass


@dataclass
class Message:
    """Сообщение чата."""
    role: str 
    content: str


class BaseLLMProvider(ABC):
    """Базовый класс для LLM провайдеров."""
    
    @abstractmethod
    async def generate(
        self,
        messages: List[Message],
        temperature: float = 0.7,
        max_tokens: int = 1024,
        json_mode: bool = False,
    ) -> str:
        """
        Асинхронная генерация ответа.
        
        Аргументы:
            messages: История сообщений
            temperature: Температура генерации
            max_tokens: Лимит токенов
            json_mode: Требовать JSON-ответ от модели
        """
        pass
    
    @abstractmethod
    def generate_sync(
        self,
        messages: List[Message],
        temperature: float = 0.7,
        max_tokens: int = 1024,
        json_mode: bool = False,
    ) -> str:
        """Синхронная генерация ответа."""
        pass
    
    def format_messages(self, messages: List[Message]) -> List[Dict]:
        """Конвертирует Message в dict."""
        return [{"role": m.role, "content": m.content} for m in messages]
        
    @staticmethod
    def clean_response(response: str) -> str:
        import re
        response = response.strip()
        response = re.sub(r'\n{3,}', '\n\n', response)
        response = re.sub(r' {2,}', ' ', response)
        return response
        
    @staticmethod
    def clean_json_response(response: str) -> str:
        response = response.strip()
        if response.startswith("```json"):
            response = response[7:]
        if response.startswith("```"):
            response = response[3:]
        if response.endswith("```"):
            response = response[:-3]
        return response.strip()
