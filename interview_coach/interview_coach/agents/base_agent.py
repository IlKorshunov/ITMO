import json
from abc import ABC, abstractmethod
from typing import List, Any, Callable, TypeVar

from ..llm.base_llm import BaseLLMProvider, Message
from ..config import config


T = TypeVar('T')


def require_fields(data: dict, fields: List[str], context: str = "") -> None:
    """Проверяет наличие обязательных полей в словаре."""
    for field in fields:
        if field not in data:
            prefix = f"{context}: " if context else ""
            raise ValueError(f"{prefix}отсутствует поле '{field}'")


def map_enum_value(value: str, mapping: dict, field_name: str):
    """Маппит строку в enum, кидает ошибку если значение неизвестно."""
    if value not in mapping:
        raise ValueError(f"некорректное значение {field_name}: {value}")
    return mapping[value]


class BaseAgent(ABC):
    """Базовый класс для всех агентов."""
    
    MAX_RETRY = 3
    
    def __init__(self, llm: BaseLLMProvider, name: str):
        self.llm = llm
        self.name = name
        self.system_prompt = ""
    
    @abstractmethod
    def process(self, *args, **kwargs) -> Any:
        """Основной метод обработки."""
        pass
    
    def _create_messages(
        self,
        user_prompt: str,
        conversation_history: List[Message] = None,
    ) -> List[Message]:
        """Собирает сообщения для LLM."""
        messages = [Message(role="system", content=self.system_prompt)]
        if conversation_history:
            messages.extend(conversation_history)
        messages.append(Message(role="user", content=user_prompt))
        return messages
    
    def _call_llm(
        self,
        prompt: str,
        conversation_history: List[Message] = None,
        json_mode: bool = False,
        temperature: float = None,
    ) -> str:
        """
        Вызов LLM.
        
        Аргументы:
            prompt: Текст промпта
            conversation_history: История диалога (опционально)
            json_mode: Режим JSON-ответа
            temperature: Температура генерации (None = из конфига)
            
        Возвращает:
            Ответ LLM
        """
        messages = self._create_messages(prompt, conversation_history)
        return self.llm.generate_sync(
            messages=messages,
            temperature=temperature or config.llm.temperature,
            max_tokens=config.llm.max_tokens,
            json_mode=json_mode,
        )
    
    def _clean_json(self, response: str) -> str:
        """Убирает markdown из JSON."""
        return BaseLLMProvider.clean_json_response(response)
    
    def _parse_json(self, response: str) -> dict:
        """Парсит JSON из ответа."""
        return json.loads(self._clean_json(response))
    
    def _with_retry(
        self,
        call_fn: Callable[[], str],
        parse_fn: Callable[[str], T],
        error_msg: str,
    ) -> T:
        """
        Вызов с повтором при ошибках.
        
        Аргументы:
            call_fn: Функция вызова LLM
            parse_fn: Функция парсинга ответа
            error_msg: Сообщение об ошибке после всех попыток
            
        Возвращает:
            Результат парсинга
        """
        last_error = None
        for _ in range(self.MAX_RETRY):
            try:
                return parse_fn(call_fn())
            except (json.JSONDecodeError, ValueError, KeyError) as e:
                last_error = e
        raise ValueError(f"{error_msg}: {last_error}")
