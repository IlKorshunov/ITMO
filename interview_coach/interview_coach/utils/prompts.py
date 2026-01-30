import os
from typing import List

from ..formatting import SEPARATOR, SEPARATOR2, TAB, MINITAB, ITAB
from .prompts_content import (
    OBSERVER_SYSTEM,
    INTERVIEWER_SYSTEM,
    MANAGER_SYSTEM,
    CANDIDATE_SYSTEM_TEMPLATE,
)

class Prompts:    
    OBSERVER_SYSTEM = OBSERVER_SYSTEM
    INTERVIEWER_SYSTEM = INTERVIEWER_SYSTEM
    MANAGER_SYSTEM = MANAGER_SYSTEM
    CANDIDATE_SYSTEM_TEMPLATE = CANDIDATE_SYSTEM_TEMPLATE

    SEPARATOR = SEPARATOR
    SEPARATOR2 = SEPARATOR2
    TAB = TAB
    MINITAB = MINITAB
    ITAB = ITAB

    @staticmethod
    def format_topics(topics: list) -> str:
        return ", ".join(topics) if topics else "ещё не обсуждались"

    @staticmethod
    def format_history(history: List[str]) -> str:
        return os.linesep.join(history)

    @staticmethod
    def get_observer_prompt(
        candidate_info: str,
        conversation_history: List[str],
        current_message: str,
        topics_covered: list,
    ) -> str:
        """Промпт для Observer."""
        return f"""Проанализируй последний ответ кандидата.
{Prompts.SEPARATOR}
ИНФОРМАЦИЯ О КАНДИДАТЕ:
{candidate_info}
{Prompts.SEPARATOR}
ИСТОРИЯ ДИАЛОГА:
{Prompts.format_history(conversation_history)}
{Prompts.SEPARATOR}
ПОСЛЕДНЕЕ СООБЩЕНИЕ КАНДИДАТА:
{current_message}
{Prompts.SEPARATOR}
ТЕМЫ, КОТОРЫЕ УЖЕ ОБСУЖДАЛИСЬ:
{Prompts.format_topics(topics_covered)}
{Prompts.SEPARATOR}
Дай анализ в JSON формате."""

    @staticmethod
    def get_interviewer_prompt(
        candidate_info: str,
        conversation_history: List[str],
        observer_recommendation: str,
        difficulty_level: int,
        topics_covered: list,
        is_greeting: bool = False,
        is_first_question: bool = False,
    ) -> str:
        if is_greeting:
            return f"""Поприветствуй кандидата.
{Prompts.SEPARATOR}
ИНФОРМАЦИЯ О КАНДИДАТЕ:
{candidate_info}
{Prompts.SEPARATOR}
Представься как интервьюер и спроси, готов ли кандидат начать. Не задавай технических вопросов."""

        if is_first_question:
            return f"""Кандидат поприветствовал тебя и готов к интервью. Задай первый технический вопрос.
{Prompts.SEPARATOR}
ИНФОРМАЦИЯ О КАНДИДАТЕ:
{candidate_info}
{Prompts.SEPARATOR}
Задай технический вопрос уровня сложности {difficulty_level} (из 5) по теме позиции кандидата."""

        return f"""Продолжи интервью.
{Prompts.SEPARATOR}
ИНФОРМАЦИЯ О КАНДИДАТЕ:
{candidate_info}
{Prompts.SEPARATOR}
ИСТОРИЯ ДИАЛОГА:
{Prompts.format_history(conversation_history)}
{Prompts.SEPARATOR}
РЕКОМЕНДАЦИЯ OBSERVER:
{observer_recommendation}
{Prompts.SEPARATOR}
УРОВЕНЬ СЛОЖНОСТИ: {difficulty_level}/5
ТЕМЫ (не повторяй): {Prompts.format_topics(topics_covered)}
{Prompts.SEPARATOR}
Сгенерируй следующую реплику. Если галлюцинация - поправь. Если вопрос - ответь."""

    @staticmethod
    def get_manager_prompt(
        candidate_info: str,
        full_conversation: List[str],
        all_observations: list,
    ) -> str:
        """Промпт для Manager."""
        observations_str = os.linesep.join([
            f"Ход {i+1}: {obs}" 
            for i, obs in enumerate(all_observations)
        ])
        
        return f"""Проанализируй результаты интервью и выдай финальный фидбэк.
{Prompts.SEPARATOR}
ИНФОРМАЦИЯ О КАНДИДАТЕ:
{candidate_info}
{Prompts.SEPARATOR}
ПОЛНЫЙ ДИАЛОГ:
{Prompts.format_history(full_conversation)}
{Prompts.SEPARATOR}
ЗАМЕТКИ OBSERVER ПО КАЖДОМУ ХОДУ:
{observations_str}
{Prompts.SEPARATOR}
Сгенерируй детальный фидбэк в JSON формате. 
Обязательно укажи правильные ответы для тем, где кандидат ошибся.
Дай конкретные рекомендации по развитию."""

    @staticmethod
    def get_candidate_response_prompt(interviewer_message: str) -> str:
        """Промпт для ответа кандидата."""
        return f"""Интервьюер сказал:
{interviewer_message}
{Prompts.SEPARATOR}
Твой ответ как кандидата:"""

    @staticmethod
    def get_candidate_greeting_prompt() -> str:
        """Промпт для приветствия кандидата."""
        return f"""Интервьюер поприветствовал тебя и представился.
{Prompts.SEPARATOR}
Ответь вежливо и кратко, покажи, что готов к интервью.
Можешь немного рассказать о себе или просто подтвердить готовность."""