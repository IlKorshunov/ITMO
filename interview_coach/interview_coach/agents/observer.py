"""Агент-наблюдатель."""

import json

from .base_agent import BaseAgent
from ..llm.base_llm import BaseLLMProvider
from ..models.turn import ObserverAnalysis, ResponseQuality, ConfidenceLevel
from ..models.candidate import Candidate
from ..utils.prompts import Prompts


QUALITY_MAP = {
    "excellent": ResponseQuality.EXCELLENT,
    "good": ResponseQuality.GOOD,
    "acceptable": ResponseQuality.ACCEPTABLE,
    "poor": ResponseQuality.POOR,
    "wrong": ResponseQuality.WRONG,
    "off_topic": ResponseQuality.OFF_TOPIC,
    "hallucination": ResponseQuality.HALLUCINATION,
}

CONFIDENCE_MAP = {
    "high": ConfidenceLevel.HIGH,
    "medium": ConfidenceLevel.MEDIUM,
    "low": ConfidenceLevel.LOW,
    "uncertain": ConfidenceLevel.UNCERTAIN,
}


class ObserverAgent(BaseAgent):
    """
    Агент-наблюдатель.
    
    Задачи:
    1) Оценка качества ответов (1-10)
    2) Обнаружение галлюцинаций и ухода от темы
    3) Фиксация вопросов кандидата
    4) Рекомендации интервьюеру
    """
    
    def __init__(self, llm: BaseLLMProvider):
        super().__init__(llm, "Observer")
        self.system_prompt = Prompts.OBSERVER_SYSTEM
    
    def process(
        self,
        candidate: Candidate,
        conversation_history: list,
        current_message: str,
        topics_covered: list,
    ) -> ObserverAnalysis:
        """
        Анализирует последний ответ кандидата.
        
        Аргументы:
            candidate: Данные кандидата
            conversation_history: История диалога
            current_message: Последнее сообщение кандидата
            topics_covered: Уже обсуждённые темы
            
        Возвращает:
            Результат анализа с рекомендациями
        """
        prompt = Prompts.get_observer_prompt(
            candidate_info=candidate.get_context_string(),
            conversation_history=conversation_history,
            current_message=current_message,
            topics_covered=topics_covered,
        )
        
        response = self._call_llm(prompt, json_mode=True, temperature=0.3)
        return self._parse_response(response)
    
    def _parse_response(self, response: str) -> ObserverAnalysis:
        """Парсит ответ в ObserverAnalysis."""
        try:
            data = self._parse_json(response)
            
            return ObserverAnalysis(
                quality_score=data.get("quality_score", 5),
                quality_level=QUALITY_MAP.get(data.get("quality_level", "acceptable"), ResponseQuality.ACCEPTABLE),
                confidence=CONFIDENCE_MAP.get(data.get("confidence", "medium"), ConfidenceLevel.MEDIUM),
                hallucination_detected=data.get("hallucination_detected", False),
                hallucination_details=data.get("hallucination_details"),
                off_topic_detected=data.get("off_topic_detected", False),
                candidate_question_detected=data.get("candidate_question_detected", False),
                candidate_question=data.get("candidate_question"),
                topics_mentioned=data.get("topics_mentioned", []),
                correct_points=data.get("correct_points", []),
                incorrect_points=data.get("incorrect_points", []),
                recommendation=data.get("recommendation", ""),
                suggested_difficulty_change=data.get("suggested_difficulty_change", 0),
                suggested_follow_up=data.get("suggested_follow_up"),
            )
        except json.JSONDecodeError as e:
            return ObserverAnalysis(
                quality_score=5,
                quality_level=ResponseQuality.ACCEPTABLE,
                confidence=ConfidenceLevel.MEDIUM,
                recommendation=f"Ошибка анализа: {e}",
            )
