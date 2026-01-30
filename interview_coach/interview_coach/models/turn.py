from dataclasses import dataclass, field
from typing import Optional, List
from datetime import datetime
from enum import Enum


class ResponseQuality(Enum):
    EXCELLENT = "excellent"
    GOOD = "good"
    ACCEPTABLE = "acceptable"
    POOR = "poor"
    WRONG = "wrong"
    OFF_TOPIC = "off_topic"
    HALLUCINATION = "hallucination"


class ConfidenceLevel(Enum):
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"
    UNCERTAIN = "uncertain"


class LoggableModel:
    """Абстрактный класс для моделей, которые можно логировать."""
    
    def _build_log_string(self, base_part: str, checks: list) -> str:
        """Собирает строку лога из базовой части и проверок."""
        parts = [base_part]
        
        for condition, text in checks:
            if condition:
                value = text() if callable(text) else text
                parts.append(value)
                
        return ". ".join(parts) + "."


@dataclass
class ObserverAnalysis(LoggableModel):
    """Анализ ответа кандидата"""
    quality_score: int
    quality_level: ResponseQuality
    confidence: ConfidenceLevel
    
    hallucination_detected: bool = False
    hallucination_details: Optional[str] = None
    off_topic_detected: bool = False
    candidate_question_detected: bool = False
    candidate_question: Optional[str] = None
    
    topics_mentioned: List[str] = field(default_factory=list)
    correct_points: List[str] = field(default_factory=list)
    incorrect_points: List[str] = field(default_factory=list)
    
    recommendation: str = ""
    suggested_difficulty_change: int = 0  # delta = 1
    suggested_follow_up: Optional[str] = None
    
    def to_dict(self) -> dict:
        return {
            "quality_score": self.quality_score,
            "quality_level": self.quality_level.value,
            "confidence": self.confidence.value,
            "hallucination_detected": self.hallucination_detected,
            "hallucination_details": self.hallucination_details,
            "off_topic_detected": self.off_topic_detected,
            "candidate_question_detected": self.candidate_question_detected,
            "candidate_question": self.candidate_question,
            "topics_mentioned": self.topics_mentioned,
            "correct_points": self.correct_points,
            "incorrect_points": self.incorrect_points,
            "recommendation": self.recommendation,
            "suggested_difficulty_change": self.suggested_difficulty_change,
            "suggested_follow_up": self.suggested_follow_up,
        }
    
    def to_internal_thoughts(self) -> str:
        """Форматирует анализ для логов."""
        base = f"Оценка: {self.quality_score}/10 ({self.quality_level.value})"
        
        checks = [
            (self.hallucination_detected, f"[HALLUCINATION] {self.hallucination_details}"),
            (self.off_topic_detected, "[OFF-TOPIC] Уход от темы"),
            (self.candidate_question_detected, f"Вопрос кандидата: '{self.candidate_question}'"),
            (self.correct_points, lambda: f"Верно: {', '.join(self.correct_points[:2])}"),
            (self.incorrect_points, lambda: f"Ошибки: {', '.join(self.incorrect_points[:2])}"),
            (True, f"Рекомендация: {self.recommendation}"),
        ]
        
        return self._build_log_string(base, checks)


@dataclass
class InterviewerInstruction(LoggableModel):
    """Формирование инструкций для интервьюера"""
    action: str
    difficulty_level: int 
    topic_suggestion: Optional[str] = None
    specific_instruction: Optional[str] = None
    should_answer_candidate_question: bool = False
    candidate_question_to_answer: Optional[str] = None
    correction_needed: bool = False
    correction_text: Optional[str] = None
    
    def to_dict(self) -> dict:
        """Convert to dictionary."""
        return {
            "action": self.action,
            "difficulty_level": self.difficulty_level,
            "topic_suggestion": self.topic_suggestion,
            "specific_instruction": self.specific_instruction,
            "should_answer_candidate_question": self.should_answer_candidate_question,
            "candidate_question_to_answer": self.candidate_question_to_answer,
            "correction_needed": self.correction_needed,
            "correction_text": self.correction_text,
        }
    
    def to_internal_thoughts(self) -> str:
        """Форматирует инструкцию для логов."""
        action_map = {
            "ask_question": "Задам вопрос",
            "answer_question": "Отвечу на вопрос",
            "clarify": "Уточню",
            "correct": "Поправлю",
            "redirect": "Верну к теме",
        }
        text = action_map.get(self.action, self.action)
        base = f"{text} (сложность {self.difficulty_level}/5)"
        checks = [(self.correction_needed, f"Исправление: '{str(self.correction_text)[:50]}...'"), (self.should_answer_candidate_question, "Сначала ответ на вопрос"), (self.specific_instruction, self.specific_instruction)]
        return self._build_log_string(base, checks)


@dataclass
class Turn:
    """класс раунда в диалоге"""
    turn_id: int
    timestamp: datetime = field(default_factory=datetime.now)
    
    user_message: str = ""
    agent_visible_message: str = ""
    
    observer_analysis: Optional[ObserverAnalysis] = None
    interviewer_instruction: Optional[InterviewerInstruction] = None
    
    internal_thoughts: str = ""
    
    def to_dict(self) -> dict:
        return {
            "turn_id": self.turn_id,
            "timestamp": self.timestamp.isoformat(),
            "agent_visible_message": self.agent_visible_message,
            "user_message": self.user_message,
            "internal_thoughts": self.internal_thoughts,
        }
    
    def build_internal_thoughts(self) -> str:
        """
        Преобразование рассуждений в формат диалога
        """
        dialogue_parts = []
        
        if self.observer_analysis:
            observer_text = self.observer_analysis.to_internal_thoughts()
            dialogue_parts.append(f"[Observer]: {observer_text}")
        
        if self.interviewer_instruction:
            interviewer_text = self.interviewer_instruction.to_internal_thoughts()
            dialogue_parts.append(f"[Interviewer]: {interviewer_text}")
        
        self.internal_thoughts = " ".join(dialogue_parts)
        return self.internal_thoughts
