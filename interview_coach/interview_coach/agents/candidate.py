from typing import List
from dataclasses import dataclass

from .base_agent import BaseAgent
from ..llm.base_llm import BaseLLMProvider, Message
from ..models.candidate import Candidate
from ..utils.prompts import Prompts


@dataclass
class CandidateTraits:
    """Черты личности кандидата (0.0-1.0)."""
    knowledge_level: float = 0.6
    confidence: float = 0.5    
    honesty: float = 0.8    
    verbosity: float = 0.5    
    off_topic_tendency: float = 0.2    
    nervousness: float = 0.3    
    curiosity: float = 0.4
    
    def validate(self):
        """Проверяет корректность значений."""
        for name, value in self.__dict__.items():
            if value < 0.0:
                setattr(self, name, 0.0)
            elif value > 1.0:
                setattr(self, name, 1.0)
        
    def generate_personality_description(self) -> str:
        """Генерирует описание личности."""
        return f"""Ты - кандидат со следующими чертами личности:

УРОВЕНЬ ЗНАНИЙ: {self._desc(self.knowledge_level, ["Практически не знаком с технологиями", "Базовые знания, много пробелов", "Средний уровень, знает основы", "Хорошие знания, решает задачи", "Глубокие знания, опытный"])}
УРОВЕНЬ ЧЕСТНОСТИ: {self._desc(self.honesty, ["Склонен выдумывать.", "Иногда скрывает пробелы.", "Обычно признаёт незнание.", "Всегда честен.", "Максимально честен."])}

МАНЕРА ОБЩЕНИЯ: {self._desc(self.verbosity, ["Односложные ответы", "Лаконичные ответы", "Развёрнутые ответы", "Подробные объяснения", "Очень многословный"])}
УРОВЕНЬ БОЛТЛИОВОСТИ: {self._desc(self.off_topic_tendency, ["Строго по теме.", "Иногда добавляет детали.", "Периодически отклоняется.", "Часто уходит в сторону.", "Постоянно off-topic."])} {self._desc(self.curiosity, ["Редко задаёт вопросы.", "Иногда уточняет.", "Периодически спрашивает.", "Часто интересуется.", "Постоянно задаёт вопросы."])}

ЭМОЦИОНАЛЬНОЕ СОСТОЯНИЕ: {self._desc(self.nervousness, ["Спокоен", "Немного волнуется", "Заметно нервничает", "Сильно волнуется", "Очень нервный"])}
УРОВЕНЬ УВЕРЕННОСТИ: {self._desc(self.confidence, ["Неуверен, сомневается.", "Немного неуверен.", "Умеренная уверенность.", "Уверенный тон.", "Очень самоуверенный."])}

ПОВЕДЕНИЕ: На основе своих черт веди себя естественно."""
    
    def _desc(self, value: float, options: list) -> str:
        return options[min(int(value * len(options)), len(options) - 1)]


class CandidatePresets:
    """Готовые пресеты личностей."""
    
    @staticmethod
    def strong_junior() -> CandidateTraits:
        return CandidateTraits(knowledge_level=0.6, confidence=0.6, honesty=0.9, verbosity=0.5, off_topic_tendency=0.1, nervousness=0.2, curiosity=0.5)
    
    @staticmethod
    def weak_junior() -> CandidateTraits:
        return CandidateTraits(knowledge_level=0.3, confidence=0.3, honesty=0.8, verbosity=0.4, off_topic_tendency=0.3, nervousness=0.5, curiosity=0.3)
    
    @staticmethod
    def confident_wrong() -> CandidateTraits:
        return CandidateTraits(knowledge_level=0.4, confidence=0.9, honesty=0.2, verbosity=0.7, off_topic_tendency=0.2, nervousness=0.1, curiosity=0.3)
    
    @staticmethod
    def nervous() -> CandidateTraits:
        return CandidateTraits(knowledge_level=0.5, confidence=0.3, honesty=0.8, verbosity=0.4, off_topic_tendency=0.2, nervousness=0.8, curiosity=0.2)
    
    @staticmethod
    def experienced() -> CandidateTraits:
        return CandidateTraits(knowledge_level=0.85, confidence=0.7, honesty=0.9, verbosity=0.6, off_topic_tendency=0.1, nervousness=0.1, curiosity=0.6)
    
    @staticmethod
    def off_topic() -> CandidateTraits:
        return CandidateTraits(knowledge_level=0.5, confidence=0.5, honesty=0.6, verbosity=0.8, off_topic_tendency=0.9, nervousness=0.3, curiosity=0.4)
    
    @staticmethod
    def questioner() -> CandidateTraits:
        return CandidateTraits(knowledge_level=0.6, confidence=0.6, honesty=0.8, verbosity=0.6, off_topic_tendency=0.2, nervousness=0.2, curiosity=0.9)


PRESET_MAP = {
    "strong_junior": CandidatePresets.strong_junior,
    "weak_junior": CandidatePresets.weak_junior,
    "confident_wrong": CandidatePresets.confident_wrong,
    "nervous": CandidatePresets.nervous,
    "experienced": CandidatePresets.experienced,
    "off_topic": CandidatePresets.off_topic,
    "questioner": CandidatePresets.questioner,
}


class CandidateAgent(BaseAgent):
    """LLM симулирует ответы кандидата на собеседовании."""
    
    def __init__(self, llm: BaseLLMProvider, candidate_info: Candidate, traits: CandidateTraits = None):
        super().__init__(llm, "Candidate")
        self.candidate_info = candidate_info
        self.traits = traits or CandidatePresets.strong_junior()
        self.traits.validate()
        
        self.system_prompt = Prompts.CANDIDATE_SYSTEM_TEMPLATE.format(
            position=candidate_info.position,
            name=candidate_info.name,
            experience=candidate_info.experience,
            grade=candidate_info.expected_grade,
            personality_description=self.traits.generate_personality_description(),
            knowledge_level=int(self.traits.knowledge_level * 10),
            verbosity=int(self.traits.verbosity * 10),
            honesty=int(self.traits.honesty * 10),
        )
    
    def process(self, interviewer_message: str, conversation_history: List[Message] = None) -> str:
        """
        Генерирует ответ на реплику интервьюера.
        
        Аргументы:
            interviewer_message: Сообщение интервьюера
            conversation_history: История диалога (для контекста)
            
        Возвращает:
            Ответ кандидата
        """
        return self._call_llm(
            Prompts.get_candidate_response_prompt(interviewer_message),
            conversation_history=conversation_history,
            temperature=0.8,
        )
    
    def generate_initial_response(self) -> str:
        """Генерирует ответ на приветствие."""
        return self._call_llm(Prompts.get_candidate_greeting_prompt(), temperature=0.7)


def create_candidate_agent(
    llm: BaseLLMProvider,
    candidate: Candidate,
    personality: str = "strong_junior",
    **trait_overrides,
) -> CandidateAgent:
    """
    Создаёт агента-кандидата.
    
    Аргументы:
        llm: LLM провайдер
        candidate: Данные кандидата
        personality: Пресет личности (strong_junior, weak_junior, confident_wrong, nervous, experienced, off_topic, questioner) или "custom"
        knowledge_level: (0.0-1.0) Уровень знаний
        confidence: (0.0-1.0) Уверенность в себе
        honesty: (0.0-1.0) Честность
        verbosity: (0.0-1.0) Многословность
        off_topic_tendency: (0.0-1.0) Склонность уходить от темы
        nervousness: (0.0-1.0) Нервозность
        curiosity: (0.0-1.0) Любопытство
        
    Возвращает:
        Настроенный CandidateAgent
    """
    if personality == "custom":
        traits = CandidateTraits(**trait_overrides)
    else:
        traits = PRESET_MAP.get(personality, CandidatePresets.strong_junior)()
        for key, value in trait_overrides.items():
            if hasattr(traits, key):
                setattr(traits, key, value)
    
    return CandidateAgent(llm, candidate, traits)
