"""Агент-менеджер."""

from typing import List

from .base_agent import BaseAgent, require_fields, map_enum_value
from ..llm.base_llm import BaseLLMProvider
from ..models.candidate import Candidate
from ..models.feedback import (
    FinalFeedback, Decision, SkillAssessment, KnowledgeGap,
    SoftSkillsAssessment, RoadmapItem, Grade, HiringRecommendation,
)
from ..utils.prompts import Prompts


GRADE_MAP = {
    "Junior": Grade.JUNIOR,
    "Middle": Grade.MIDDLE,
    "Senior": Grade.SENIOR,
}

HIRE_MAP = {
    "Strong Hire": HiringRecommendation.STRONG_HIRE,
    "Hire": HiringRecommendation.HIRE,
    "No Hire": HiringRecommendation.NO_HIRE,
}


class ManagerAgent(BaseAgent):
    """
    Агент-менеджер.
    
    Задачи:
    1) Анализ полного диалога
    2) Оценка грейда и решение по найму
    3) Подробный фидбэк
    4) План развития
    """
    
    def __init__(self, llm: BaseLLMProvider):
        super().__init__(llm, "Manager")
        self.system_prompt = Prompts.MANAGER_SYSTEM
    
    def process(
        self,
        candidate: Candidate,
        full_conversation: List[str],
        all_observations: List[str],
    ) -> FinalFeedback:
        """
        Формирует финальный отчёт.
        До 3 попыток при ошибках парсинга.
        
        Аргументы:
            candidate: Данные кандидата
            full_conversation: Полная история диалога
            all_observations: Заметки наблюдателя
            
        Возвращает:
            Готовый FinalFeedback
        """
        prompt = Prompts.get_manager_prompt(
            candidate_info=candidate.get_context_string(),
            full_conversation=full_conversation,
            all_observations=all_observations,
        )
        
        return self._with_retry(
            call_fn=lambda: self._call_llm(prompt, json_mode=True, temperature=0.3),
            parse_fn=self._parse_response,
            error_msg="не удалось получить ответ от менеджера",
        )
    
    def _parse_response(self, response: str) -> FinalFeedback:
        """Парсит ответ в FinalFeedback."""
        data = self._parse_json(response)
        
        require_fields(data, ["decision", "confirmed_skills", "knowledge_gaps", "roadmap", "summary"])
        require_fields(data["decision"], ["grade", "hiring_recommendation"], "decision")
        
        decision = Decision(
            grade=map_enum_value(data["decision"]["grade"], GRADE_MAP, "grade"),
            hiring_recommendation=map_enum_value(data["decision"]["hiring_recommendation"], HIRE_MAP, "hiring_recommendation"),
            confidence_score=data["decision"].get("confidence_score", 50),
        )
        
        confirmed_skills = [
            SkillAssessment(topic=s.get("topic", ""), evidence=s.get("evidence", ""), score=s.get("score", 5))
            for s in data["confirmed_skills"]
        ]
        
        knowledge_gaps = [
            KnowledgeGap(
                topic=g.get("topic", ""), user_answer=g.get("user_answer", ""),
                correct_answer=g.get("correct_answer", ""), importance=g.get("importance", "medium")
            )
            for g in data["knowledge_gaps"]
        ]
        
        soft_data = data.get("soft_skills")
        soft_skills = SoftSkillsAssessment(
            clarity=soft_data.get("clarity", 5), honesty=soft_data.get("honesty", 5),
            engagement=soft_data.get("engagement", 5), notes=soft_data.get("notes")
        ) if soft_data else None
        
        roadmap = [
            RoadmapItem(
                topic=i.get("topic", ""), priority=i.get("priority", "medium"),
                resources=i.get("resources", []), description=i.get("description")
            )
            for i in data["roadmap"]
        ]
        
        return FinalFeedback(
            decision=decision,
            confirmed_skills=confirmed_skills,
            knowledge_gaps=knowledge_gaps,
            soft_skills=soft_skills,
            roadmap=roadmap,
            summary=data["summary"],
        )
