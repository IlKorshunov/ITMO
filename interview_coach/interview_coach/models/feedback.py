from dataclasses import dataclass, field
from typing import List, Optional
from enum import Enum
import os

from ..formatting import SEPARATOR, SEPARATOR2, TAB, MINITAB, ITAB

class HiringRecommendation(Enum):
    """Hiring recommendation levels."""
    STRONG_HIRE = "Strong Hire"
    HIRE = "Hire"
    NO_HIRE = "No Hire"


class Grade(Enum):
    """Candidate grade levels."""
    JUNIOR = "Junior"
    MIDDLE = "Middle"
    SENIOR = "Senior"

@dataclass
class Decision:
    """Final hiring decision."""
    grade: Grade
    hiring_recommendation: HiringRecommendation
    confidence_score: int  
    
    def to_dict(self) -> dict:
        return {
            "grade": self.grade.value,
            "hiring_recommendation": self.hiring_recommendation.value,
            "confidence_score": self.confidence_score,
        }


@dataclass
class SkillAssessment:
    """Assessment of a confirmed skill."""
    topic: str
    evidence: str
    score: int = 0 
    
    def to_dict(self) -> dict:
        return {
            "topic": self.topic,
            "evidence": self.evidence,
            "score": self.score,
        }


@dataclass
class KnowledgeGap:
    """Identified knowledge gap."""
    topic: str
    user_answer: str
    correct_answer: str
    importance: str = "medium"
    
    def to_dict(self) -> dict:
        return {
            "topic": self.topic,
            "user_answer": self.user_answer,
            "correct_answer": self.correct_answer,
            "importance": self.importance,
        }


@dataclass
class SoftSkillsAssessment:
    """Assessment of soft skills."""
    clarity: int 
    honesty: int
    engagement: int
    notes: Optional[str] = None
    
    def to_dict(self) -> dict:
        return {
            "clarity": self.clarity,
            "honesty": self.honesty,
            "engagement": self.engagement,
            "notes": self.notes,
        }


@dataclass
class RoadmapItem:
    """Learning roadmap item."""
    topic: str
    priority: str = "medium"
    resources: List[str] = field(default_factory=list)
    description: Optional[str] = None
    
    def to_dict(self) -> dict:
        return {
            "topic": self.topic,
            "priority": self.priority,
            "resources": self.resources,
            "description": self.description,
        }


@dataclass
class FinalFeedback:
    """Complete final feedback structure."""
    decision: Decision
    confirmed_skills: List[SkillAssessment] = field(default_factory=list)
    knowledge_gaps: List[KnowledgeGap] = field(default_factory=list)
    soft_skills: Optional[SoftSkillsAssessment] = None
    roadmap: List[RoadmapItem] = field(default_factory=list)
    summary: str = ""
    
    def to_dict(self) -> dict:
        return {
            "decision": self.decision.to_dict(),
            "technical_review": {
                "confirmed_skills": [s.to_dict() for s in self.confirmed_skills],
                "knowledge_gaps": [g.to_dict() for g in self.knowledge_gaps],
            },
            "soft_skills": self.soft_skills.to_dict() if self.soft_skills else None,
            "roadmap": [r.to_dict() for r in self.roadmap],
            "summary": self.summary,
        }
    
    def to_formatted_string(self) -> str:
        """Форматирование красивого вывода"""
        lines = [SEPARATOR, f"{TAB}ФИНАЛЬНЫЙ ФИДБЭК ИНТЕРВЬЮ", SEPARATOR, "", "ВЕРДИКТ", SEPARATOR2, f"{MINITAB}Уровень: {self.decision.grade.value}", f"{MINITAB}Рекомендация: {self.decision.hiring_recommendation.value}", f"{MINITAB}Уверенность: {self.decision.confidence_score}%", "", "ТЕХНИЧЕСКИЕ НАВЫКИ", SEPARATOR2]
        
        if self.confirmed_skills:
            lines.append(f"{MINITAB}Подтвержденные навыки:")
            for skill in self.confirmed_skills: lines.append(f"{TAB}{ITAB}{skill.topic}: {skill.evidence}")
        
        if self.knowledge_gaps:
            lines.append("")
            lines.append(f"{MINITAB}Пробелы в знаниях:")
            for gap in self.knowledge_gaps:
                lines.append(f"{TAB}{ITAB}{gap.topic}")
                lines.append(f"{TAB}{TAB}Ваш ответ: {gap.user_answer}")
                lines.append(f"{TAB}{TAB}Правильный ответ: {gap.correct_answer}")
        
        if self.soft_skills:
            lines.extend(["", "Soft skills", SEPARATOR2, f"{MINITAB}Ясность: {self.soft_skills.clarity}/10", f"{MINITAB}Честность: {self.soft_skills.honesty}/10", f"{MINITAB}Вовлеченность: {self.soft_skills.engagement}/10"])
            if self.soft_skills.notes: lines.append(f"{MINITAB}Примечание: {self.soft_skills.notes}")
        
        if self.roadmap:
            lines.extend(["", "Для развития", SEPARATOR2])
            for item in self.roadmap:
                lines.append(f"{MINITAB}{ITAB}{item.topic} [{item.priority}]")
                if item.description: lines.append(f"{TAB}{item.description}")
                for resource in item.resources: lines.append(f"{TAB}- {resource}")
        
        if self.summary: lines.extend(["", "РЕЗЮМЕ", SEPARATOR2, f"{MINITAB}{self.summary}"])
        lines.extend(["", SEPARATOR])
        return os.linesep.join(lines)