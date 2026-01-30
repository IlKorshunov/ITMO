from dataclasses import dataclass, field
from typing import Optional
from datetime import datetime


@dataclass
class Candidate:
    """Represents an interview candidate."""
    name: str
    position: str
    expected_grade: str
    experience: str
    
    interview_start: datetime = field(default_factory=datetime.now)
    interview_id: str = field(default_factory=lambda: datetime.now().strftime("%Y%m%d_%H%M%S"))
    
    def to_dict(self) -> dict:
        """Convert to dictionary for serialization."""
        return {
            "name": self.name,
            "position": self.position,
            "expected_grade": self.expected_grade,
            "experience": self.experience,
            "interview_start": self.interview_start.isoformat(),
            "interview_id": self.interview_id,
        }
    
    def get_context_string(self) -> str:
        """Get candidate info as a context string for prompts."""
        return (
            f"Кандидат: {self.name}\n"
            f"Позиция: {self.position}\n"
            f"Ожидаемый грейд: {self.expected_grade}\n"
            f"Опыт: {self.experience}"
        )
