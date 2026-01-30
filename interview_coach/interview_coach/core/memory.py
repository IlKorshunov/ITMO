from dataclasses import dataclass, field
from typing import List

from ..models.turn import Turn, ObserverAnalysis


@dataclass
class ConversationMemory:
    """Хранит историю диалога и отслеживает качество ответов."""
    
    turns: List[Turn] = field(default_factory=list)
    topics_covered: List[str] = field(default_factory=list)
    questions_asked: List[str] = field(default_factory=list)
    quality_history: List[int] = field(default_factory=list)
    consecutive_high_scores: int = 0
    consecutive_low_scores: int = 0
    
    def add_turn(self, turn: Turn) -> None:
        """Добавляет ход в историю."""
        self.turns.append(turn)
        if turn.observer_analysis:
            self._update_quality_tracking(turn.observer_analysis)
    
    def _update_quality_tracking(self, analysis: ObserverAnalysis) -> None:
        """Обновляет статистику качества."""
        score = analysis.quality_score
        self.quality_history.append(score)
        
        if len(self.quality_history) > 10:
            self.quality_history = self.quality_history[-10:]
        
        if score >= 7:
            self.consecutive_high_scores += 1
            self.consecutive_low_scores = 0
        elif score <= 4:
            self.consecutive_low_scores += 1
            self.consecutive_high_scores = 0
        else:
            self.consecutive_high_scores = 0
            self.consecutive_low_scores = 0
        
        for topic in analysis.topics_mentioned:
            if topic not in self.topics_covered:
                self.topics_covered.append(topic)
    
    def add_question(self, question: str) -> None:
        """Добавляет заданный вопрос."""
        self.questions_asked.append(question)
    
    def get_history_list(self, last_n: int = None) -> List[str]:
        """Возвращает историю как список строк."""
        turns = self.turns[-last_n:] if last_n else self.turns
        lines = []
        for turn in turns:
            if turn.agent_visible_message:
                lines.append(f"Интервьюер: {turn.agent_visible_message}")
            if turn.user_message:
                lines.append(f"Кандидат: {turn.user_message}")
        return lines

    def get_conversation_history(self, last_n: int = None) -> str:
        """Возвращает историю как строку."""
        return "\n\n".join(self.get_history_list(last_n))
    
    def get_all_observations(self) -> List[str]:
        """Возвращает все заметки Observer."""
        return [t.observer_analysis.to_internal_thoughts() for t in self.turns if t.observer_analysis]
    
    def should_increase_difficulty(self) -> bool:
        return self.consecutive_high_scores >= 3
    
    def should_decrease_difficulty(self) -> bool:
        return self.consecutive_low_scores >= 2
    
    def get_average_score(self) -> float:
        return sum(self.quality_history) / len(self.quality_history) if self.quality_history else 5.0
    
    def get_turn_count(self) -> int:
        return len(self.turns)
    
    def to_dict(self) -> dict:
        return {
            "turns": [t.to_dict() for t in self.turns],
            "topics_covered": self.topics_covered,
            "questions_asked": self.questions_asked,
            "average_score": self.get_average_score(),
        }
