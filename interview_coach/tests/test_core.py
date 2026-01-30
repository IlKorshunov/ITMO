import pytest
from interview_coach.core.memory import ConversationMemory
from interview_coach.models.turn import Turn, ObserverAnalysis, ResponseQuality, ConfidenceLevel


class TestConversationMemory:
    def test_add_turn(self):
        memory = ConversationMemory()
        turn = Turn(turn_id=1, agent_visible_message="Привет", user_message="Привет")
        
        memory.add_turn(turn)
        
        assert memory.get_turn_count() == 1
    
    def test_get_conversation_history(self):
        memory = ConversationMemory()
        memory.add_turn(Turn(turn_id=1, agent_visible_message="Вопрос 1", user_message="Ответ 1"))
        memory.add_turn(Turn(turn_id=2, agent_visible_message="Вопрос 2", user_message="Ответ 2"))
        
        history = memory.get_conversation_history()
        assert "Интервьюер: Вопрос 1" in history
        assert "Кандидат: Ответ 1" in history
    
    def test_quality_tracking(self):
        memory = ConversationMemory()
        
        analysis = ObserverAnalysis(
            quality_score=8,
            quality_level=ResponseQuality.GOOD,
            confidence=ConfidenceLevel.HIGH,
            recommendation="Ок"
        )
        
        turn = Turn(turn_id=1, observer_analysis=analysis)
        memory.add_turn(turn)
        
        assert memory.get_average_score() == 8.0
    
    def test_should_increase_difficulty(self):
        memory = ConversationMemory()
        
        for i in range(3):
            analysis = ObserverAnalysis(
                quality_score=8,
                quality_level=ResponseQuality.GOOD,
                confidence=ConfidenceLevel.HIGH,
                recommendation="Хорошо"
            )
            turn = Turn(turn_id=i+1, observer_analysis=analysis)
            memory.add_turn(turn)
        
        assert memory.should_increase_difficulty()
    
    def test_topics_tracking(self):
        memory = ConversationMemory()
        
        analysis = ObserverAnalysis(
            quality_score=7,
            quality_level=ResponseQuality.GOOD,
            confidence=ConfidenceLevel.MEDIUM,
            topics_mentioned=["Python", "Django"],
            recommendation="Ок"
        )
        
        turn = Turn(turn_id=1, observer_analysis=analysis)
        memory.add_turn(turn)
        
        assert "Python" in memory.topics_covered
        assert "Django" in memory.topics_covered
