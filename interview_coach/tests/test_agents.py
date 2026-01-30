import pytest
from unittest.mock import Mock, patch
from interview_coach.agents.base_agent import BaseAgent
from interview_coach.agents.interviewer import InterviewerAgent
from interview_coach.agents.observer import ObserverAgent
from interview_coach.agents.manager import ManagerAgent
from interview_coach.agents.candidate import CandidateAgent, CandidateTraits, CandidatePresets
from interview_coach.models.candidate import Candidate
from interview_coach.models.turn import ObserverAnalysis, ResponseQuality, ConfidenceLevel


class TestCandidateTraits:
    def test_validate_correct_values(self):
        traits = CandidateTraits(knowledge_level=0.5, confidence=0.7)
        traits.validate()
    
    def test_validate_invalid_values(self):
        traits = CandidateTraits(knowledge_level=1.5)
        traits.validate()
        assert traits.knowledge_level == 1.0
    
    def test_generate_personality_description(self):
        traits = CandidatePresets.strong_junior()
        desc = traits.generate_personality_description()
        assert "кандидат" in desc.lower()
        assert "УРОВЕНЬ ЗНАНИЙ" in desc


class TestCandidatePresets:
    def test_all_presets_valid(self):
        presets = [
            CandidatePresets.strong_junior(),
            CandidatePresets.weak_junior(),
            CandidatePresets.confident_wrong(),
            CandidatePresets.nervous(),
            CandidatePresets.experienced(),
            CandidatePresets.off_topic(),
            CandidatePresets.questioner(),
        ]
        for preset in presets:
            preset.validate()


class TestInterviewerAgent:
    @patch('interview_coach.agents.interviewer.InterviewerAgent._call_llm')
    def test_generate_greeting(self, mock_llm):
        mock_llm.return_value = "Привет! Я интервьюер."
        
        llm = Mock()
        agent = InterviewerAgent(llm)
        candidate = Candidate(name="Тест", position="Dev", expected_grade="Junior", experience="1 год")
        
        greeting = agent.generate_greeting(candidate, difficulty_level=2)
        assert greeting == "Привет! Я интервьюер."
        mock_llm.assert_called_once()


class TestObserverAgent:
    def test_parse_response_valid_json(self):
        llm = Mock()
        agent = ObserverAgent(llm)
        
        json_response = '''{
            "quality_score": 7,
            "quality_level": "good",
            "confidence": "high",
            "hallucination_detected": false,
            "off_topic_detected": false,
            "candidate_question_detected": false,
            "topics_mentioned": ["Python"],
            "correct_points": ["GIL"],
            "incorrect_points": [],
            "recommendation": "Продолжать",
            "suggested_difficulty_change": 1
        }'''
        
        result = agent._parse_response(json_response)
        assert result.quality_score == 7
        assert result.quality_level == ResponseQuality.GOOD
        assert result.confidence == ConfidenceLevel.HIGH
