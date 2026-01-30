import pytest
from interview_coach.models.candidate import Candidate
from interview_coach.models.feedback import (
    FinalFeedback, Decision, Grade, HiringRecommendation,
    SkillAssessment, KnowledgeGap, RoadmapItem
)
from interview_coach.models.turn import (
    ObserverAnalysis, InterviewerInstruction,
    ResponseQuality, ConfidenceLevel, Turn
)


class TestCandidate:
    def test_create_candidate(self):
        c = Candidate(name="Иван", position="Backend", expected_grade="Junior", experience="1 год Python")
        assert c.name == "Иван"
        assert c.position == "Backend"
    
    def test_context_string(self):
        c = Candidate(name="Иван", position="Backend", expected_grade="Junior", experience="1 год")
        ctx = c.get_context_string()
        assert "Иван" in ctx
        assert "Backend" in ctx


class TestFeedback:
    def test_decision_to_dict(self):
        d = Decision(grade=Grade.JUNIOR, hiring_recommendation=HiringRecommendation.HIRE, confidence_score=80)
        result = d.to_dict()
        assert result["grade"] == "Junior"
        assert result["hiring_recommendation"] == "Hire"
        assert result["confidence_score"] == 80
    
    def test_final_feedback_to_dict(self):
        decision = Decision(grade=Grade.MIDDLE, hiring_recommendation=HiringRecommendation.STRONG_HIRE, confidence_score=90)
        skills = [SkillAssessment(topic="Python", evidence="Хорошо объяснил", score=8)]
        gaps = [KnowledgeGap(topic="Docker", user_answer="Не знаю", correct_answer="Контейнеризация")]
        
        fb = FinalFeedback(decision=decision, confirmed_skills=skills, knowledge_gaps=gaps, summary="Хороший кандидат")
        result = fb.to_dict()
        
        assert result["decision"]["grade"] == "Middle"
        assert len(result["technical_review"]["confirmed_skills"]) == 1
        assert len(result["technical_review"]["knowledge_gaps"]) == 1


class TestTurn:
    def test_observer_analysis_to_internal_thoughts(self):
        analysis = ObserverAnalysis(
            quality_score=7,
            quality_level=ResponseQuality.GOOD,
            confidence=ConfidenceLevel.HIGH,
            hallucination_detected=True,
            hallucination_details="Неверная дата выхода Python 3",
            recommendation="Уточнить знания"
        )
        thoughts = analysis.to_internal_thoughts()
        assert "7/10" in thoughts
        assert "HALLUCINATION" in thoughts
    
    def test_interviewer_instruction_to_internal_thoughts(self):
        instruction = InterviewerInstruction(
            action="ask_question",
            difficulty_level=3,
            correction_needed=False
        )
        thoughts = instruction.to_internal_thoughts()
        assert "сложность 3/5" in thoughts
    
    def test_turn_build_internal_thoughts(self):
        analysis = ObserverAnalysis(
            quality_score=5,
            quality_level=ResponseQuality.ACCEPTABLE,
            confidence=ConfidenceLevel.MEDIUM,
            recommendation="Продолжать"
        )
        instruction = InterviewerInstruction(action="clarify", difficulty_level=2)
        
        turn = Turn(turn_id=1, observer_analysis=analysis, interviewer_instruction=instruction)
        turn.build_internal_thoughts()
        
        assert "[Observer]" in turn.internal_thoughts
        assert "[Interviewer]" in turn.internal_thoughts
