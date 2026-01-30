import pytest
from interview_coach.utils.prompts import Prompts


class TestPrompts:
    def test_format_topics_empty(self):
        result = Prompts.format_topics([])
        assert result == "ещё не обсуждались"
    
    def test_format_topics_with_items(self):
        result = Prompts.format_topics(["Python", "Django", "REST"])
        assert "Python" in result
        assert "Django" in result
        assert "REST" in result
    
    def test_format_history(self):
        history = ["Интервьюер: Привет", "Кандидат: Привет"]
        result = Prompts.format_history(history)
        assert "Интервьюер: Привет" in result
        assert "Кандидат: Привет" in result
    
    def test_get_observer_prompt(self):
        prompt = Prompts.get_observer_prompt(
            candidate_info="Иван, Backend",
            conversation_history=["Вопрос", "Ответ"],
            current_message="Новый ответ",
            topics_covered=["Python"]
        )
        assert "Иван, Backend" in prompt
        assert "Новый ответ" in prompt
        assert "Python" in prompt
    
    def test_get_interviewer_prompt_greeting(self):
        prompt = Prompts.get_interviewer_prompt(
            candidate_info="Тест",
            conversation_history=[],
            observer_recommendation="",
            difficulty_level=3,
            topics_covered=[],
            is_greeting=True
        )
        assert "Поприветствуй" in prompt
    
    def test_get_interviewer_prompt_first_question(self):
        prompt = Prompts.get_interviewer_prompt(
            candidate_info="Тест",
            conversation_history=[],
            observer_recommendation="",
            difficulty_level=3,
            topics_covered=[],
            is_first_question=True
        )
        assert "первый" in prompt.lower()
        assert "3" in prompt
    
    def test_get_interviewer_prompt_regular(self):
        prompt = Prompts.get_interviewer_prompt(
            candidate_info="Тест",
            conversation_history=["История"],
            observer_recommendation="Рекомендация",
            difficulty_level=4,
            topics_covered=["Python"],
            is_greeting=False
        )
        assert "Продолжи" in prompt
        assert "Рекомендация" in prompt
    
    def test_constants_exist(self):
        assert Prompts.SEPARATOR
        assert Prompts.SEPARATOR2
        assert Prompts.TAB
        assert Prompts.MINITAB
        assert Prompts.ITAB
