"""Агент-интервьюер."""

import os
from typing import List

from .base_agent import BaseAgent
from ..llm.base_llm import BaseLLMProvider
from ..models.candidate import Candidate
from ..models.turn import ObserverAnalysis, InterviewerInstruction
from ..utils.prompts import Prompts


class InterviewerAgent(BaseAgent):
    """Ведёт диалог с кандидатом, задаёт вопросы."""
    
    def __init__(self, llm: BaseLLMProvider):
        super().__init__(llm, "Interviewer")
        self.system_prompt = Prompts.INTERVIEWER_SYSTEM
    
    def generate_greeting(self, candidate: Candidate, difficulty_level: int = 3) -> str:
        prompt = Prompts.get_interviewer_prompt(
            candidate_info=candidate.get_context_string(),
            conversation_history=[],
            observer_recommendation="",
            difficulty_level=difficulty_level,
            topics_covered=[],
            is_greeting=True,
        )
        return self._call_llm(prompt, temperature=0.7)
    
    def generate_first_question(self, candidate: Candidate, difficulty_level: int = 3) -> str:
        prompt = Prompts.get_interviewer_prompt(
            candidate_info=candidate.get_context_string(),
            conversation_history=[],
            observer_recommendation="",
            difficulty_level=difficulty_level,
            topics_covered=[],
            is_first_question=True,
        )
        return self._call_llm(prompt, temperature=0.7)
    
    def process(
        self,
        candidate: Candidate,
        conversation_history: List[str],
        observer_analysis: ObserverAnalysis,
        difficulty_level: int,
        topics_covered: list,
    ) -> tuple[str, InterviewerInstruction]:
        """
        Генерирует следующую реплику.
        
        Аргументы:
            candidate: Данные кандидата
            conversation_history: История диалога
            observer_analysis: Анализ от Observer
            difficulty_level: Текущая сложность (1-5)
            topics_covered: Уже обсуждённые темы
            
        Возвращает:
            (текст ответа, инструкция)
        """
        instruction = self._build_instruction(observer_analysis, difficulty_level)
        recommendation = self._format_recommendation(observer_analysis, instruction)
        
        prompt = Prompts.get_interviewer_prompt(
            candidate_info=candidate.get_context_string(),
            conversation_history=conversation_history,
            observer_recommendation=recommendation,
            difficulty_level=instruction.difficulty_level,
            topics_covered=topics_covered,
            is_greeting=False,
        )
        
        return self._call_llm(prompt, temperature=0.7), instruction
    
    def _build_instruction(self, analysis: ObserverAnalysis, current_difficulty: int) -> InterviewerInstruction:
        """Формирует инструкцию на основе анализа."""
        action_rules = [
            (analysis.candidate_question_detected, "answer_question"),
            (analysis.hallucination_detected, "correct"),
            (analysis.off_topic_detected, "redirect"),
            (bool(analysis.suggested_follow_up), "clarify"),
        ]
        action = next((act for cond, act in action_rules if cond), "ask_question")
        
        return InterviewerInstruction(
            action=action,
            difficulty_level=max(1, min(5, current_difficulty + analysis.suggested_difficulty_change)),
            topic_suggestion=analysis.suggested_follow_up,
            specific_instruction=analysis.recommendation,
            should_answer_candidate_question=analysis.candidate_question_detected,
            candidate_question_to_answer=analysis.candidate_question,
            correction_needed=analysis.hallucination_detected,
            correction_text=analysis.hallucination_details,
        )
    
    def _format_recommendation(self, analysis: ObserverAnalysis, instruction: InterviewerInstruction) -> str:
        """Форматирует рекомендацию Observer."""
        parts = [f"Качество: {analysis.quality_score}/10", f"Рекомендация: {analysis.recommendation}"]
        
        if analysis.hallucination_detected:
            parts.extend([f"[!] Галлюцинация: {analysis.hallucination_details}", "-> Укажи на ошибку"])
        if analysis.off_topic_detected:
            parts.extend(["[!] Off-topic", "-> Верни к теме"])
        if analysis.candidate_question_detected:
            parts.extend([f"[?] Вопрос: {analysis.candidate_question}", "-> Сначала ответь"])
        if analysis.correct_points:
            parts.append(f"Верно: {', '.join(analysis.correct_points)}")
        if analysis.incorrect_points:
            parts.append(f"Ошибки: {', '.join(analysis.incorrect_points)}")
        
        parts.append(f"Сложность: {instruction.difficulty_level}/5")
        return os.linesep.join(parts)
