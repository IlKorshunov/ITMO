import json
import os
from typing import List, Optional, Type, TypeVar

from pydantic import BaseModel
from langchain_core.messages import HumanMessage, SystemMessage, AIMessage, BaseMessage
from langchain_huggingface import HuggingFaceEndpoint, ChatHuggingFace

from .base_llm import BaseLLMProvider, Message
from ..config import config
from ..utils.prompts_content import (
    RELEVANCE_CLASSIFIER_SYSTEM,
    HIRING_CLASSIFIER_SYSTEM,
    ANSWER_EVALUATOR_SYSTEM,
)

T = TypeVar('T', bound=BaseModel)


class LangChainHuggingFaceProvider(BaseLLMProvider):
    """провайдер для HuggingFace Inference API."""
    
    def __init__(self, api_token: str = None, model: str = None, temperature: float = 0.7, max_tokens: int = 1024):
        self.api_token = api_token or config.llm.hf_api_token
        self.model = model or config.llm.hf_model
        self.temperature = temperature
        self.max_tokens = max_tokens
        
        if not self.api_token:
            raise ValueError("Требуется HuggingFace API token")
        
        self.llm = HuggingFaceEndpoint(
            repo_id=self.model,
            huggingfacehub_api_token=self.api_token,
            temperature=self.temperature,
            max_new_tokens=self.max_tokens,
            task="text-generation",
        )
        self.chat_model = ChatHuggingFace(llm=self.llm, token=self.api_token)
    
    def _convert_messages(self, messages: List[Message]) -> List[BaseMessage]:
        """Конвертирует Message в LangChain формат."""
        role_map = {
            "system": SystemMessage,
            "user": HumanMessage,
            "assistant": AIMessage,
        }
        return [role_map[msg.role](content=msg.content) for msg in messages if msg.role in role_map]
    
    def _add_json_instruction(self, messages: List[BaseMessage], schema: str = None):
        """Добавляет инструкцию JSON к последнему сообщению пользователя."""
        if not messages or not isinstance(messages[-1], HumanMessage):
            return

        instruction = f"{os.linesep}{os.linesep}Ответь ТОЛЬКО валидным JSON без markdown."
        if schema:
            instruction = (
                f"{os.linesep}Ответь в формате JSON по схеме:{os.linesep}"
                f"{schema}{os.linesep}{os.linesep}"
                f"ВАЖНО: Только валидный JSON, без markdown."
            )
        messages[-1].content += instruction

    async def generate(
        self, messages: List[Message], temperature: float = 0.7,
        max_tokens: int = 1024, json_mode: bool = False,
    ) -> str:
        return self.generate_sync(messages, temperature, max_tokens, json_mode)
    
    def generate_sync(
        self, messages: List[Message], temperature: float = 0.7,
        max_tokens: int = 1024, json_mode: bool = False,
    ) -> str:
        """Синхронная генерация через LangChain."""
        lc_messages = self._convert_messages(messages)
        
        if json_mode:
            self._add_json_instruction(lc_messages)
        
        try:
            response = self.chat_model.invoke(lc_messages)
            return BaseLLMProvider.clean_response(response.content)
        except Exception as e:
            prompt = self._format_as_prompt(messages)
            if json_mode:
                prompt += f"{os.linesep}{os.linesep}Ответь ТОЛЬКО валидным JSON без markdown."
            try:
                return BaseLLMProvider.clean_response(self.llm.invoke(prompt))
            except Exception as fallback_error:
                raise RuntimeError(f"LangChain HF error: {e}. Fallback: {fallback_error}")
    
    def _format_as_prompt(self, messages: List[Message]) -> str:
        """Форматирует сообщения как текст."""
        parts = [f"{msg.role.capitalize()}: {msg.content}" for msg in messages]
        parts.append("Assistant:")
        return f"{os.linesep}{os.linesep}".join(parts)
    
    def generate_structured(self, messages: List[Message], output_schema: Type[T], temperature: float = 0.3) -> T:
        """Генерация структурированного вывода."""
        lc_messages = self._convert_messages(messages)
        schema_json = json.dumps(output_schema.model_json_schema(), indent=2, ensure_ascii=False)
        self._add_json_instruction(lc_messages, schema=schema_json)
        
        try:
            response = self.chat_model.invoke(lc_messages)
            content = self.clean_json_response(response.content)
            return output_schema(**json.loads(content))
        except Exception as e:
            raise RuntimeError(f"Structured output error: {e}")


class RelevanceClassification(BaseModel):
    is_relevant: bool
    confidence: float
    explanation: str


class HiringDecision(BaseModel):
    decision: str
    confidence: float
    grade: str
    reasoning: str
    key_strengths: List[str]
    key_weaknesses: List[str]


class AnswerQuality(BaseModel):
    score: int
    quality_level: str
    has_hallucination: bool
    hallucination_details: Optional[str] = None
    is_off_topic: bool
    correct_points: List[str]
    incorrect_points: List[str]
    recommendation: str


def create_relevance_classifier(llm: LangChainHuggingFaceProvider):
    def classify(question: str, answer: str) -> RelevanceClassification:
        messages = [
            Message(role="system", content=RELEVANCE_CLASSIFIER_SYSTEM),
            Message(role="user", content=f"Вопрос: {question}{os.linesep}Ответ: {answer}{os.linesep}{os.linesep}Оцени релевантность.")
        ]
        return llm.generate_structured(messages, RelevanceClassification)
    return classify


def create_hiring_classifier(llm: LangChainHuggingFaceProvider):
    def classify(interview_summary: str, position: str, expected_grade: str) -> HiringDecision:
        messages = [
            Message(role="system", content=HIRING_CLASSIFIER_SYSTEM),
            Message(role="user", content=f"Позиция: {position}{os.linesep}Грейд: {expected_grade}{os.linesep}{os.linesep}Резюме:{os.linesep}{interview_summary}{os.linesep}{os.linesep}Вынеси решение.")
        ]
        return llm.generate_structured(messages, HiringDecision)
    return classify


def create_answer_evaluator(llm: LangChainHuggingFaceProvider):
    def evaluate(question: str, answer: str, context: str = "") -> AnswerQuality:
        messages = [
            Message(role="system", content=ANSWER_EVALUATOR_SYSTEM),
            Message(role="user", content=f"Контекст: {context or 'Нет'}{os.linesep}{os.linesep}Вопрос: {question}{os.linesep}Ответ: {answer}{os.linesep}{os.linesep}Оцени ответ.")
        ]
        return llm.generate_structured(messages, AnswerQuality)
    return evaluate
