from typing import Optional, List
from dataclasses import dataclass, fields
import time

from ..agents.interviewer import InterviewerAgent
from ..agents.observer import ObserverAgent
from ..agents.manager import ManagerAgent
from ..agents.candidate import create_candidate_agent
from ..llm.base_llm import BaseLLMProvider, Message
from ..llm.factory import create_llm_provider
from ..models.candidate import Candidate
from ..models.turn import Turn
from ..models.feedback import FinalFeedback
from ..utils.logger import InterviewLogger
from ..config import config
from .memory import ConversationMemory


@dataclass
class AutoInterviewConfig:
    max_turns: int = 10
    min_turns: int = 5
    candidate_personality: str = "strong_junior"
    knowledge_level: Optional[float] = None
    confidence: Optional[float] = None
    honesty: Optional[float] = None
    verbosity: Optional[float] = None
    off_topic_tendency: Optional[float] = None
    nervousness: Optional[float] = None
    curiosity: Optional[float] = None
    delay_between_turns: float = 0.5
    verbose: bool = False

    def get_trait_overrides(self) -> dict:
        trait_fields = {"knowledge_level", "confidence", "honesty", "verbosity", 
                        "off_topic_tendency", "nervousness", "curiosity"}
        return {f.name: getattr(self, f.name) for f in fields(self) 
                if f.name in trait_fields and getattr(self, f.name) is not None}


class AutoInterviewOrchestrator:
    def __init__(self, candidate: Candidate, llm: BaseLLMProvider = None, 
                 output_dir: str = None, auto_config: AutoInterviewConfig = None):
        self.candidate = candidate
        self.llm = llm or create_llm_provider()
        self.auto_config = auto_config or AutoInterviewConfig()
        
        self.interviewer = InterviewerAgent(self.llm)
        self.observer = ObserverAgent(self.llm)
        self.manager = ManagerAgent(self.llm)
        
        self.candidate_agent = create_candidate_agent(
            llm=self.llm,
            candidate=candidate,
            personality=self.auto_config.candidate_personality,
            **self.auto_config.get_trait_overrides(),
        )
        
        self.memory = ConversationMemory()
        self.logger = InterviewLogger(candidate, output_dir)
        
        self.current_turn_id = 0
        self.difficulty_level = config.interview.initial_difficulty
        self.is_active = False
        self.candidate_history: List[Message] = []
    
    def run_full_interview(self) -> FinalFeedback:
        greeting = self._start_interview()
        
        candidate_response = self.candidate_agent.generate_initial_response()
        self._add_to_candidate_history("assistant", greeting)
        self._add_to_candidate_history("user", candidate_response)
        
        turn_count = 1
        while turn_count < self.auto_config.max_turns:
            turn_count += 1
            
            if self.auto_config.delay_between_turns > 0:
                time.sleep(self.auto_config.delay_between_turns)
            
            interviewer_response, _ = self._process_candidate_message(candidate_response)
            
            if turn_count >= self.auto_config.max_turns:
                break
            
            self._add_to_candidate_history("assistant", interviewer_response)
            candidate_response = self.candidate_agent.process(interviewer_response, self.candidate_history)
            self._add_to_candidate_history("user", candidate_response)
            
            if turn_count >= self.auto_config.min_turns and self._should_end_naturally(turn_count):
                break
        
        return self._end_interview()
    
    def _start_interview(self) -> str:
        self.is_active = True
        self.current_turn_id = 1
        
        greeting = self.interviewer.generate_greeting(self.candidate, self.difficulty_level)
        
        turn = Turn(
            turn_id=self.current_turn_id,
            agent_visible_message=greeting,
            user_message="",
            internal_thoughts="",
        )
        self.memory.add_turn(turn)
        self.logger.log_turn(turn)
        
        return greeting
    
    def _process_candidate_message(self, candidate_message: str) -> tuple[str, bool]:
        self.current_turn_id += 1
        
        observer_analysis = self.observer.process(
            candidate=self.candidate,
            conversation_history=self.memory.get_history_list(),
            current_message=candidate_message,
            topics_covered=self.memory.topics_covered,
        )
        
        self._update_difficulty(observer_analysis)
        
        response, instruction = self.interviewer.process(
            candidate=self.candidate,
            conversation_history=self.memory.get_history_list(),
            observer_analysis=observer_analysis,
            difficulty_level=self.difficulty_level,
            topics_covered=self.memory.topics_covered,
        )
        
        turn = Turn(
            turn_id=self.current_turn_id,
            user_message=candidate_message,
            agent_visible_message=response,
            observer_analysis=observer_analysis,
            interviewer_instruction=instruction,
        )
        turn.build_internal_thoughts()
        self.memory.add_turn(turn)
        self.logger.log_turn(turn)
        
        return response, False
    
    def _end_interview(self) -> FinalFeedback:
        self.is_active = False
        
        feedback = self.manager.process(
            candidate=self.candidate,
            full_conversation=self.memory.get_history_list(),
            all_observations=self.memory.get_all_observations(),
        )
        self.logger.log_feedback(feedback)
        
        return feedback
    
    def _update_difficulty(self, analysis) -> None:
        self.memory._update_quality_tracking(analysis)
        
        if self.memory.should_increase_difficulty():
            self.difficulty_level = min(5, self.difficulty_level + 1)
            self.memory.consecutive_high_scores = 0
        elif self.memory.should_decrease_difficulty():
            self.difficulty_level = max(1, self.difficulty_level - 1)
            self.memory.consecutive_low_scores = 0
        
        if analysis.suggested_difficulty_change != 0:
            self.difficulty_level = max(1, min(5, self.difficulty_level + analysis.suggested_difficulty_change))
    
    def _should_end_naturally(self, turn_count: int) -> bool:
        return len(self.memory.topics_covered) >= 3 or turn_count >= self.auto_config.max_turns - 2
    
    def _add_to_candidate_history(self, role: str, content: str) -> None:
        self.candidate_history.append(Message(role=role, content=content))


def run_auto_interview(
    name: str = "Кандидат",
    position: str = "Developer",
    grade: str = "Junior",
    experience: str = "1 год опыта",
    personality: str = "strong_junior",
    max_turns: int = 8,
    **kwargs,
) -> FinalFeedback:
    trait_keys = {"knowledge_level", "confidence", "honesty", "verbosity", 
                  "off_topic_tendency", "nervousness", "curiosity"}
    trait_overrides = {k: v for k, v in kwargs.items() if k in trait_keys}
    llm_kwargs = {k: v for k, v in kwargs.items() if k not in trait_keys}
    
    candidate = Candidate(name=name, position=position, expected_grade=grade, experience=experience)
    llm = create_llm_provider(**llm_kwargs)
    auto_config = AutoInterviewConfig(
        max_turns=max_turns, 
        candidate_personality=personality, 
        **trait_overrides
    )
    
    orchestrator = AutoInterviewOrchestrator(candidate=candidate, llm=llm, auto_config=auto_config)
    return orchestrator.run_full_interview()
