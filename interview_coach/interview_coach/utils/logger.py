import json
import os
from datetime import datetime
from typing import List, Optional

from ..models.candidate import Candidate
from ..models.turn import Turn
from ..models.feedback import FinalFeedback
from ..config import config


class InterviewLogger:
    def __init__(self, candidate: Candidate, output_dir: str = None, use_timestamp: bool = False):
        self.candidate = candidate
        self.output_dir = output_dir or config.output_dir
        self.turns: List[dict] = []
        self.final_feedback: Optional[dict] = None
        
        os.makedirs(self.output_dir, exist_ok=True)
        
        if use_timestamp:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            safe_name = "".join(c if c.isalnum() else "_" for c in candidate.name)
            self.filename = f"interview_log_{safe_name}_{timestamp}.json"
        else:
            self.filename = "interview_log.json"
        
        self.filepath = os.path.join(self.output_dir, self.filename)
    
    def log_turn(self, turn: Turn) -> None:
        self.turns.append(turn.to_dict())
        self._save()
    
    def log_feedback(self, feedback: FinalFeedback) -> None:
        self.final_feedback = feedback.to_dict()
        self._save()
    
    def _save(self) -> None:
        data = {
            "turns": self.turns,
            "final_feedback": self.final_feedback,
        }
        
        with open(self.filepath, "w", encoding="utf-8") as f:
            json.dump(data, f, ensure_ascii=False, indent=2)
    
    def get_filepath(self) -> str:
        return self.filepath
