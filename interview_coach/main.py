import argparse
import sys
import os
from typing import Optional, Dict

from interview_coach.config import config, LLMProvider
from interview_coach.models.candidate import Candidate
from interview_coach.agents.interviewer import InterviewerAgent
from interview_coach.agents.observer import ObserverAgent
from interview_coach.agents.manager import ManagerAgent
from interview_coach.core.memory import ConversationMemory
from interview_coach.models.turn import Turn
from interview_coach.utils.logger import InterviewLogger
from interview_coach.llm.factory import create_llm_provider


def load_config_from_file(filepath: str) -> Optional[Dict]:
    if not os.path.exists(filepath):
        return None
    
    config_data = {}
    with open(filepath, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            if '=' in line:
                key, value = line.split('=', 1)
                config_data[key.strip()] = value.strip()
    
    return config_data if config_data else None


def parse_config(config_data: Dict):
    candidate = Candidate(
        name=config_data.get('name', 'Кандидат'),
        position=config_data.get('position', 'Developer'),
        expected_grade=config_data.get('grade', 'Junior'),
        experience=config_data.get('experience', 'Не указано'),
    )
    
    provider_str = config_data.get('provider', 'local').lower()
    provider_map = {'local': LLMProvider.LOCAL, 'huggingface_api': LLMProvider.HUGGINGFACE_API}
    provider = provider_map.get(provider_str, LLMProvider.LOCAL)
    
    kwargs = {}
    if provider == LLMProvider.LOCAL:
        kwargs['base_url'] = config_data.get('local_url', 'http://localhost:11434/v1')
        kwargs['model'] = config_data.get('local_model', 'mistral-nemo')
    elif provider == LLMProvider.HUGGINGFACE_API:
        kwargs['model'] = config_data.get('hf_model', 'Qwen/Qwen2.5-7B-Instruct')
        if 'hf_token' in config_data:
            kwargs['api_token'] = config_data['hf_token']
    
    return candidate, provider, kwargs


def run_interview(candidate: Candidate, provider: LLMProvider, provider_kwargs: dict, output_dir: str):
    try:
        llm = create_llm_provider(provider, **provider_kwargs)
    except ValueError as e:
        print(f"Ошибка конфигурации: {e}", file=sys.stderr)
        sys.exit(1)
    except ConnectionError as e:
        print(f"Ошибка подключения: {e}", file=sys.stderr)
        sys.exit(1)
    
    interviewer = InterviewerAgent(llm)
    observer = ObserverAgent(llm)
    manager = ManagerAgent(llm)
    memory = ConversationMemory()
    logger = InterviewLogger(candidate, output_dir)
    
    difficulty_level = config.interview.initial_difficulty
    turn_id = 0
    is_greeting_phase = True
    
    turn_id += 1
    greeting = interviewer.generate_greeting(candidate, difficulty_level)
    print(f"Интервьюер: {greeting}")
    
    turn = Turn(turn_id=turn_id, agent_visible_message=greeting, user_message="", internal_thoughts="")
    memory.add_turn(turn)
    logger.log_turn(turn)
    
    while True:
        try:
            user_input = input("Вы: ").strip()
            if not user_input:
                continue
            
            if config.interview.stop_command in user_input.lower().strip():
                break
            
            turn_id += 1
            
            if is_greeting_phase:
                is_greeting_phase = False
                response = interviewer.generate_first_question(candidate, difficulty_level)
                turn = Turn(turn_id=turn_id, user_message=user_input, agent_visible_message=response, internal_thoughts="")
                memory.add_turn(turn)
                logger.log_turn(turn)
                print(f"Интервьюер: {response}")
                continue
            
            observer_analysis = observer.process(
                candidate=candidate,
                conversation_history=memory.get_history_list(),
                current_message=user_input,
                topics_covered=memory.topics_covered,
            )
            
            memory._update_quality_tracking(observer_analysis)
            if memory.should_increase_difficulty():
                difficulty_level = min(5, difficulty_level + 1)
                memory.consecutive_high_scores = 0
            elif memory.should_decrease_difficulty():
                difficulty_level = max(1, difficulty_level - 1)
                memory.consecutive_low_scores = 0
            
            response, instruction = interviewer.process(
                candidate=candidate,
                conversation_history=memory.get_history_list(),
                observer_analysis=observer_analysis,
                difficulty_level=difficulty_level,
                topics_covered=memory.topics_covered,
            )
            
            turn = Turn(
                turn_id=turn_id,
                user_message=user_input,
                agent_visible_message=response,
                observer_analysis=observer_analysis,
                interviewer_instruction=instruction,
            )
            turn.build_internal_thoughts()
            memory.add_turn(turn)
            logger.log_turn(turn)
            
            print(f"Интервьюер: {response}")
                
        except KeyboardInterrupt:
            print("Прервано")
            break
        except Exception as e:
            print(f"Ошибка: {e}", file=sys.stderr)
            continue
    
    print("Генерирую фидбэк")
    feedback = manager.process(
        candidate=candidate,
        full_conversation=memory.get_history_list(),
        all_observations=memory.get_all_observations(),
    )
    logger.log_feedback(feedback)
    print(feedback.to_formatted_string())


def main():
    parser = argparse.ArgumentParser(description="Interview Coach")
    parser.add_argument("--input", type=str, default="input.txt")
    parser.add_argument("--output", type=str, default="./output")
    parser.add_argument("--provider", choices=["huggingface_api", "local"], default="local")
    parser.add_argument("--model", type=str, default=None)
    parser.add_argument("--name", type=str, default="Кандидат")
    parser.add_argument("--position", type=str, default="Developer")
    parser.add_argument("--grade", choices=["Junior", "Middle", "Senior"], default="Junior")
    parser.add_argument("--experience", type=str, default="Не указано")
    
    args = parser.parse_args()
    
    config_from_file = load_config_from_file(args.input)
    
    if config_from_file:
        candidate, provider, provider_kwargs = parse_config(config_from_file)
    else:
        candidate = Candidate(
            name=args.name,
            position=args.position,
            expected_grade=args.grade,
            experience=args.experience,
        )
        provider_map = {"huggingface_api": LLMProvider.HUGGINGFACE_API, "local": LLMProvider.LOCAL}
        provider = provider_map[args.provider]
        provider_kwargs = {}
        if provider == LLMProvider.LOCAL:
            provider_kwargs['base_url'] = 'http://localhost:11434/v1'
            provider_kwargs['model'] = args.model or 'mistral-nemo'
        else:
            provider_kwargs['model'] = args.model or 'Qwen/Qwen2.5-7B-Instruct'
    
    run_interview(
        candidate=candidate,
        provider=provider,
        provider_kwargs=provider_kwargs,
        output_dir=args.output,
    )


if __name__ == "__main__":
    main()
