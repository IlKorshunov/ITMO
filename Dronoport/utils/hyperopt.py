import numpy as np
import optuna
from optuna.samplers import TPESampler
import agents
from training import Trainer
from evaluation import Evaluator

optuna.logging.set_verbosity(optuna.logging.WARNING)

SEARCH_RANGES = {
    'qlearning': {'alpha': (0.05, 0.5, True), 'gamma': (0.9, 0.999, False), 'epsilon_decay': (0.99, 0.999, False)},
    'prospect': {'learning_rate': (0.05, 0.3, True), 'gamma': (0.95, 0.999, False), 'epsilon_decay': (0.995, 0.999, False)},
    'sarsa': {'alpha': (0.05, 0.5, True), 'gamma': (0.9, 0.999, False), 'epsilon_decay': (0.99, 0.999, False)},
    'double_q': {'alpha': (0.05, 0.5, True), 'gamma': (0.9, 0.999, False), 'epsilon_decay': (0.99, 0.999, False)},
    'ucb': {'alpha': (0.05, 0.5, True), 'gamma': (0.9, 0.999, False), 'c': (0.5, 5.0, False)},
    'expected_sarsa': {'alpha': (0.05, 0.3, True), 'gamma': (0.95, 0.999, False), 'epsilon_decay': (0.995, 0.999, False)},
}

AGENT_CLASSES = {
    'qlearning': 'QLearningAgent',
    'prospect': 'ProspectAgent', 
    'sarsa': 'SARSAAgent',
    'double_q': 'DoubleQLearningAgent',
    'ucb': 'UCBAgent',
    'expected_sarsa': 'ExpectedSARSAAgent',
}

def run_trial(trial, agent_type, env, discretizer, n_states, n_actions, train_eps, test_eps, seed):
    np.random.seed(seed + trial.number)
    agent = create_agent(agent_type, n_states, n_actions, suggest_params(trial, agent_type))
    Trainer(env, agent, discretizer).train(n_episodes=train_eps, verbose=False)
    m = Evaluator(env, discretizer).evaluate(agent, n_episodes=test_eps, epsilon=0.005, verbose=False)
    return m['success_rate'] + max(0, m['mean_reward'] + 150) / 10

def optimize_agent(agent_type, env, discretizer, n_trials=30, train_eps=2000, test_eps=100, seed=42, verbose=False, n_jobs=4):
    n_states, n_actions = discretizer.get_n_states(), env.n_actions
    study = optuna.create_study(direction='maximize', sampler=TPESampler(seed=seed))
    study.optimize(lambda t: run_trial(t, agent_type, env, discretizer, n_states, n_actions, train_eps, test_eps, seed), n_trials=n_trials, n_jobs=n_jobs, show_progress_bar=False)
    if verbose: print(f"Best params: {study.best_params}")
    return study.best_params

def suggest_params(trial, agent_type):
    result = {}
    for name, (lo, hi, log) in SEARCH_RANGES[agent_type].items():
        result[name] = trial.suggest_int(name, lo, hi) if log == 'int' else trial.suggest_float(name, lo, hi, log=log)
    return result

def create_agent(agent_type, n_states, n_actions, params):
    cls = getattr(agents, AGENT_CLASSES[agent_type])
    if agent_type == 'prospect': return cls(n_states, n_actions, learning_rate=params.get('learning_rate', 0.1), gamma=params.get('gamma', 0.99), epsilon_start=1.0, epsilon_end=0.01, epsilon_decay=params.get('epsilon_decay', 0.997), reference_decay=params.get('reference_decay', 0.995))
    if agent_type == 'ucb': return cls(n_states, n_actions, alpha=params.get('alpha', 0.15), gamma=params.get('gamma', 0.99), c=params.get('c', 2.0))
    return cls(n_states, n_actions, alpha=params.get('alpha', 0.15), gamma=params.get('gamma', 0.99), epsilon_start=1.0, epsilon_end=0.01, epsilon_decay=params.get('epsilon_decay', 0.997))

def get_default_params(agent_type):
    defaults = {}
    for name, (lo, hi, log) in SEARCH_RANGES.get(agent_type, {}).items(): defaults[name] = (lo + hi) / 2 if log != 'int' else (lo + hi) // 2
    return defaults

def run_hyperopt_for_agents(agent_list, env, discretizer, n_trials=30, train_eps=2000, test_eps=100, seed=42):
    print("Hyperparameter Optimization")
    return {t: optimize_agent(t, env, discretizer, n_trials, train_eps, test_eps, seed) for t in agent_list}
