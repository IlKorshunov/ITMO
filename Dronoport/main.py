import os, csv, json
import numpy as np
from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).parent))
from config import TRAINING_EPISODES, TESTING_EPISODES, TEST_EPSILON, SEED
from environment import DroneEnvironment
from training import Trainer
from evaluation import Evaluator
from utils import StateDiscretizer, create_agent, get_default_params, optimize_agent
from visualization import PlotManager

AGENTS = ['qlearning', 'prospect', 'sarsa', 'double_q', 'ucb', 'expected_sarsa']
OUTPUT_DIR = 'results'
HYPEROPT_TRIALS = 6
HYPEROPT_TRAIN_EPS = 1500
HYPEROPT_TEST_EPS = 75

def main():
    for subdir in ['trajectories', 'comparison', 'learning', 'data', 'hyperopt']:  os.makedirs(os.path.join(OUTPUT_DIR, subdir), exist_ok=True)
    np.random.seed(SEED)
    
    env = DroneEnvironment(seed=SEED)
    disc = StateDiscretizer()
    n_states, n_actions = disc.get_n_states(), env.n_actions
    
    best_params = {}
    params_file = os.path.join(OUTPUT_DIR, 'hyperopt', 'best_params.json')
    print("selection of hyperparameters" + os.linesep)
    
    for name in AGENTS:
        if name == 'ucb': best_params[name] = get_default_params(name)
        else:
            print(f"[{name}]")
            best_params[name] = optimize_agent(name, env, disc, n_trials=HYPEROPT_TRIALS, train_eps=HYPEROPT_TRAIN_EPS, test_eps=HYPEROPT_TEST_EPS, seed=SEED)
        print()
    
    with open(params_file, 'w') as f: json.dump(best_params, f, indent=2)
    print(f"Params saved to {params_file}" + os.linesep)
    
    histories, metrics, trajectories = {}, {}, {}
    evaluator = Evaluator(env, disc)
    
    for name in AGENTS:
        params = best_params.get(name, get_default_params(name))
        agent = create_agent(name, n_states, n_actions, params)
        print(f"Training {agent.name} with params: {params}")
        histories[name] = Trainer(env, agent, disc).train(n_episodes=TRAINING_EPISODES)
        metrics[name] = evaluator.evaluate(agent, n_episodes=TESTING_EPISODES, epsilon=TEST_EPSILON)
        trajectories[name] = evaluator.collect_trajectories(agent, n=4, epsilon=TEST_EPSILON)
        print(f"Success: {metrics[name]['success_rate']:.1f}%" + os.linesep)
    
    save_csv(histories, metrics)
    
    pm = PlotManager(OUTPUT_DIR)
    pm.plot_all_agents_comparison(metrics)
    pm.plot_all_agents_learning_curves(histories)
    pm.plot_all_agents_trajectories(trajectories)
    
    print(os.linesep + f"Results: {OUTPUT_DIR}/")

def save_csv(histories, metrics):
    with open(os.path.join(OUTPUT_DIR, 'data', 'results.csv'), 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['agent', 'success_rate', 'mean_steps', 'mean_charge', 'mean_reward'])
        for m in metrics.values():
            w.writerow([m['agent_name'], f"{m['success_rate']:.2f}", 
                        f"{m['mean_success_steps']:.2f}" if not np.isnan(m['mean_success_steps']) else "",
                        f"{m['mean_final_charge']:.4f}", f"{m['mean_reward']:.2f}"])
    
    with open(os.path.join(OUTPUT_DIR, 'data', 'training_history.csv'), 'w', newline='') as f:
        w = csv.writer(f)
        names = list(histories.keys())
        w.writerow(['episode'] + [f"{n}_{k}" for n in names for k in ['reward', 'length', 'success', 'eps']])
        for i in range(len(next(iter(histories.values()))['rewards'])):
            row = [i]
            for h in histories.values(): row += [f"{h['rewards'][i]:.4f}", h['lengths'][i], int(h['successes'][i]), f"{h['epsilon'][i]:.6f}"]
            w.writerow(row)

if __name__ == "__main__":
    main()
