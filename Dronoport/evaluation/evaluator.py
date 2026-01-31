import numpy as np
from dataclasses import dataclass
from scipy import stats
from tqdm import tqdm
from config import TESTING_EPISODES, TEST_EPSILON
import os

@dataclass
class TestResult:
    success: bool
    steps: int
    final_charge: float
    total_reward: float
    total_collisions: int
    trajectory: list

class Evaluator:
    def __init__(self, env, discretizer):
        self.env, self.discretizer = env, discretizer
    
    def evaluate(self, agent, n_episodes=TESTING_EPISODES, epsilon=TEST_EPSILON, verbose=True, collect_trajectories=False):
        it = tqdm(range(n_episodes), desc=f"Testing {agent.name}") if verbose else range(n_episodes)
        return self._compute_metrics([self._run_episode(agent, epsilon, collect_trajectories) for _ in it], agent.name)
    
    def _run_episode(self, agent, epsilon, collect_traj):
        state = self.env.reset()
        ds = self.discretizer.discretize(state)
        total_reward, done, info = 0.0, False, {}
        
        while not done:
            action = agent.select_action(ds, epsilon=epsilon)
            state, reward, done, info = self.env.step(action)
            ds = self.discretizer.discretize(state)
            total_reward += reward
        
        return TestResult(info['success'], info['steps'], info['final_charge'], total_reward, info['total_collisions'], self.env.get_trajectory() if collect_traj else [])
    
    def _compute_metrics(self, results, name):
        n = len(results)
        succ = [r.success for r in results]
        charges = [r.final_charge for r in results]
        rewards = [r.total_reward for r in results]
        succ_steps = [r.steps for r in results if r.success]
        
        return {
            'agent_name': name, 'n_episodes': n,
            'success_rate': np.mean(succ) * 100, 'success_count': sum(succ),
            'mean_success_steps': np.mean(succ_steps) if succ_steps else np.nan,
            'std_success_steps': np.std(succ_steps) if succ_steps else np.nan,
            'mean_final_charge': np.mean(charges), 'std_final_charge': np.std(charges),
            'mean_reward': np.mean(rewards), 'std_reward': np.std(rewards),
            'collision_rate': sum(1 for r in results if r.total_collisions > 0) / n * 100,
            'raw_successes': np.array(succ), 'raw_charges': np.array(charges), 
            'raw_rewards': np.array(rewards), 'success_steps_list': succ_steps,
        }
    
    def collect_trajectories(self, agent, n=4, epsilon=TEST_EPSILON):
        results, attempts = [], 0
        while len(results) < n and attempts < n * 20:
            r = self._run_episode(agent, epsilon, True)
            if r.success: results.append(r)
            attempts += 1
        return results
    
    @staticmethod
    def compare_agents(m1, m2):
        comp = {'agent1_name': m1['agent_name'], 'agent2_name': m2['agent_name']}
        
        if m1['success_steps_list'] and m2['success_steps_list']:
            t, p = stats.ttest_ind(m1['success_steps_list'], m2['success_steps_list'])
            comp['steps_ttest'] = {'t_statistic': t, 'p_value': p, 'significant': p < 0.05}
        
        t, p = stats.ttest_ind(m1['raw_rewards'], m2['raw_rewards'])
        comp['rewards_ttest'] = {'t_statistic': t, 'p_value': p, 'significant': p < 0.05}
        
        return comp
    
    @staticmethod
    def format_comparison_table(m1, m2, comp):
        def fmt(v, d=2): return f"{v:<20.{d}f}" if not np.isnan(v) else f"{'N/A':<20}"
        
        lines = [
            f"{'Metric':<25} {m1['agent_name']:<20} {m2['agent_name']:<20}",
            f"{'Success Rate (%)':<25} {fmt(m1['success_rate'])} {fmt(m2['success_rate'])}",
            f"{'Mean Steps (success)':<25} {fmt(m1['mean_success_steps'])} {fmt(m2['mean_success_steps'])}",
            f"{'Std Steps (success)':<25} {fmt(m1['std_success_steps'])} {fmt(m2['std_success_steps'])}",
            f"{'Mean Final Charge':<25} {fmt(m1['mean_final_charge'], 4)} {fmt(m2['mean_final_charge'], 4)}",
            f"{'Collision Rate (%)':<25} {fmt(m1['collision_rate'])} {fmt(m2['collision_rate'])}",
            f"{'Mean Reward':<25} {fmt(m1['mean_reward'])} {fmt(m2['mean_reward'])}",
            "", "Statistical Tests (p < 0.05):"
        ]
        
        if 'steps_ttest' in comp:  lines.append(f"Steps t-test: p = {comp['steps_ttest']['p_value']:.4f} {'***' if comp['steps_ttest']['significant'] else ''}")
        lines.append(f"Rewards t-test: p = {comp['rewards_ttest']['p_value']:.4f} {'***' if comp['rewards_ttest']['significant'] else ''}")
        return os.linesep.join(lines)
