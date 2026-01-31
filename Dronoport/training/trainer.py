import numpy as np
from tqdm import tqdm
from config import TRAINING_EPISODES

class Trainer:
    def __init__(self, env, agent, discretizer):
        self.env, self.agent, self.discretizer = env, agent, discretizer
        self._reset()
    
    def train(self, n_episodes=TRAINING_EPISODES, verbose=True):
        self._reset()
        it = tqdm(range(n_episodes), desc=f"Training {self.agent.name}") if verbose else range(n_episodes)
        
        for ep in it:
            reward, info = self._run_episode()
            self.rewards.append(reward)
            self.lengths.append(info['steps'])
            self.successes.append(info['success'])
            self.charges.append(info['final_charge'])
            self.epsilons.append(self.agent.get_epsilon())
            self.agent.decay_epsilon()
            
            if verbose and ep % 100 == 0:
                it.set_postfix({'success': f'{(np.mean(self.successes[-100:]) * 100 if len(self.successes) >= 100 else 0):.2f}%', 'eps': f'{self.agent.get_epsilon():.2f}'})
        
        return {'rewards': np.array(self.rewards), 'lengths': np.array(self.lengths), 'successes': np.array(self.successes), 'charges': np.array(self.charges), 'epsilon': np.array(self.epsilons)}
    
    def _run_episode(self):
        state = self.env.reset()
        ds = self.discretizer.discretize(state)
        total, done, info = 0.0, False, {}
        
        while not done:
            action = self.agent.select_action(ds)
            state, reward, done, info = self.env.step(action)
            nds = self.discretizer.discretize(state)
            self.agent.update(ds, action, reward, nds, done)
            total += reward
            ds = nds
        
        return total, info
    
    def _reset(self):
        self.rewards, self.lengths, self.successes, self.charges, self.epsilons = [], [], [], [], []
    
    def get_smoothed_rewards(self, window=150):
        if len(self.rewards) < window: return np.array(self.rewards)
        return np.convolve(self.rewards, np.ones(window)/window, mode='valid')
    
    def get_success_rate(self, window=150):
        if len(self.successes) < window: return np.array([])
        return np.convolve(np.array(self.successes, dtype=float), np.ones(window)/window, mode='valid') * 100
