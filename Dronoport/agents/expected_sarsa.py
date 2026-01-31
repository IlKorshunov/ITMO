import numpy as np
from .base_agent import BaseAgent
from config import *

class ExpectedSARSAAgent(BaseAgent):
    def __init__(self, n_states, n_actions, alpha=None, gamma=None, epsilon_start=None, epsilon_end=None, epsilon_decay=None):
        super().__init__(n_states, n_actions, name="Expected SARSA")
        p = EXPECTED_SARSA_PARAMS
        self.alpha = alpha or p['alpha']
        self.gamma = gamma or p['gamma']
        self.epsilon = epsilon_start or p['epsilon_start']
        self.epsilon_end = epsilon_end or p['epsilon_end']
        self.epsilon_decay = epsilon_decay or p['epsilon_decay']
    
    def update(self, state, action, reward, next_state, done):
        self.visit_count[state, action] += 1
        self.total_steps += 1
        if done:
            target = reward
        else:
            q = self.q_table[next_state]
            max_q, n_best = np.max(q), np.sum(q == np.max(q))
            exp_q = sum((((1 - self.epsilon) / n_best + self.epsilon / self.n_actions) if q[a] == max_q else self.epsilon / self.n_actions) * q[a] for a in range(self.n_actions))
            target = reward + self.gamma * exp_q
        self.q_table[state, action] += self.alpha * (target - self.q_table[state, action])
