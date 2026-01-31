import numpy as np
from .base_agent import BaseAgent
from config import *

class ProspectAgent(BaseAgent):
    def __init__(self, n_states, n_actions, alpha=None, beta=None, lambda_=None, learning_rate=None, gamma=None, epsilon_start=None, epsilon_end=None, epsilon_decay=None, reference_decay=None):
        super().__init__(n_states, n_actions, name="Prospect Theory")
        p = PROSPECT_PARAMS
        self.pt_alpha = alpha or p['alpha']
        self.pt_beta = beta or p['beta']
        self.pt_lambda = lambda_ or p['lambda_']
        self.alpha = learning_rate or p['learning_rate']
        self.gamma = gamma or p['gamma']
        self.epsilon = epsilon_start or p['epsilon_start']
        self.epsilon_end = epsilon_end or p['epsilon_end']
        self.epsilon_decay = epsilon_decay or p['epsilon_decay']
    
    def update(self, state, action, reward, next_state, done):
        self.visit_count[state, action] += 1
        self.total_steps += 1
        if reward >= 0: r = np.sign(reward) * np.power(np.abs(reward) + 1e-6, self.pt_alpha)
        else: r = -self.pt_lambda * np.power(-reward + 1e-6, self.pt_beta)
        target = r if done else r + self.gamma * np.max(self.q_table[next_state])
        self.q_table[state, action] += self.alpha * (target - self.q_table[state, action])
