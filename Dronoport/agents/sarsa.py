import numpy as np
from .base_agent import BaseAgent
from config import *

class SARSAAgent(BaseAgent):
    def __init__(self, n_states, n_actions, alpha=None, gamma=None, epsilon_start=None, epsilon_end=None, epsilon_decay=None):
        super().__init__(n_states, n_actions, name="SARSA (On-Policy)")
        p = SARSA_PARAMS
        self.alpha = alpha or p['alpha']
        self.gamma = gamma or p['gamma']
        self.epsilon = epsilon_start or p['epsilon_start']
        self.epsilon_end = epsilon_end or p['epsilon_end']
        self.epsilon_decay = epsilon_decay or p['epsilon_decay']
        self.next_action = None
    
    def select_action(self, state, epsilon=None):
        if epsilon is None and self.next_action is not None:
            a, self.next_action = self.next_action, None
            return a
        return super().select_action(state, epsilon)
    
    def _pick_action(self, state):
        if np.random.random() < self.epsilon: return np.random.randint(self.n_actions)
        q = self.q_table[state]
        return np.random.choice(np.where(q == np.max(q))[0])
    
    def update(self, state, action, reward, next_state, done):
        self.visit_count[state, action] += 1
        self.total_steps += 1
        if done:
            target = reward
        else:
            self.next_action = self._pick_action(next_state)
            target = reward + self.gamma * self.q_table[next_state, self.next_action]
        self.q_table[state, action] += self.alpha * (target - self.q_table[state, action])
    
    def decay_epsilon(self):
        super().decay_epsilon()
        self.next_action = None
