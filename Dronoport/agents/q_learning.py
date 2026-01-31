import numpy as np
from .base_agent import BaseAgent
from config import *

class QLearningAgent(BaseAgent):
    def __init__(self, n_states, n_actions, alpha=None, gamma=None, epsilon_start=None, epsilon_end=None, epsilon_decay=None):
        super().__init__(n_states, n_actions, name="Q-Learning (Rational)")
        p = Q_LEARNING_PARAMS
        self.alpha = alpha or p['alpha']
        self.gamma = gamma or p['gamma']
        self.epsilon = epsilon_start or p['epsilon_start']
        self.epsilon_end = epsilon_end or p['epsilon_end']
        self.epsilon_decay = epsilon_decay or p['epsilon_decay']
    
    def update(self, state, action, reward, next_state, done):
        self.visit_count[state, action] += 1
        self.total_steps += 1
        target = reward if done else reward + self.gamma * np.max(self.q_table[next_state])
        self.q_table[state, action] += self.alpha * (target - self.q_table[state, action])
