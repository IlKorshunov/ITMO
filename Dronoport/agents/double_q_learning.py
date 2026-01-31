import numpy as np
from .base_agent import BaseAgent
from config import *

class DoubleQLearningAgent(BaseAgent):
    def __init__(self, n_states, n_actions, alpha=None, gamma=None, epsilon_start=None, epsilon_end=None, epsilon_decay=None):
        super().__init__(n_states, n_actions, name="Double Q-Learning")
        p = DOUBLE_Q_PARAMS
        self.alpha = alpha or p['alpha']
        self.gamma = gamma or p['gamma']
        self.epsilon = epsilon_start or p['epsilon_start']
        self.epsilon_end = epsilon_end or p['epsilon_end']
        self.epsilon_decay = epsilon_decay or p['epsilon_decay']
        self.q_table_a = np.zeros((n_states, n_actions))
        self.q_table_b = np.zeros((n_states, n_actions))
    
    def get_q_values(self, state): return (self.q_table_a[state] + self.q_table_b[state]) / 2
    
    def select_action(self, state, epsilon=None):
        eps = epsilon if epsilon is not None else self.epsilon
        if np.random.random() < eps: return np.random.randint(self.n_actions)
        q = self.q_table_a[state] + self.q_table_b[state]
        return np.random.choice(np.where(q == np.max(q))[0])
    
    def update(self, state, action, reward, next_state, done):
        self.visit_count[state, action] += 1
        self.total_steps += 1
        if np.random.random() < 0.5:
            qa, qb = self.q_table_a, self.q_table_b
        else:
            qa, qb = self.q_table_b, self.q_table_a
        target = reward if done else reward + self.gamma * qb[next_state, np.argmax(qa[next_state])]
        qa[state, action] += self.alpha * (target - qa[state, action])
