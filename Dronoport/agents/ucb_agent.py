import numpy as np
from .base_agent import BaseAgent
from config import *

class UCBAgent(BaseAgent):
    def __init__(self, n_states, n_actions, alpha=None, gamma=None, c=None):
        super().__init__(n_states, n_actions, name="UCB Exploration")
        p = UCB_PARAMS
        self.alpha = alpha or p['alpha']
        self.gamma = gamma or p['gamma']
        self.c = c or p['c']
        self.state_count = np.zeros(n_states)
    
    def get_epsilon(self): return 0.0
    
    def select_action(self, state, epsilon=None):
        self.state_count[state] += 1
        ucb = np.array([float('inf') if self.visit_count[state, a] == 0 
                        else self.q_table[state, a] + self.c * np.sqrt(np.log(self.state_count[state]) / self.visit_count[state, a]) for a in range(self.n_actions)])
        return np.random.choice(np.where(ucb == np.max(ucb))[0])
    
    def update(self, state, action, reward, next_state, done):
        self.visit_count[state, action] += 1
        self.total_steps += 1
        target = (reward if done else reward + self.gamma * np.max(self.q_table[next_state]))
        self.q_table[state, action] += self.alpha * (target - self.q_table[state, action])
    
    def decay_epsilon(self): self.total_episodes += 1
