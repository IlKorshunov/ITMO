from abc import ABC, abstractmethod
import numpy as np

class BaseAgent(ABC):
    def __init__(self, n_states, n_actions, name="BaseAgent"):
        self.n_states, self.n_actions, self.name = n_states, n_actions, name
        self.total_steps, self.total_episodes = 0, 0
        self.q_table = np.zeros((n_states, n_actions))
        self.visit_count = np.zeros((n_states, n_actions))
        self.epsilon = 1.0
        self.epsilon_end = 0.01
        self.epsilon_decay = 0.997
        self.alpha = 0.15
        self.gamma = 0.99
    
    def select_action(self, state, epsilon=None):
        eps = epsilon if epsilon is not None else self.epsilon
        if np.random.random() < eps: return np.random.randint(self.n_actions)
        q = self.get_q_values(state)
        return np.random.choice(np.where(q == np.max(q))[0])
    
    @abstractmethod
    def update(self, state, action, reward, next_state, done): pass
    def get_q_values(self, state): return self.q_table[state].copy()
    def get_epsilon(self): return self.epsilon

    def decay_epsilon(self):
        self.epsilon = max(self.epsilon_end, self.epsilon * self.epsilon_decay)
        self.total_episodes += 1
