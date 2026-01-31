import numpy as np
from .base_agent import BaseAgent
from config import *

class MCTSNode:
    def __init__(self, state, parent=None, action=None):
        self.state, self.parent, self.action = state, parent, action
        self.children, self.visits, self.value = {}, 0, 0.0
        self.untried = []
    
    def best_child(self, c):
        best, best_node = float('-inf'), None
        for child in self.children.values():
            score = float('inf') if child.visits == 0 else child.value / child.visits + c * np.sqrt(np.log(self.visits) / child.visits)
            if score > best: best, best_node = score, child
        return best_node
    
    def best_action(self):
        return max(self.children.items(), key=lambda x: x[1].visits)[0] if self.children else 0

class MCTSAgent(BaseAgent):
    def __init__(self, n_states, n_actions, simulations=None, c=None, gamma=None, rollout_depth=None):
        super().__init__(n_states, n_actions, name="MCTS")
        p = MCTS_PARAMS
        self.simulations = simulations or p['simulations']
        self.c = c or p['c']
        self.gamma = gamma or p['gamma']
        self.rollout_depth = rollout_depth or p['rollout_depth']
    
    def get_epsilon(self): return 0.0
    def decay_epsilon(self): self.total_episodes += 1
    
    def select_action(self, state, epsilon=None):
        root = MCTSNode(state)
        root.untried = list(range(self.n_actions))
        
        for _ in range(self.simulations):
            node = root
            while len(node.children) == self.n_actions and node.children:
                node = node.best_child(self.c)
            
            if node.untried:
                a = np.random.choice(node.untried)
                node.untried.remove(a)
                child = MCTSNode(state, node, a)
                child.untried = list(range(self.n_actions))
                node.children[a] = child
                node = child
            
            r = self._rollout(node.state)
            while node:
                node.visits += 1
                node.value += r
                r *= self.gamma
                node = node.parent
        
        return root.best_action()
    
    def _rollout(self, state):
        total, discount = 0.0, 1.0
        for _ in range(self.rollout_depth):
            total += discount * self.q_table[state, np.random.randint(self.n_actions)]
            discount *= self.gamma
            if discount < 0.01: break
        return total
    
    def update(self, state, action, reward, next_state, done):
        self.visit_count[state, action] += 1
        self.total_steps += 1
        target = reward if done else reward + self.gamma * np.max(self.q_table[next_state])
        self.q_table[state, action] += 0.1 * (target - self.q_table[state, action])
