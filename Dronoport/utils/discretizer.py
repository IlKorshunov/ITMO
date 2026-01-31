import numpy as np
from config import STATE_BINS

class StateDiscretizer:
    def __init__(self):
        self.dx_bins, self.dy_bins = STATE_BINS['dx'], STATE_BINS['dy']
        self.charge_bins, self.speed_bins = STATE_BINS['charge'], STATE_BINS['speed']
        self.dx_edges = np.linspace(-18, 2, self.dx_bins + 1)
        self.dy_edges = np.linspace(-6, 4, self.dy_bins + 1)
        self.charge_edges = np.linspace(0, 1, self.charge_bins + 1)
        self.n_states = self.dx_bins * self.dy_bins * self.charge_bins * self.speed_bins
    
    def discretize(self, state):
        dx, dy, charge, speed_idx = state
        dx_bin = max(0, min(np.digitize(dx, self.dx_edges) - 1, self.dx_bins - 1))
        dy_bin = max(0, min(np.digitize(dy, self.dy_edges) - 1, self.dy_bins - 1))
        ch_bin = max(0, min(np.digitize(charge, self.charge_edges) - 1, self.charge_bins - 1))
        sp_bin = max(0, min(speed_idx, self.speed_bins - 1))
        return int(dx_bin * self.dy_bins * self.charge_bins * self.speed_bins + dy_bin * self.charge_bins * self.speed_bins + ch_bin * self.speed_bins + sp_bin)
    
    def get_n_states(self): return self.n_states

    def get_state_shape(self): return (self.dx_bins, self.dy_bins, self.charge_bins, self.speed_bins)
