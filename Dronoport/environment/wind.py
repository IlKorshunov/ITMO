import numpy as np
from config import *

class WindModel:
    def __init__(self, interval=WIND_UPDATE_INTERVAL):
        self.interval = interval
        self.wmin, self.wmax = WIND_RANGE
        self.reset()
    
    def reset(self):
        self.cnt = 0
        self._update()
    
    def _update(self):
        self.wx = np.random.uniform(self.wmin, self.wmax)
        self.wy = np.random.uniform(self.wmin, self.wmax)
    
    def step(self):
        self.cnt += 1
        if self.cnt % self.interval == 0: self._update()
        return self.wx, self.wy
    
    def get_current(self): return self.wx, self.wy
    def get_magnitude(self): return np.sqrt(self.wx**2 + self.wy**2)
