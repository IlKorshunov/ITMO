import numpy as np
from config import OBSTACLES

class ObstacleManager:
    def __init__(self, obstacles=None):
        self.obstacles = obstacles or OBSTACLES
    
    def check_collision(self, x, y, margin=0.0):
        for x1, y1, x2, y2 in self.obstacles:
            if x1-margin <= x <= x2+margin and y1-margin <= y <= y2+margin: return True
        return False
    
    def check_line_collision(self, x1, y1, x2, y2, samples=10):
        for t in np.linspace(0, 1, samples):
            if self.check_collision(x1 + t*(x2-x1), y1 + t*(y2-y1)): return True
        return False
    
    def get_obstacles(self): return self.obstacles.copy()
    
    def get_obstacle_centers(self):
        return [((x1+x2)/2, (y1+y2)/2) for x1, y1, x2, y2 in self.obstacles]
