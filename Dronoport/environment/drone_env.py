import numpy as np
from dataclasses import dataclass
from config import *
from .obstacles import ObstacleManager
from .wind import WindModel

@dataclass
class StepInfo:
    position: tuple
    charge: float
    speed: float
    wind: tuple
    reward: float
    at_station: bool
    collision: bool

class DroneEnvironment:
    def __init__(self, seed=None):
        if seed: np.random.seed(seed)
        self.obstacles, self.wind = ObstacleManager(), WindModel()
        self.station_x, self.station_y = CHARGING_STATION_CENTER
        self.n_actions = 16
        self.reset()
    
    def reset(self):
        self.x = np.clip(np.random.normal(SPAWN_MEAN[0], SPAWN_STD), 0.5, 6.0)
        self.y = np.clip(np.random.normal(SPAWN_MEAN[1], SPAWN_STD), 0.5, 6.0)
        self.charge, self.speed_idx, self.steps, self.collisions = 1.0, 0, 0, 0
        self.wind.reset()
        self.trajectory = []
        return (self.station_x - self.x, self.station_y - self.y, self.charge, self.speed_idx)
    
    def step(self, action):
        self.steps += 1
        d, v = DIRECTIONS[action // 4], SPEEDS[action % 4]
        wx, wy = self.wind.step()
        old_x, old_y = self.x, self.y
        
        dx, dy = v*d[0] + wx, v*d[1] + wy
        new_x, new_y = np.clip(self.x + dx, 0, 20), np.clip(self.y + dy, 0, 10)
        
        self.speed_idx = int(np.argmin([abs(np.sqrt(dx**2 + dy**2) - s) for s in SPEEDS]))
        
        if self.obstacles.check_line_collision(old_x, old_y, new_x, new_y):
            self.collisions += 1
            new_x, new_y = old_x, old_y
        
        self.x, self.y = new_x, new_y
        self.charge -= 0.015*v**2 + 0.002*self.wind.get_magnitude()**2
        
        at_st = np.sqrt((self.x - self.station_x)**2 + (self.y - self.station_y)**2) < 0.7
        if at_st: self.charge = min(1.0, self.charge + CHARGING_POWER)
        
        reward = -0.8 - 0.025*v**2 + (0.002 if at_st else 0)
        done, success, late, dead, timeout = False, False, False, False, False
        
        if at_st and self.charge >= SUCCESS_CHARGE:
            done = True
            if self.steps <= MAX_STEPS_SUCCESS: success, reward = True, reward + REWARD_SUCCESS
            else: late = True
        if self.charge < MIN_CHARGE: done, dead, reward = True, True, reward + REWARD_BATTERY_DEAD
        if self.steps >= MAX_STEPS_TOTAL: done, timeout = True, True
        
        self.trajectory.append(StepInfo((self.x, self.y), self.charge, v, (wx, wy), reward, at_st, self.collisions > 0 and new_x == old_x))
        
        return (self.station_x - self.x, self.station_y - self.y, self.charge, self.speed_idx), reward, done, {
            'success': success, 'late_arrival': late, 'timeout': timeout, 'battery_dead': dead,
            'at_station': at_st, 'steps': self.steps, 'final_charge': self.charge, 'total_collisions': self.collisions
        }
    
    def get_trajectory(self): return self.trajectory.copy()

    def get_position(self): return self.x, self.y

    def get_obstacles(self): return self.obstacles.get_obstacles()

    def get_station_info(self): return (self.station_x, self.station_y), 0.7
