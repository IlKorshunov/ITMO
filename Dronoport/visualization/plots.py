import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import os
from config import *

def smooth(data, w): return np.convolve(data, np.ones(w)/w, mode='valid') if len(data) >= w else data

class PlotManager:
    def __init__(self, output_dir="results"):
        self.output_dir = output_dir
        self.colors = plt.cm.tab10(np.linspace(0, 1, 10))
    
    def _save(self, fig, subdir, name):
        path = os.path.join(self.output_dir, subdir, f'{name}.png')
        fig.savefig(path, dpi=100, bbox_inches='tight')
        plt.close(fig)
    
    def _env(self, ax):
        ax.set_xlim(-0.5, 20.5)
        ax.set_ylim(-0.5, 10.5)
        ax.set_aspect('equal')
        ax.add_patch(plt.Circle(CHARGING_STATION_CENTER, 0.7, color='#FFD700', alpha=0.7, label='Station'))
        for x1, y1, x2, y2 in OBSTACLES: ax.add_patch(patches.Rectangle((x1, y1), x2-x1, y2-y1, facecolor='#6C757D', alpha=0.7))
        ax.grid(True, alpha=0.3)
        ax.set_xlabel('X')
        ax.set_ylabel('Y')
    
    def plot_all_agents_comparison(self, metrics):
        names = list(metrics.keys())
        n = len(names)
        fig, ax = plt.subplots(figsize=(max(12, n*1.5), 8))
        ax.axis('off')
        data = []
        for _, m in metrics.items():
            data.append([
                m['agent_name'],
                f"{m['success_rate']:.1f}",
                f"{m['mean_success_steps']:.1f}" if not np.isnan(m['mean_success_steps']) else "N/A",
                f"{m['std_success_steps']:.1f}" if not np.isnan(m['std_success_steps']) else "N/A",
                f"{m['mean_final_charge']:.3f}",
                f"{m['collision_rate']:.1f}",
                f"{m['mean_reward']:.2f}"
            ])
        
        table = ax.table(cellText=data, colLabels=['Agent', 'Success %', 'Mean Steps', 'Std Steps', 'Mean Charge', 'Collision %', 'Mean Reward'], loc='center', cellLoc='center')
        table.auto_set_font_size(False)
        table.set_fontsize(10)
        table.scale(1.2, 2.0)
        
        for (r, _), cell in table.get_celld().items():
            if r == 0: 
                cell.set_facecolor('#4A90D9')
                cell.set_text_props(color='white', fontweight='bold')
            elif r > 0:
                rgb = self.colors[(r-1) % 10][:3]
                cell.set_facecolor((*rgb, 0.2))
        
        plt.title('All Agents Comparison', fontsize=16, fontweight='bold', pad=20)
        self._save(fig, 'comparison', 'all_agents_table')
        
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))
        for ax, key, ylabel, title in [
            (axes[0,0], 'success_rate', 'Success %', 'Success Rate'),
            (axes[0,1], 'mean_reward', 'Reward', 'Mean Reward'),
            (axes[1,0], 'mean_success_steps', 'Steps', 'Mean Steps to Success'),
            (axes[1,1], 'collision_rate', 'Collision %', 'Collision Rate')
        ]:
            vals = [m[key] if not np.isnan(m[key]) else 0 for m in metrics.values()]
            bars = ax.bar(range(n), vals, color=[self.colors[i % 10] for i in range(n)])
            ax.set_xticks(range(n))
            ax.set_xticklabels([m['agent_name'] for m in metrics.values()], rotation=45, ha='right')
            ax.set_ylabel(ylabel)
            ax.set_title(title)
            ax.grid(True, alpha=0.3, axis='y')
            for bar, v in zip(bars, vals):
                ax.text(bar.get_x() + bar.get_width()/2, bar.get_height(), f'{v:.1f}', ha='center', va='bottom', fontsize=8)
        
        plt.tight_layout()
        self._save(fig, 'comparison', 'all_agents_bars')
    
    def plot_all_agents_learning_curves(self, histories, window=150):
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))
        
        for ax, key, ylabel, title, mult, yscale in [
            (axes[0,0], 'rewards', 'Reward', f'Learning Curve (window={window})', 1, None),
            (axes[0,1], 'successes', 'Success %', 'Success Rate', 100, None),
            (axes[1,0], 'epsilon', 'Epsilon', 'Exploration Decay', 1, 'log'),
            (axes[1,1], 'lengths', 'Steps', 'Episode Length', 1, None)
        ]:
            for i, (name, h) in enumerate(histories.items()):
                data = smooth(h[key].astype(float), window) * mult
                ax.plot(data, label=name, color=self.colors[i % 10], linewidth=1.5, alpha=0.8)
            ax.set_xlabel('Episode')
            ax.set_ylabel(ylabel)
            ax.set_title(title)
            ax.legend(loc='best', fontsize=8)
            ax.grid(True, alpha=0.3)
            if yscale: ax.set_yscale(yscale)
            if key == 'successes': ax.set_ylim(0, 100)
        
        plt.tight_layout()
        self._save(fig, 'learning', 'all_agents_learning')
    
    def plot_all_agents_trajectories(self, all_trajectories):
        for agent_name, trajs in all_trajectories.items():
            if not trajs: continue
            
            n = len(trajs)
            rows = (n + 1) // 2
            fig, axes = plt.subplots(rows, 2, figsize=(14, 5*rows))
            if rows == 1: axes = axes.reshape(1, -1)
            
            for i, tr in enumerate(trajs):
                ax = axes[i//2, i%2]
                self._env(ax)
                
                if tr.trajectory:
                    xs = [s.position[0] for s in tr.trajectory]
                    ys = [s.position[1] for s in tr.trajectory]
                    charges = [s.charge for s in tr.trajectory]
                    
                    for j in range(len(xs)-1):
                        ax.plot([xs[j], xs[j+1]], [ys[j], ys[j+1]], 
                               color=plt.cm.RdYlGn(charges[j]), linewidth=2, alpha=0.8)
                    
                    ax.scatter(xs[0], ys[0], s=120, c='blue', marker='o', zorder=5, label='Start')
                    ax.scatter(xs[-1], ys[-1], s=120, c='green' if tr.success else 'red', 
                              marker='*', zorder=5, label='End')
                
                ax.set_title(f'{'SUCCESS' if tr.success else 'FAILURE'} | Steps: {tr.steps}, Charge: {tr.final_charge:.3f}')
                ax.legend(loc='upper left', fontsize=8)
            
            for i in range(n, rows*2): axes[i//2, i%2].set_visible(False)
            plt.suptitle(f'{agent_name} Trajectories', fontsize=14, fontweight='bold')
            plt.tight_layout()
            self._save(fig, 'trajectories', f'{agent_name}_trajectories')
        
        fig, axes = plt.subplots(2, 3, figsize=(18, 12))
        axes = axes.flatten()
        
        for i, (agent_name, trajs) in enumerate(all_trajectories.items()):
            if i >= 6: break
            ax = axes[i]
            self._env(ax)
            if trajs:
                best = min(trajs, key=lambda t: t.steps if t.success else 999)
                if best.trajectory:
                    xs = [s.position[0] for s in best.trajectory]
                    ys = [s.position[1] for s in best.trajectory]
                    ax.plot(xs, ys, color=self.colors[i % 10], linewidth=2, alpha=0.8)
                    ax.scatter(xs[0], ys[0], s=100, c='blue', marker='o', zorder=5)
                    ax.scatter(xs[-1], ys[-1], s=100, c='green' if best.success else 'red', marker='*', zorder=5)
            
            status = 'OK' if trajs and min(trajs, key=lambda t: t.steps if t.success else 999).success else 'FAIL'
            ax.set_title(f'{agent_name+ os.linesep+ status}', fontsize=10)
        
        plt.suptitle('Best Trajectory per Agent', fontsize=14, fontweight='bold')
        plt.tight_layout()
        self._save(fig, 'trajectories', 'all_agents_best')
