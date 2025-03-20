from matplotlib import pyplot as plt
import numpy as np
import sympy as sp
from sklearn.kernel_approximation import RBFSampler
from sklearn.preprocessing import PolynomialFeatures
import numpy as np
import plotly.graph_objects as go
import Data as data
from abc import ABC, abstractmethod

class BaseModel(ABC):
    def __init__(self, x_train, y_train, epochs, alpha, eta, x_test, y_test):
        self.x_train = np.hstack([x_train, np.ones((x_train.shape[0], 1))])
        self.y_train = y_train
        self.x_test = np.hstack([x_test, np.ones((x_test.shape[0], 1))])
        self.y_test = y_test
        self._epochs = epochs
        self._eta = eta
        self._alpha = alpha
        self._weight = np.ones(self.x_train.shape[1])
        self._history_weight = []
        self._history_loss_train = []
        self._history_errors_train = []
        self._history_loss_test = []
        self._history_errors_test = []
        
    def l1_regularization(self):
        return self._alpha * self._weight

    def l2_regularization(self):
        return self._alpha * np.dot(self._weight, self._weight)
    
    
    @abstractmethod
    def loss(self, x_data, y_data):
        pass
    
    @abstractmethod
    def loss_gradient(self, x_data, y_data):
        pass
    
    @abstractmethod
    def fit(self, x_data, y_data, flag = None):
        pass
    
    def plot(self):
        fig, ax = plt.subplots(1, 2, figsize=(14, 6))  

        epochs_range = range(len(self._history_loss_train))

        ax[0].plot(epochs_range, self._history_loss_train, label='Train Loss', color='blue', marker='o')
        ax[0].plot(epochs_range, self._history_loss_test, label='Test Loss', color='red',  marker='o')
        ax[0].set_title('Loss')
        ax[0].set_xlabel('Epochs')
        ax[0].set_ylabel('Loss')
        ax[0].grid()
        ax[0].legend()

        ax[1].plot(epochs_range, self._history_errors_train, label='Train Errors', color='blue', marker='x')
        ax[1].plot(epochs_range, self._history_errors_test, label='Test Errors', color='red',  marker='x')
        ax[1].set_title('Errors')
        ax[1].set_xlabel('Epochs')
        ax[1].set_ylabel('Errors')
        ax[1].grid()
        ax[1].legend()

        plt.tight_layout()  
        plt.show()


    def plot_plane(self, x_range, y_range):
        x_array = np.linspace(x_range[0], x_range[1], 3)
        y_array = np.linspace(y_range[0], y_range[1], 3)
        
        x_steps, y_steps = np.meshgrid(x_array, y_array)
        
        w0, w1, w2 = self._weight[:3]
        b = self._weight[3]
        
        z_steps = -(w0 * x_steps + w1 * y_steps + b) / w2
        plane = go.Surface(x=x_steps, y=y_steps, z=z_steps, colorscale=[[0, 'green'], [1, 'green']], opacity=0.5, showscale=False)
        return plane

    
    def get_3d_with_plane(self, dataset):
        x = dataset['lead time']
        y = dataset['average price']
        z = dataset['special requests']
        color = dataset['booking status'].apply(lambda status: 'blue' if status == 1 else 'orange')

        fig = go.Figure(data=[go.Scatter3d(
            x=x,
            y=y,
            z=z,
            mode='markers',
            marker=dict(
                size=5,
                color=color,  
                opacity=0.5
            )
        )])
        
        x_range = (x.min(), x.max())
        y_range = (y.min(), y.max())
        plane = self.plot_plane(x_range, y_range)
        fig.add_trace(plane)
        
        fig.update_layout(
            scene=dict(
                xaxis_title='Lead Time',
                yaxis_title='Average Price',
                zaxis_title='Special Requests'
            ),
            width=800,
            height=600
        )

        fig.show() 
        
    @staticmethod
    def tune_hyperparameters(x_train, y_train, x_test, y_test, ModelClass, **model_kwargs):
        best_model = None
        best_loss = float('inf')
        
        epochs_values = [10, 20, 50]
        alpha_values = [0.01, 0.1, 1.0]
        eta_values = [0.001, 0.01, 0.1]

        for epochs in epochs_values:
            for alpha in alpha_values:
                for eta in eta_values:
                    model = ModelClass(x_train, y_train, epochs=epochs, alpha=alpha, eta=eta, x_test=x_test, y_test=y_test, **model_kwargs)
                    model.fit()
                    final_loss = model._history_loss_test[-1]

                    if final_loss < best_loss:
                        best_loss = final_loss
                        best_model = model

        print(f"Лучшая модель: epochs={best_model._epochs}, alpha={best_model._alpha}, eta={best_model._eta}, loss={best_loss}")
        return best_model

    def data_validation(self, x_data=None):
        if x_data is None:
            x_data = self.x_test
        cur_loss = sum(self.loss(x_data[i], self.y_test[i]) for i in range(len(self.y_test)))
        cur_bad_count = sum(1 for i in range(len(self.y_test)) if self.y_test[i] * np.dot(self._weight, x_data[i]) < 1)
        return cur_loss, cur_bad_count