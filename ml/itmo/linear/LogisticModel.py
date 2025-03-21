import numpy as np
import math
import BaseModel

class LogisticModel(BaseModel):
    def __init__(self, x_train, y_train, epochs, alpha, eta, x_test, y_test):
        super().__init__(x_train, y_train, epochs, alpha, eta, x_test, y_test)
    
    def get_sigmoid(self, x_data):
        return 1 / (1 + np.exp(-np.dot(self._weight, x_data)))

    def loss(self, x_data, y_data):
        loss = 0
        for index in range(len(x_data)):
            curX = x_data[index]
            curY = y_data[index]
            sigmoid = self.get_sigmoid(curX)
            loss += curY * np.log(sigmoid) + (1 - curY) * np.log(1 - sigmoid)
        return -loss

    def loss_gradient(self, x_data, y_data):
        gradient = 0
        for index in range(len(x_data)):
            curX = x_data[index]
            curY = y_data[index]
            sigmoid = self.get_sigmoid(curX)
            gradient += curX * (curY - sigmoid)
        return -gradient 

    def fit(self):
        for epoch in range(self._epochs):
            gradient = self.loss_gradient(self.x_train, self.y_train)
            self._weight -= self._eta * gradient
            self._history_weight.append(self._weight.copy())
            self._history_loss_train.append(self.loss(self.x_train, self.y_train))
            if self.x_test is not None:
                self._history_loss_test.append(self.loss(self.x_test, self.y_test))
