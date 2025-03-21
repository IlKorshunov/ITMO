import numpy as np
import BaseModel

class MyLinearModel(BaseModel):
    def __init__(self, x_train, y_train, epochs, alpha, eta, x_test, y_test):
        super().__init__(x_train, y_train, epochs, alpha, eta, x_test, y_test)

    def SVD_Decompsition(self, matrix):
        U, S, Vt = np.linalg.svd(matrix)
        return (U, S, Vt)

    def loss(self, x_data, y_data):
        reg = super().l2_regularization()
        dists = np.sum((np.dot(x_data, self._weight) - y_data) ** 2)
        return (dists + reg) / x_data.shape[0]

    def loss_gradient(self, x_data, y_data):
        return 2 * x_data.T @ (np.dot(x_data, self._weight) - y_data) + 2 * self._alpha * self._weight

    def update_weights_with_grad(self, x_data, y_data):
        for cur_epoch in range(self._epochs):
            cur_loss = self.loss(x_data, y_data)
            self._history_loss_train.append(cur_loss)
            cur_grad = self.loss_gradient(x_data, y_data)
            self._weight -= self._eta * cur_grad

    def update_weight_with_squares(self, x_data, y_data):
        U, S, Vt = self.SVD_Decompsition(x_data.T @ x_data)
        S_inv = np.diag(1 / S)
        x_data_inverse = Vt.T @ S_inv @ U.T
        self._weight = x_data_inverse @ x_data.T @ y_data

    def fit(self, x_data=None, y_data=None, flag=None):
        x_data = self.x_train if x_data is None else x_data
        y_data = self.y_train if y_data is None else y_data
        if flag:
            self.update_weights_with_grad(x_data, y_data)
        else:
            self.update_weight_with_squares(x_data, y_data)

        self._history_loss_train.append(self.loss(self.x_train, self.y_train))
        if self.x_test is not None:
            self._history_loss_test.append(self.loss(self.x_test, self.y_test))
