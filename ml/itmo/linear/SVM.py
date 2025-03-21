from sklearn.kernel_approximation import RBFSampler
from sklearn.preprocessing import PolynomialFeatures
import numpy as np
import BaseModel

class MySVM(BaseModel):
    def __init__(self, x_train, y_train, epochs, alpha, eta, kernel, x_test, y_test, degree=3, gamma=1.0):
        super().__init__(x_train, y_train, epochs, alpha, eta, x_test, y_test)
        self.kernel = kernel
        self.degree = degree  
        self.gamma = gamma    
        self.x_test_kernel = self._apply_kernel(self.x_test)

    def _apply_kernel(self, X):
        if self.kernel == 'linear':
            return X
        elif self.kernel == 'polynomial':
            return self._polynomial_features(X, self.degree)
        elif self.kernel == 'rbf':
            return self._rbf_features(X, self.gamma)
        else:
            raise ValueError(f"Unsupported kernel: {self.kernel}")

    def _polynomial_features(self, X, degree):
        poly = PolynomialFeatures(degree)
        return poly.fit_transform(X)

    def _rbf_features(self, X, gamma):
        rbf_feature = RBFSampler(gamma=gamma, random_state=1)
        return rbf_feature.fit_transform(X)

    def loss(self, x, y):
        regularization = (self._alpha / 2) * np.dot(self._weight, self._weight)
        hinge_loss = max(0, 1 - y * np.dot(x, self._weight))
        total_loss = regularization + hinge_loss
        return total_loss

    def loss_gradient(self, x_data, y_data):
        if y_data * np.dot(self._weight, x_data) >= 1:
            gradient = 2 * self._alpha * self._weight
        else:
            gradient = 2 * self._alpha * self._weight - y_data * x_data
        return gradient
    
    def fit(self, x_data=None, y_data=None, flag=None):
        x_data = self._apply_kernel(self.x_train) if x_data is None else self._apply_kernel(x_data)
        y_data = self.y_train if y_data is None else y_data

        for cur_epoch in range(self._epochs):
            gradient = self.loss_gradient(x_data, y_data)
            self._weight -= self._eta * gradient
            cur_loss_train = sum(self.loss(x_data[i], y_data[i]) for i in range(len(x_data)))
            self._history_loss_train.append(cur_loss_train)
            self._history_loss_test.append(sum(self.loss(self._apply_kernel(self.x_test)[i], self.y_test[i]) for i in range(len(self.x_test))))

    def data_validation(self):
        return super().data_validation(x_data=self.x_test_kernel)
