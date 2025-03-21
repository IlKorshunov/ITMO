import pandas as pd
import numpy as np
import seaborn as sns
import plotly.graph_objects as go
from sklearn.discriminant_analysis import StandardScaler
from sklearn.model_selection import train_test_split

dataset = None

def get_data():
    global dataset 
    dataset = pd.read_csv('booking.csv')
    columns_to_keep = ['lead time', 'average price', 'special requests', 'booking status']
    dataset = dataset[columns_to_keep]
    dataset['booking status'] = dataset['booking status'].replace({'Canceled' : -1, 'Not_Canceled' : 1})
    dataset = dataset.head(5000)
    print(dataset.shape)
    return dataset
    
def scalars():
    standard_scaler = StandardScaler()
    scaled_price = standard_scaler.fit_transform(dataset[['average price']])
    log_time = np.log1p(dataset['lead time'])
    dataset['average price'] = scaled_price
    dataset['lead time'] = log_time
    
def split_data():
    x_train, x_test, y_train, y_test = train_test_split(
        dataset.drop(['booking status'], axis=1),
        dataset['booking status'],
        test_size=0.8,
        random_state=2
    )

    x_train = x_train.drop_duplicates().reset_index(drop=True)
    y_train = y_train.reset_index(drop=True)

    x_test = x_test.drop_duplicates().reset_index(drop=True)
    y_test = y_test.reset_index(drop=True)

    x_train = x_train.iloc[:500].reset_index(drop=True)
    y_train = y_train.iloc[:500].reset_index(drop=True)
    x_test = x_test.iloc[:1500].reset_index(drop=True)
    y_test = y_test.iloc[:1500].reset_index(drop=True)
    
    return x_train, x_test, y_train, y_test


