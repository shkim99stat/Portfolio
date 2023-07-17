import pickle
import pandas as pd 
import numpy as np
from sklearn.model_selection import train_test_split
import sklearn
from sklearn.decomposition import PCA
import random
import tensorflow as tf
import torch
import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px


# Read "X_data.pkl" and "y_data.pkl" files to X and y ==========
with open(file = 'X_data.pkl',mode = 'rb') as f:
    X = pickle.load(f)
    
with open(file = 'y_data.pkl',mode = 'rb') as g:
    y = pickle.load(g)
    
# column = sensor , row = time

print(X)
print(y)
print('X length : ', len(X))
print('y length : ', len(y))

print('number of y == 1 : ', len(y[y == 1]))        # 1915 
print('number of y == -1 : ', len(y[y == -1]))      # 2021  => normal and abnormal values are similar. (not imbalance)


# Train - Validation - Test split ==========
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size = 0.2, shuffle = False, random_state = 500)   # train : test = 8 : 2

print('X_train-shape',X_train.shape)
print('X_test-shape',X_test.shape)
print('y_train-shape',y_train.shape)
print('y_test-shape',y_test.shape)

type(X_train)       # type check : numpy.ndarray


#=====================================
sns.set_theme(style = 'darkgrid')
for i in np.arange(0,500):
    sns.kdeplot(X[i,:], legend = False)
    print('mean: ', np.mean(X[i,:]), 'std: ', np.std(X[i,:])) 
#=====================================
Xy_train = np.c_[X_train,y_train]
Xy_test = np.c_[X_test,y_test]

print(Xy_train)

Xy_train_p1 = Xy_train[Xy_train[:,500] == 1,:]      # y = 1 case
Xy_train_m1 = Xy_train[Xy_train[:,500] == -1,:]     # y = -1 case

f, axs = plt.subplots(1,1, figsize=(15,4))
rand_num1 = random.randint(0,500)
rand_num2 = random.randint(0,500)
plt.scatter(np.arange(0,500), Xy_train_p1[rand_num1,:-1], marker = 'o', color = 'b', label = 'y = 1')
plt.scatter(np.arange(0,500), Xy_train_m1[rand_num2,:-1], marker = '.', color = 'r', label = 'y = -1')

plt.legend()
#=====================================
corr = np.corrcoef(X_train[:].T)
corr
sns.heatmap(corr)
#=====================================

pca = PCA(random_state=500)
pca.fit(X_train)

print(pca.explained_variance_ratio_)

plt.bar(range(0,len(pca.explained_variance_ratio_[0:79])), pca.explained_variance_ratio_[0:79], alpha = 0.5, align = 'center', label = 'Individual explained variance')
plt.step(range(0,len(np.cumsum(pca.explained_variance_ratio_[0:79]))), np.cumsum(pca.explained_variance_ratio_[0:79]), where = 'mid', label = 'Cumulative explained variance')
plt.axhline(y=0.95, xmin = 0, xmax = 80, color = 'r')
plt.ylabel('Explained variance ratio')
plt.xlabel('Principla component index')
plt.legend(loc='best')
plt.tight_layout()
plt.show()

