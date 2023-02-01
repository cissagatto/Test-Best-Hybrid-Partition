import sys
from sklearn.tree import DecisionTreeRegressor
from skml.ensemble import EnsembleClassifierChain
import numpy as np
import pandas as pd

if __name__ == '__main__':

    n_chains = 5
    random_state = 0
    baseModel = DecisionTreeRegressor(random_state = random_state)
   
    train = pd.read_csv(sys.argv[1])
    test = pd.read_csv(sys.argv[2])
    start = int(sys.argv[3])
    directory = sys.argv[4]
    
    # TREINO
    X_train = train.iloc[:, :start] # atributos 
    Y_train = train.iloc[:, start:] # rótulos 
    
    # TESTE
    X_test = test.iloc[:, :start] # atributos
    Y_test = test.iloc[:, start:] # rótulos verdadeiros
    
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)

    ensemble = EnsembleClassifierChain(baseModel)
    ensemble.fit(X_train.values, Y_train.values)
    
    y_pred = pd.DataFrame(ensemble.predict(X_test.values)) 
    y_pred.columns = labels_y_test
    y_true = pd.DataFrame(Y_test)
    
    true = (directory + "/y_true.csv")
    pred = (directory + "/y_pred.csv")
    
    y_pred.to_csv(pred, index=False)
    y_true.to_csv(true, index=False)

