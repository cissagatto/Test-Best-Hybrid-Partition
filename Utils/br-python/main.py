import sys
import pandas as pd
import numpy as np
from skmultilearn.problem_transform import BinaryRelevance
from sklearn.tree import DecisionTreeRegressor


if __name__ == '__main__':
    
    random_state = 0
    baseModel = DecisionTreeRegressor(random_state = random_state)
    
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    start = int(sys.argv[4])
    end = int(sys.argv[5])
    diretorio = sys.argv[6]
    
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    X_train = train.iloc[:, :start]
    X_train.shape
    
    Y_train = train.iloc[:, start:]
    Y_train.shape
    
    X_test = test.iloc[:, :start]
    X_test.shape
    
    Y_test = test.iloc[:, start:]
    Y_test.shape
    
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)
    
    classifier = BinaryRelevance(baseModel)
    classifier.fit(X_train, Y_train)
    test_predictions = classifier.predict(X_test)
    
    true = (diretorio + "/y_true.csv")
    pred = (diretorio + "/y_pred.csv")
    
    test_predictions_2 = test_predictions.toarray()
    test_predictions_2 = pd.DataFrame(test_predictions_2)
    test_predictions_2.columns = labels_y_test
    
    test_predictions_2.to_csv(pred, index=False)
    test[labels_y_test].to_csv(true, index=False)
    
    
