import sys
import pandas as pd
from ecc import ECC
from sklearn.tree import DecisionTreeRegressor

if __name__ == '__main__':
    n_chains = 5
    random_state = 0
    baseModel = DecisionTreeRegressor(random_state = random_state)
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    partitions = pd.read_csv(sys.argv[4])
    
    directory = sys.argv[5]
    print(directory)
    
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    clusters = partitions.groupby("group")["label"].apply(list)   
    allLabels = partitions["label"].unique()
    x_train = train.drop(allLabels, axis=1)
    y_train = train[allLabels]
    x_test = test.drop(allLabels, axis=1)
    ecc = ECC(baseModel,
            n_chains)
    ecc.fit(x_train,
            y_train,
            clusters,
            )
    test_predictions = pd.DataFrame(ecc.predict(x_test))
    train_predictions = pd.DataFrame(ecc.predict(x_train))
    
    true = (directory + "/y_true.csv")
    pred = (directory + "/y_pred.csv")
    
    #print(true)
    #print(pred)
    
    
    # test_predictions.to_csv("y_pred.csv", index=False)
    # test[allLabels].to_csv("y_true.csv", index=False)
    
    test_predictions.to_csv(pred, index=False)
    test[allLabels].to_csv(true, index=False)
    
