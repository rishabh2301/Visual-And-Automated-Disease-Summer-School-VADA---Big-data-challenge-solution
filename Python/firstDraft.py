# -*- coding: utf-8 -*-
"""
Created on Sun Jul  8 16:16:34 2018

"""
import pandas as pd
from sklearn.model_selection import train_test_split, cross_val_score, KFold
from sklearn.metrics import classification_report, confusion_matrix, roc_auc_score
from sklearn.linear_model import LogisticRegression

#seed = np.random.randint(1,100) #Get consistent breakdown across models
model = LogisticRegression() #Only one model for now

def main(filename,keyColumn):
    """
    Calls the other functions
    """
    data = readData(filename)
    data = cleanData(data)
    testSize = 0.3
    xTrain, xValid, yTrain, yValid = classificationSetup(data,testSize,keyColumn)
    runAlgorithms(xTrain, xValid, yTrain, yValid,'roc_auc')
    predictions(xTrain, xValid, yTrain, yValid,model)

def readData(filename):
    """
    Reads in the data from the CSV file with name filename and returns it
    """
    return pd.read_csv(filename, header=0,index_col=0)

def cleanData(data):
    """
    Cleans up the data, including making dummies out of categorical variables
    """
    return pd.get_dummies(data)


def classificationSetup(data,valSize,keyColumn):
    """
    Prepares the data for analysis
    """
    xColumns = data.columns.tolist() #Get column names
    xColumns.remove(keyColumn) #Remove key column
    xData = data.loc[:,xColumns].values #Get values for non-key column
    yData = data.loc[:,keyColumn].values #Get values for key column
    
    xTrain, xValid, yTrain, yValid = train_test_split(xData,yData,test_size=valSize)#,random_state=seed)
    return xTrain, xValid, yTrain, yValid

def runAlgorithms(xTrain, xValid, yTrain, yValid, scoring='accuracy', folds=3):
    """
    Runs an algorithm on the data to do some machine learning analysis
    """
    kfold = KFold(n_splits=folds)#, random_state=seed)
    cv_results = cross_val_score(model, xTrain, yTrain, cv=kfold, scoring=scoring)
    msg = "{0:5s}: {1:.6f} ({2:.6f})".format("LR", cv_results.mean(), cv_results.std())
    print(msg)

def predictions(xTrain, xValid, yTrain, yValid, alg):
    """
    Evaluates the predictive power of our algorithm
    """
    alg.fit(xTrain, yTrain) #Fit the algorithm to our data
    predictions = alg.predict(xValid) #Make predictions
    print(roc_auc_score(yValid,predictions))
    print(confusion_matrix(yValid, predictions))
    print(classification_report(yValid, predictions))

main("diabetes_data.csv","readmit_30")
