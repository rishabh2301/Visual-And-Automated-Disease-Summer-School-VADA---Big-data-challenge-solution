# -*- coding: utf-8 -*-
"""
Created on Sun Jul  8 16:16:34 2018

"""
import pandas as pd
from sklearn.model_selection import train_test_split, cross_val_score, KFold
from sklearn.metrics import classification_report, confusion_matrix, roc_auc_score,roc_curve
from sklearn.linear_model import LogisticRegression
from sklearn.feature_selection import RFE
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.preprocessing import StandardScaler, RobustScaler
import matplotlib.pyplot as plt
import statsmodels.api as sm
import numpy as np
import warnings
warnings.filterwarnings("ignore")

'''
Previously obtained features from the RFE model; empty if we have none

rfeFeatures = ["euribor3m","job_retired","job_student","month_apr",
               "month_mar","month_may","month_nov","poutcome_failure",
               "poutcome_nonexistent","poutcome_success"]

rfeFeatures = ['time_in_hospital','num_lab_procedures','num_procedures',
 'num_medications', 'age_[30, 60)', 'age_[60, 100)', 'race_AfricanAmerican', 
 'race_Caucasian', 'primary_diag_Circulatory',
 'discharge_Discharge to home', 'discharge_Other', 'hba1c_None']
'''
rfeFeatures = []
pd.set_option('display.expand_frame_repr', True) #Set to false to show more columns

#seed = np.random.randint(1,100) #Get consistent breakdown across models

def main(data,filename,keyColumn,modelName,model):
    """
    Calls the other functions
    """
    features = recursiveFeatureElimination(data,keyColumn,model,10)
    xTrain, xValid, yTrain, yValid, scale = classificationSetup(data,0.3,keyColumn,features)
    results = runAlgorithms(xTrain, xValid, yTrain, yValid, scale, modelName,model,'roc_auc')
    return results

def readData(filename):
    """
    Reads in the data from the CSV file with name filename and returns it
    """
    table = pd.read_csv(filename, header=0)
    if "Unnamed" in table.columns.values[0]:
        table.set_index("Unnamed: 0",inplace=True)
    return table

def showCrosstabFrequency(data,xx,yy):
    """
    Compute and display crosstab frequency between xx and yy
    in dataset data. Show results as 100% stacked bar chart
    """
    ct = pd.crosstab(data[xx],data[yy])
    ct = ct.div(ct.sum(1),axis=0) #Normalize
    ct.plot(kind='bar',stacked=True)
    plt.title("{0} frequency across {1}".format(xx.title(),yy.title()))
    plt.xlabel(xx)
    plt.ylabel("Frequency")
    plt.show()
    

def viewData(data,keyVar,cutoff=20):
    """
    Display some information about the data
    and show graphs of variables against the key variable
    """
    print(data[keyVar].value_counts()) #A count of our key variable
    print(data[keyVar].value_counts(normalize=True)) #A count of our key variable
    print(data.groupby(keyVar).mean())
    for col in data.columns.values:
        kinds = data[col].unique()
        if col != keyVar and len(kinds) < cutoff:
            showCrosstabFrequency(data,col,keyVar)
            
def recursiveFeatureElimination(data,keyVar,model,features=None):
    """
    Use RFE to help identify features to use in the model
    Note: This doesn't work for KNN so we have to run LR first
    and keep the features from that
    """
    global rfeFeatures
    if rfeFeatures:
        if isinstance(model,LogisticRegression):
            featureCheck(*separateKeyVar(data,keyVar,rfeFeatures))
        return rfeFeatures
    else:
        xx,yy = separateKeyVar(data,keyVar)
        #Optional second argument tells it how many features to pick
        #Default is half
        rfe = RFE(model,features) 
        fit = rfe.fit(xx,yy)
        print(fit.support_)
        print(fit.ranking_)
        useCols = xx.columns[fit.support_].values
        print(useCols)
        
        featureCheck(*separateKeyVar(data,keyVar,useCols))
        rfeFeatures = useCols.tolist()
        return useCols

def featureCheck(xx,yy):
    logModel = sm.Logit(yy,xx)
    results = logModel.fit()
    print(results.summary())
    
def separateKeyVar(data,keyVar,useCols=None):
    """
    Separates data into two pieces:
        1. Containing all but keyVar
        2. Containing keyVar
    If useCols is not None, returns only those in the xData
    """
    if useCols is None:
        xColumns = data.columns.tolist() #Get column names
        xColumns.remove(keyVar) #Remove key column
    else:
        xColumns = useCols
    xData = data.loc[:,xColumns] #Get non-key columns
    yData = data.loc[:,keyVar] #Get key column
    return xData,yData

def cleanData(data):
    """
    Cleans up the data, including making dummies out of categorical variables
    """
    data = data.loc[data["gender"] != "Unknown/Invalid"] #Fix Rob's mistake
    return pd.get_dummies(data)


def classificationSetup(data,valSize,keyColumn,features=None):
    """
    Prepares the data for analysis
    """
    xx,yy = separateKeyVar(data,keyColumn,features)
    xData,yData = xx.values, yy.values #Don't want DataFrames here
    xTrain, xValid, yTrain, yValid = train_test_split(xData,yData,test_size=valSize)#,random_state=seed)
    #scaler = StandardScaler().fit(xTrain)
    scaler = RobustScaler().fit(xTrain)
    return xTrain, xValid, yTrain, yValid, scaler

def runAlgorithms(xTrain, xValid, yTrain, yValid, scale, modelTxt, model, scoring='accuracy', folds=10):
    """
    Runs an algorithm on the data to do some machine learning analysis
    """
    trainScaled = scale.transform(xTrain)
    validScaled = scale.transform(xValid)
    kfold = KFold(n_splits=folds)#, random_state=seed)
    cv_results = cross_val_score(model, trainScaled, yTrain, cv=kfold, scoring=scoring)
    msg = "{0:5s}: {1:.6f} ({2:.6f})".format(modelTxt, cv_results.mean(), cv_results.std())
    print(msg)
    
    #if modelTxt == "LR":
    predictions(trainScaled, validScaled, yTrain, yValid,model, [tt/100 for tt in range(101)])
    #else:
    #    predictions(xTrain, xValid, yTrain, yValid,model)
    return cv_results
    
def adjustClasses(yScores, threshold):
    """
    Adjust class predictions to a given threshold
    """
    return [1 if yy >= threshold else 0 for yy in yScores]
    
def showROC(xx,yy,predict,probs):  
    rocAuc = roc_auc_score(yy, predict)
    fpr, tpr, thresholds = roc_curve(yy, probs)
    plt.figure()
    plt.plot(fpr, tpr, label='Logistic Regression (area = %0.2f)' % rocAuc)
    plt.plot([0, 1], [0, 1],'r--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic')
    plt.legend(loc="lower right")
    plt.savefig('Log_ROC')
    plt.show()
    
def compareAlgorithms(results,names):
    fig = plt.figure()
    fig.suptitle('Algorithm Comparison')
    ax = fig.add_subplot(111)
    plt.boxplot(results)
    ax.set_xticklabels(names)
    plt.show()

def predictions(xTrain, xValid, yTrain, yValid, alg, thresholds=[]):
    """
    Evaluates the predictive power of our algorithm
    """
    fit = alg.fit(xTrain, yTrain) #Fit the algorithm to our data
    predictions = alg.predict(xValid) #Make predictions
    try:
        probs = fit.predict_proba(xValid)[:,1]
    except:
        probs = fit.decision_function(xValid)
        
    #showROC(xValid,yValid,predictions,probs)
    print("Results with Default Threshold")
    rocauc = roc_auc_score(yValid,predictions)
    confuse = confusion_matrix(yValid, predictions)
    report  = classification_report(yValid, predictions) 
    print(rocauc)
    print(confuse)
    print(report)   
    
    results = []
    for tt in thresholds:
        predictions = adjustClasses(probs,tt)
        rocauc = roc_auc_score(yValid,predictions)
        confuse = confusion_matrix(yValid, predictions)
        report  = classification_report(yValid, predictions)
        results.append((rocauc,"K = {0}".format(tt),confuse,report))
    results.sort(key=lambda x: x[0],reverse=True) #Sorts by first item
    for item in results[0]:
        print(item)
   
    '''
    Classification Report Terms:
        Support: Sum of row
        Precision: Column correct / total column
        Recall: Row correct / total row
        f1-score: Harmonic Mean of Precision and Recall
        
    Statistician Names:
        - Precision = Positive Predictive Value
        - Recall = Sensitivity
        - They don't have an F1 score. I don't have a Specificity Score
    
    '''

def multiModel(file,keyVar):
    data = readData(file)
    #viewData(data,keyVar)
    data = cleanData(data)
    models = []
    models.append(('LR', LogisticRegression()))
    models.append(('KNN', KNeighborsClassifier()))
    models.append(('CART', DecisionTreeClassifier()))
    results = []
    names = []
    for txt, model in models:
        rr = main(data,file,keyVar,txt,model)
        results.append(rr)
        names.append(txt)
        print("-"*30)
    compareAlgorithms(results,names)

multiModel("diabetes_data.csv","readmit_30")
#main("diabetes_data-1000x9.csv","readmit_30")
#multiModel("../banking.csv","y")
#main("../iris.csv","IsYellow")
