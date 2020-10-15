# -*- coding: utf-8 -*-
"""
Created on Sun Jul  8 15:42:26 2018

"""
import pandas as pd #Pandas, a module for DataFrames (tables) and working with data
#https://pandas.pydata.org/pandas-docs/stable/
#https://pandas.pydata.org/pandas-docs/stable/cookbook.html
import numpy as np #Numpy, Python's main numbers library

def main():
    data = createData()
    #print(data)
    displayData(data)
    #processData(data)

def createData():
    """
    Creates the data from random numbers and returns it
    """
    randNum = np.random.randint(0,100,size=(100,5)) #100x5 table of random integers
    df = pd.DataFrame(randNum,columns=list('ABCDE'))
    return df

def displayData(data):
    print(data.head(5)) #Prints first 5 entries in data
    print(data.tail(5)) #Prints last 5 entries in data
    print(data.shape)
    print(data.describe())

def processData(data):
    """
    Shows off a few ways to manipulate DataFrames with pandas
    """
    
    data["F"] = data["A"] + data["B"]   #Make new column F out of A + B
    data["C"] = data["C"]*2 #Double the C column
    
    singleDigitA = data.loc[data["A"]<10,:] #Get the entirety of all rows where A < 10
    print(singleDigitA)

    getAForEvenB = data.loc[data.B % 2 == 0, ["A"]]
    print(getAForEvenB)    
    
    point = data.lookup([17],["C"]) #Gets point at row index 17, column C
    print(point)
    points = data.lookup([1,2,3],["A","B","C"]) #Gets point at (1,"A"), (2,"B"), (3,"C")
    print(points)
    
    labels = ["X","Y","Z"]
    numRows = data.shape[0] #How many rows do we have
    data["Label"] = np.random.choice(labels,numRows,replace=True)
    
    print(data.groupby('Label').size())
    print(data.groupby('Label').mean())
    
    data["HasBig"] = data.apply(findBigNum,axis=1,args=(150,))
    print(data.head(5))
    
def findBigNum(row,bigNum=100):
    """
    Returns True for rows that have at least one entry as big 
    or bigger than bigNum
    """
    return any([isinstance(ii,int) and ii >= bigNum for ii in row])


main() #Set everything in motion
