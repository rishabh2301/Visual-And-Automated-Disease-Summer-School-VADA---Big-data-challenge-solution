# -*- coding: utf-8 -*-
"""
Created on Sun Jul  8 16:35:29 2018

"""
import pandas as pd


def main():
    """
    Change parameters here to change output
    - Change filename to read different data
    - Change rows to change number of rows read. If < 1, gives that percentage
    - Change the column list to change the columns saved; use All to show all
    """
    columnSelects = {
            "All": [],
            "JustTime":["time_in_hospital","readmit_30"],
            "Numeric":["patient_nbr","time_in_hospital","num_lab_procedures",
                       "num_procedures","num_medications","number_outpatient",
                       "number_emergency","number_inpatient","readmit_30"],
            "Demographics":["patient_nbr","age","gender","race","readmit_30"]
            }
    
    filename = "diabetes_data"
    dataset = pd.read_csv(filename+".csv", header=0,index_col=0)
    rows = 1000
    cols = columnSelects["Numeric"]
    sliced = sliceData(dataset,rows)
    saveData(sliced,filename,cols)
    
def sliceData(data,numRows):
    """
    Slices the full dataset for simpler analysis
    """
    nn = int(numRows) if numRows > 1 else None
    frac = numRows if numRows < 1 else None
    return data.sample(n=nn,frac=frac)

def saveData(data,baseName,columns):
    """
    Saves data to (hopefully) new CSV file
    """
    numRows = data.shape[0]
    numCols = len(columns)
    filename = "{0}-{1}x{2}.csv".format(baseName,numRows,numCols)
    data.to_csv(filename,columns=columns)
    
main()
