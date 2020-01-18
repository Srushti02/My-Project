# Support Vector Machines (SVM) classification
# dataset: credit risk (project)

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from collections import Counter

# read the file
path="C:/Users/Yash Patil/Desktop/DATA ANALYSIS/python/Project/cr_train.csv"
cr_train=pd.read_csv(path)
path="C:/Users/Yash Patil/Desktop/DATA ANALYSIS/python/Project/cr_validate.csv"
cr_validate=pd.read_csv(path)

#to know the columns
cr_train.columns
cr_validate.columns

cr_train.shape
cr_validate.shape

cr_train.head()
cr_train.tail(3)

#there are 2 different name of last colume so we do the same
cr_train.rename(columns={"Loan_Status":"outcome"},inplace=True)

#combine the both the data train and validate
all=pd.concat([cr_train,cr_validate],axis=0)
all.shape

#reset the index
all.reset_index(inplace=True,drop=True)
all.tail()

# do the usual checks
all.isnull().sum()


#missing value are filled by male
print 
all[all['Gender'].isnull()].index.tolist()

#
gender_null=all[all['Gender'].isnull()].index.tolist()

all['Gender'].iloc[gender_null]="Male" 

print 
sum(all['Gender'].isnull())
Counter(all['Gender'])

#lets fill married now
#most are married
print 
Counter(all['Married'])

#lets fill them yes if they have dependents else no

pd.crosstab(all['Married'].isnull(),all['Dependents'].isnull())

married_null=all[all['Married'].isnull()].index.tolist()

married_null

all['Married'].iloc[married_null]="Yes"

all.isnull().sum()

# for dependents column
print 

pd.crosstab(all['Married'].isnull(),all['Dependents'].isnull())

Depend_null=all[all['Dependents'].isnull()].index.tolist()

Depend_null

all['Dependents'].iloc[Depend_null]="Yes"
all.isnull().sum()

# fill missing value of self_employed
Counter (all['Self_Employed'])
self_emp_null=all[all['Self_Employed'].isnull()].index.tolist()
all['Self_Employed'].iloc[self_emp_null]="No"
all.isnull().sum()



pd.crosstab(all['LoanAmount'].isnull(),all['Loan_Amount_Term'].isnull())
all.groupby(all['Loan_Amount_Term'])['LoanAmount'].mean()

all['LoanAmount'][(all['LoanAmount'].isnull()) & (all['Loan_Amount_Term']==360)]=144

all['LoanAmount'][(all['LoanAmount'].isnull())]=130

( all['Loan_Amount_Term']).value_counts()

all['Loan_Amount_Term'][all['Loan_Amount_Term'].isnull()]=360
all.isnull().sum()


( all['Loan_Amount_Term']).value_counts()

pd.crosstab(all['Gender'],all['Credit_History'])

#fill missing value using logistics reg
all.columns
all_new=pd.get_dummies(all.drop(['Loan_ID'],axis=1),drop_first=True)
all_new.head()

all_new.isnull().sum()

#split credit history into train and test
test=all_new[all_new['Credit_History'].isnull()] 
all_in_test=test.index.tolist()
test.head() 

all_in_train=[x for x in all_new.index.tolist() if x not in all_in_test]

train=all_new.iloc[all_in_train]
train.shape

X_train=train.drop(['outcome_Y','Credit_History'],axis=1)
Y_train=train['Credit_History']

X_test=test.drop(['outcome_Y','Credit_History'],axis=1)
Y_test=test['Credit_History']

#logistics Regression
from sklearn.linear_model import LogisticRegression
log_reg=LogisticRegression()
log_reg.fit(X_train,Y_train)
 
pred=log_reg.predict(X_test)

test['Credit_History']=pred
print(pred )

df_all=pd.concat([train,test],axis=0)
df_all.shape
df_all.head()
df_all.isnull().sum()

#split original 
train2=df_all.head(len(cr_train))
test2=df_all.tail(len(cr_validate))

X_train=train2.drop(['outcome_Y'],axis=1)
Y_train=train2['outcome_Y']
X_test=test2.drop(['outcome_Y'],axis=1)
Y_test=test2['outcome_Y']

#svm model
from sklearn.svm import SVC
model=SVC()
model.fit(X_train,Y_train)

pred=model.predict(X_test)

from sklearn.metrics import classification_report,confusion_matrix

print(classification_report(Y_test,pred))

print (confusion_matrix(Y_test,pred))

print(test2 ['outcome_Y'])
baseline_accuracy=float(282)/(280+85)
baseline_accuracy
Accuracy=float(282+1)/(280+1+84)
Accuracy


-------------------------------------------------------------
#cv to get optimal c

from sklearn.model_selection import GridSearchCV
paran_grid={'C':[1,0.01],'gamma':[1,0.08,0.09],'Kernal':['linear']}

grid=GridSearchCV(SVC(),paran_grid,verbose=2)

grid.fit(X_train,Y_train)

print grid.best_params_
print grid.best_estimator_

grid_pred=grid.predict(X_test)
   
Counter(grid_pred)

print(confusion_matrix(Y_test,grid_pred))


---------------------------------
#logistics regression
from sklearn.linear_model import LogisticRegression
log_reg=LogisticRegression()
log_reg.fit(X_train,Y_train)
 
#prediticton
p1=log_reg.predict(X_test)
p1[0:10]
c1=len(p1[p1<=0.5])
c2=len(p2[p2<=0.5])

print("<=0.5 {},>0.5{}".format(c1,c2))

#convert probabilities to classes
import copy
predY = copy.deepcopy(p1)

predY[predY<=0.5] = 0
predY[predY>0.5] = 1

predY.value_counts()
Counter('predY')

#confusion matrix
confusion_matrix(Y_test,predY)

#classification report

print(classification_report(Y_test,predY))


























