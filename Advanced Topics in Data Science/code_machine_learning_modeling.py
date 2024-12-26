################################################################################
# Analysis of the Chicago Crimes over the last 6 years                         #
# Practical Assignment - PART II                                               #
# DM2+TACD                                                                     #
#                                                                              #
# Author:                                                                      #
# Margarida Cardeano Pinheiro    (up201805012@edu.fc.up.pt)                    #
#                                                                              #
################################################################################

import pandas as pd
import numpy as np
from sklearn.utils import resample
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, BaggingClassifier
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score

################################################################################
# 4. MACHINE LEARNING                                                          #
#                                                                              #
#    Target Variable: 'arrest'                                                 #
#                                                                              #
#    Predictors: 'block', 'iucr', 'location_description', 'arrest', 'beat',    #
#                'district', 'ward', 'community_area', 'year', 'time_of_day',  #
#                'quarter', 'week_day'                                         #
#                                                                              #
################################################################################

# Read data
crimes = pd.read_csv('chicago_crimes_traintest.csv', encoding='latin-1')
crimes.drop(['id','case_number','date','description','primary_type','domestic','latitude', 'longitude'], axis=1, inplace=True)

# Create a balanced dataset
# Upsampling False 'arrest' examples and downsampling True 'arrest' examples
falsos = crimes[crimes['arrest']==False]
verdadeiros = crimes[crimes['arrest']==True]
tamanho = round((len(falsos)+len(verdadeiros))/2)
falsos_down = resample(falsos, replace=True, n_samples=tamanho, random_state=123)
verdadeiros_up = resample(verdadeiros, replace =True, n_samples=tamanho, random_state=123)     
eqcrimes = pd.concat([falsos_down, verdadeiros_up])

# Split data into train and test sets
x = eqcrimes.drop(['arrest'], axis=1)
y = eqcrimes['arrest']
xtrain, xtest, ytrain, ytest = train_test_split(x, y, train_size=0.8, stratify=y, shuffle=True, random_state=123)

# Encoding
numerical_features = ['beat', 'district', 'ward', 'community_area', 'year', 'quarter']
enc = LabelEncoder()
for col in xtrain.columns:
    if col not in numerical_features:
        xtrain[col] = enc.fit_transform(xtrain[col])
        xtest[col] = enc.fit_transform(xtest[col])
ytrain = enc.fit_transform(ytrain) # 0 = False, 1 = True
ytest = enc.fit_transform(ytest)

# MODEL 1 - Decision Tree Aproach
model1 = DecisionTreeClassifier(random_state=123)
print('\nTraining Decition Tree Classifier...')
model1.fit(xtrain, ytrain)
acc1 = accuracy_score(y_true=ytest, y_pred=model1.predict(xtest))
pre1 = precision_score(y_true=ytest, y_pred=model1.predict(xtest))
rec1 = recall_score(y_true=ytest, y_pred=model1.predict(xtest))
f1s1 = f1_score(y_true=ytest, y_pred=model1.predict(xtest))

# MODEL 2 - Random Forest Aproach
model2 = RandomForestClassifier(n_estimators=100, random_state=123)
print('\nTraining Random Forest Classifier...')
model2.fit(xtrain, ytrain)
acc2 = accuracy_score(y_true=ytest, y_pred=model2.predict(xtest))
pre2 = precision_score(y_true=ytest, y_pred=model2.predict(xtest))
rec2 = recall_score(y_true=ytest, y_pred=model2.predict(xtest))
f1s2 = f1_score(y_true=ytest, y_pred=model2.predict(xtest))

# MODEL 3 - AdaBoost Aproach
model3 = AdaBoostClassifier(random_state=123, n_estimators=100)
print('\nTraining AdaBoost Classifier...')
model3.fit(xtrain, ytrain)
acc3 = accuracy_score(y_true=ytest, y_pred=model3.predict(xtest))
pre3 = precision_score(y_true=ytest, y_pred=model3.predict(xtest))
rec3 = recall_score(y_true=ytest, y_pred=model3.predict(xtest))
f1s3 = f1_score(y_true=ytest, y_pred=model3.predict(xtest))

# MODEL 4 - Bagging of Trees Aproach
model4 = BaggingClassifier(base_estimator=DecisionTreeClassifier(random_state=123), random_state=123, n_estimators=100)
print('\nTraining Bagging Classifier...')
model4.fit(xtrain, ytrain)
acc4 = accuracy_score(y_true=ytest, y_pred=model4.predict(xtest))
pre4 = precision_score(y_true=ytest, y_pred=model4.predict(xtest))
rec4 = recall_score(y_true=ytest, y_pred=model4.predict(xtest))
f1s4 = f1_score(y_true=ytest, y_pred=model4.predict(xtest))

# Final evaluation of the four approaches with the hidden set
hidden = pd.read_csv('chicago_crimes_hidden.csv', encoding='latin-1')
hidden.drop(['id','case_number','date','description','primary_type','domestic','latitude', 'longitude'], axis=1, inplace=True)
print('\nCalculating final evaluation metrics...\n')

hidden_x = hidden.drop(['arrest'], axis=1)
hidden_y = hidden['arrest']
for col in hidden_x.columns:
    if col not in numerical_features:
        hidden_x[col] = enc.fit_transform(hidden_x[col])
hidden_y = enc.fit_transform(hidden_y) # 0 = False, 1 = True

acc1h = accuracy_score(y_true=hidden_y, y_pred=model1.predict(hidden_x))
acc2h = accuracy_score(y_true=hidden_y, y_pred=model2.predict(hidden_x))
acc3h = accuracy_score(y_true=hidden_y, y_pred=model3.predict(hidden_x))
acc4h = accuracy_score(y_true=hidden_y, y_pred=model4.predict(hidden_x))

pre1h = precision_score(y_true=hidden_y, y_pred=model1.predict(hidden_x))
pre2h = precision_score(y_true=hidden_y, y_pred=model2.predict(hidden_x))
pre3h = precision_score(y_true=hidden_y, y_pred=model3.predict(hidden_x))
pre4h = precision_score(y_true=hidden_y, y_pred=model4.predict(hidden_x))

rec1h = recall_score(y_true=hidden_y, y_pred=model1.predict(hidden_x))
rec2h = recall_score(y_true=hidden_y, y_pred=model2.predict(hidden_x))
rec3h = recall_score(y_true=hidden_y, y_pred=model3.predict(hidden_x))
rec4h = recall_score(y_true=hidden_y, y_pred=model4.predict(hidden_x))

f1s1h = f1_score(y_true=hidden_y, y_pred=model1.predict(hidden_x))
f1s2h = f1_score(y_true=hidden_y, y_pred=model2.predict(hidden_x))
f1s3h = f1_score(y_true=hidden_y, y_pred=model3.predict(hidden_x))
f1s4h = f1_score(y_true=hidden_y, y_pred=model4.predict(hidden_x))

d = {'model':['Decision Tree', 'Random Forest', 'AdaBoost', 'Bagging of Trees'],
     'test accuracy':[acc1, acc2, acc3, acc4], 'test precision':[pre1, pre2, pre3, pre4],
     'test recall':[rec1, rec2, rec3, rec4], 'test f1-score':[f1s1, f1s2, f1s3, f1s4],
     'hidden accuracy':[acc1h, acc2h, acc3h, acc4h],
     'hidden precision':[pre1h, pre2h, pre3h, pre4h],
     'hidden recall':[rec1h, rec2h, rec3h, rec4h],
     'hidden f1-score':[f1s1h, f1s2h, f1s3h, f1s4h]}

# Saving a csv file with the results
results = pd.DataFrame(d)
results.setindex('model')
print(results)
results.to_csv('machine_learning_results.csv')
print('\nResults saved to file: machine_learning_results.csv')


