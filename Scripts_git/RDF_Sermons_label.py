import pandas as pd
import os
import sys
stdout = sys.stdout
reload(sys)
sys.setdefaultencoding('utf-8')
sys.stdout = stdout
import codecs

###Download datset and update the directory to run on another laptop
path = '/Users/nicolobarbagallo/Desktop/Text as data/MSc_dissertation/Data'
os.chdir(path)

with codecs.open(path + '/sermon_labelled_unlabeled.csv', 'r', encoding='utf-8', errors='ignore') as csvfile:
    sermons = pd.read_csv(csvfile)

sermons = sermons.values.tolist()


# # Text preprocessing and normalization
# 
# As with dictionary and unsupervised approaches, the first step is **preprocessing**. We will do the following preprocessing:
# 
# 1. **Tokenize** into unigrams (i.e., words)
# 2. **Remove numbers**
# 3. **Lemmatize**
# 
# Start by loading the necessary functions:


# Tokenization
from nltk.tokenize import RegexpTokenizer

# Lemmatization
from nltk.stem import WordNetLemmatizer


# And then we get to work! Start with **tokenization**:


def tokenize(text):
    return RegexpTokenizer(r'\w+').tokenize(text)

# Tokenize entire corpus of reviews
tokens = [tokenize(row[-1]) for row in sermons]

# And then **numbers**:

import re # remove number function uses a RegEx

def remove_number(text):
    return [token for token in text if re.search(r'^\d+$', token) == None]

tokens = [remove_number(doc) for doc in tokens]

# Instantiate an NLTK lemmatizer object
lemmatizer =  WordNetLemmatizer()

def lemmatize(text, lemmatizer):
    return [lemmatizer.lemmatize(token.decode("utf8", errors='ignore')) for token in text]

tokens = [lemmatize(doc, lemmatizer) for doc in tokens]

print tokens[-1]

# Import required functions
import numpy as np

# Prepare data for sklearn. First, get the text
texts = [' '.join(doc) for doc in tokens]

# And then get the "class" variable
##positive = [row[1] for row in reviews]

supertopic = []
for i in xrange(0,1830):
    print i
    supertopic.append(sermons[i][-2])

print sermons[0:1829][-2]
##Count vecotrizer 

from sklearn.feature_extraction.text import CountVectorizer
# Initialize the vectorizer

vectorizer = CountVectorizer()

# Generate count weights
X = vectorizer.fit_transform(texts)
y = np.array(supertopic)

##Split dataset back into training and unlabeled
X_train = X[0:1830]
X_train_array = X_train.toarray()

X_unlabeled = X[1830:]
X_unlabeled_array = X_unlabeled.toarray()


## Look at Class imbalance 

def get_freq(obj):
    unique, counts = np.unique(obj, return_counts=True)
    print np.asarray((unique, counts)).T
    return unique, counts

print get_freq(y)


##Over-sample dataset to make up for class imbalances
from collections import Counter
from imblearn.over_sampling import RandomOverSampler 

ratio = 'auto'
X_res, y_res = RandomOverSampler(ratio=ratio, random_state=0).fit_sample(X_train, y)

######################################
###Plot dataset class ################
######################################
import matplotlib.pyplot as plt
def plot_pie(y):
    target_stats = Counter(y)
    labels = target_stats.keys()
    sizes = target_stats.values()
    explode = tuple([0.1] * len(target_stats))

    fig, ax = plt.subplots()
    ax.pie(sizes, explode=explode, labels=labels, shadow=True,
           autopct='%1.1f%%')
    ax.axis('equal')
###Plot balanced and imbalanced datset for visual comparison
plot_pie(y)
plot_pie(y_res)

##Export datasets for plotting with GGPLOT2
y_for_freq_plot = pd.DataFrame(y)
y_for_freq_plot.to_csv('y_for_freq_plot.csv')
y_res_for_freq_plot = pd.DataFrame(y_res)
y_res_for_freq_plot.to_csv('y_res_for_freq_plot.csv')


X_res_array = X_res.toarray()

##Split Dataset int for training and validation

from sklearn.model_selection import train_test_split
##Standard datset
X_train_new, X_test, y_train, y_test = train_test_split(X_train_array, y, test_size=0.30, random_state=1234)

##Oversampled dataset
X_train_B, X_test_B, y_train_B, y_test_B = train_test_split(X_res_array, y_res, test_size=0.30, random_state=1234)

##Fit the models

from sklearn import neural_network

##Load model and set parameters for MLP classifier
clf = neural_network.MLPClassifier(hidden_layer_sizes=(1000), max_iter=500, random_state= 0, alpha= 0.001, solver= 'lbfgs')

# Fit the training data
clf_fit = clf.fit(X_train_B, y_train_B)

# Predict the new classes
y_predict_MLP = clf_fit.predict(X_test)

# Import CV functions
from sklearn.metrics import precision_score, recall_score, f1_score
# Write cross validation metrics to list 
results = []
    
Neural_Network = ["Neural Network",['Precision = %s' % precision_score(y_test, y_predict_MLP, average= None),
              'Recall = %s' % recall_score(y_test, y_predict_MLP, average= None),
              'F1 score = %s' % f1_score(y_test, y_predict_MLP, average= None),
              'F1 score macro = %s' % f1_score(y_test, y_predict_MLP, average = 'macro'),
              'F1 score micro = %s' % f1_score(y_test, y_predict_MLP, average = 'micro')]]
              
results.append(Neural_Network)

### LINEAR SVC MODEL
from sklearn.svm import LinearSVC
### LINEAR SVC MODEL (one vs rest)
##from sklearn.svm import SVC
from sklearn.multiclass import OneVsRestClassifier

# Initialize 1 v rest classifer using a
# linear SVC
clf_OVREST=OneVsRestClassifier(LinearSVC())

# Fit the training data
clf_fit_OVREST = clf_OVREST.fit(X_train_B, y_train_B)

# Predict the new classes
y_predict_1vrest = clf_fit_OVREST.predict(X_test)

SVM = ["SVM",['Precision = %s' % precision_score(y_test, y_predict_1vrest, average= None),
              'Recall = %s' % recall_score(y_test, y_predict_1vrest, average= None),
              'F1 score = %s' % f1_score(y_test, y_predict_1vrest, average= None),
              'F1 score macro = %s' % f1_score(y_test,y_predict_1vrest, average = 'macro'),
              'F1 score micro = %s' % f1_score(y_test, y_predict_1vrest, average = 'micro')]]

results.append(SVM)

###RadomForestClassifier model

from sklearn.ensemble import RandomForestClassifier

clf_RFC= RandomForestClassifier()

# Fit the training data

clf_fit_RFC = clf_RFC.fit(X_train_B, y_train_B)

# Predict the new classes
y_predict_RFC = clf_fit_RFC.predict(X_test)

RFC = ["Random Forest",['Precision = %s' % precision_score(y_test, y_predict_RFC, average= None),
              'Recall = %s' % recall_score(y_test, y_predict_RFC, average= None),
              'F1 score = %s' % f1_score(y_test, y_predict_RFC, average= None),
              'F1 score macro = %s' % f1_score(y_test,y_predict_RFC, average = 'macro'),
              'F1 score micro = %s' % f1_score(y_test, y_predict_RFC, average = 'micro')]]

results.append(RFC)
###QDA model
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis

clf_QDA= QuadraticDiscriminantAnalysis()

# Fit the training data

clf_fit_QDA = clf_QDA.fit(X_train_B, y_train_B)

# Predict the new classes
y_predict_QDA = clf_fit_QDA.predict(X_test)

QDA = ["QDA",['Precision = %s' % precision_score(y_test, y_predict_QDA, average= None),
              'Recall = %s' % recall_score(y_test, y_predict_QDA, average= None),
              'F1 score = %s' % f1_score(y_test, y_predict_QDA, average= None),
              'F1 score macro = %s' % f1_score(y_test,y_predict_QDA, average = 'macro'),
              'F1 score micro = %s' % f1_score(y_test, y_predict_QDA, average = 'micro')]]

results.append(QDA)

##Transform results list to Dataframe

##Results_DF = pd.DataFrame(results)
##Results_DF.to_csv('validation_new.csv')
##Best performance = Random Forest


##PREDITC WITH RANDOM FOREST MODEL
## Fit final model using training data
# Fit the training data

clf_fit_RFC_final = clf_RFC.fit(X_res_array, y_res)

# save the model to disk
import pickle
filename = 'clf_fit_RFC_final.sav'
pickle.dump(clf_fit_RFC_final, open(filename, 'wb'))

##load model from pickle

clf_fit_RFC_final = pickle.load(open(path +'/clf_fit_RFC_final.sav', 'rb'))
# Predict unlaabeled data classification

Final_RFC_Class_predictions = clf_fit_RFC_final.predict(X_unlabeled_array)
del sermons[0:1830]

for i in xrange(0,9564):
    sermons[i].append(Final_RFC_Class_predictions[i].tostring())

##Save to pickle
with open(path + '/Final_sermons_labeled.pkl', 'w') as pfile:
    pickle.dump(sermons, pfile)

##Transform to dataframe and save DF
Final_sermons_labeled = pd.DataFrame(sermons)
Final_sermons_labeled.to_csv('Final_sermons_labeled.csv') 


