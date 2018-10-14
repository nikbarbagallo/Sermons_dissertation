#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Sep  3 21:25:38 2018

@author: nicolobarbagallo
"""

#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 18 09:51:11 2018

@author: nicolobarbagallo
"""

##Import packages
import sys
reload(sys)  # Reload does the trick!
sys.setdefaultencoding('UTF-8')
import pickle
import re
import pandas as pd
import os

##Define functions

def lookup_keyword(text, keyword):
    if re.search(keyword, text.lower()) != None:
        return True
    else:
        return False

def lookup_keyword_delete(text, keyword, emptylistoflists):
    if re.search(keyword, text.lower()) != None:
        emptylistoflists[i].append(text)
    else:
        del text
        
def flatten(list_of_lists):
    return [item for sublist in list_of_lists for item in sublist]

##Define Keyword list
    
gay = [u'gay marriage',
       u'gay',
       u'gay ',
       u'gay lifestyle',
       u'same-sex',
       u'same-sex ',
       u'same sex',
       u'same sex ',
       u'homosexu*',
       u'same-sex union*',
       u'gay union*',
       u'homosexuality']

###Download datset and update the directory to run on another laptop
##Define directory and set WD 
path = '/Users/nicolobarbagallo/Desktop/Text as data/MSc_dissertation/Data' 
os.chdir(path)

###Select sermons including homosexuality keywords

with open(path + '/gay_content.pkl', 'r') as f:
    gay_content= pd.read_pickle(f)   
    
gay_full_NOSODOM = []

for i,sermon in enumerate(gay_content):
    # Loop over keywords
    for keyword in gay:
        if lookup_keyword(sermon[-3], keyword):
            gay_full_NOSODOM.append(flatten([sermon, [keyword]]))
            break
        

###Split text into paragraphs     
gay_para = [[] for i in enumerate(gay_full_NOSODOM)]

for i in xrange(0,4769):
    gay_para[i] = gay_full_NOSODOM[i][-2]
    gay_para[i] = gay_para[i].split('\n\n')

    
###Save text split by paragraph

"""with open(path + '/gay_para.pkl', 'w') as pfile:
    pickle.dump(Gay_para, pfile)"""
### Open Gay_para
with open(path + '/gay_para.pkl', 'r') as f:
    gay_para=pd.read_pickle(f)
    

"""Create empty list of lists of Gay_para length"""

good = [[] for sublist in gay_para]
for i in xrange(0,7162): 
    good[i] = []

good = [[] for sublist in gay_full_NOSODOM]
for i in xrange(0,4769): 
    good[i] = []


"""Select only paragraphs about homosexuality"""
            
"""NOSODOM"""

for i in xrange(0,4769):
    for k in gay_para[i]:
        for keyword in gay:
            lookup_keyword_delete(k,keyword,good)

"""Create empty list of lists"""
    
gay_para_filtered = [[] for sublist in gay_para]
for i in xrange(0,4769): 
    gay_para_filtered[i] = []
    
"""eliminate duplicates in Gay Para"""

for i in xrange(0,4769):
    for k in good[i]:
        if k not in gay_para_filtered[i]:
            gay_para_filtered[i].append(k)
"""save filtered dataset"""
with open(path +'/gay_para_filtered.pkl', 'w') as pfile:
    pickle.dump(gay_para_filtered, pfile)

"""Open filtered dataset"""
with open(path +'/gay_para_filtered.pkl', 'r') as f:
    gay_para_filtered = pd.read_pickle(f)

"""insert HTMLS"""

with open(path + '/gay_content.pkl', 'r') as f:
    gay_content = pd.read_pickle(f)

for i in xrange(0,4769):
    gay_para_filtered[i].insert(0, gay_full_NOSODOM[i][0])


"""resave dataset"""
with open(path +'/gay_para_filtered_NOSODOM.pkl', 'w') as pfile:
    pickle.dump(gay_para_filtered, pfile)
    
##Data frame for R
gay_df = pd.DataFrame(gay_para_filtered)
gay_df.to_csv('gay_para_filtered_NOSODOM.csv')
