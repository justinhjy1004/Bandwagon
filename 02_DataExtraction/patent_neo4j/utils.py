import pandas as pd
import os
import json

"""
Takes a dataframe that contains a list that traces lineage and converts to
direct descendent alongside with similarity
Inputs: 
    df - pandas dataframe containing the data containing columns in cols
    cols - columns to apply function on
"""
def convert_lineage(df, cols = ["lineage", "similarity"]):
    for col in cols:
        df[col] = df[col].apply(lambda x: x[0])
        
    return df

"""
Tracks the 'generation' as defined by the length of lineage
This means that generations can overlap
Inputs:
    df - pandas dataframe that has the column lineage in list form
"""
def trace_generation(df):
    df["generation"] = df["lineage"].apply(lambda x: len(x))
    return df

"""
Takes coinventor edge list and convert to integer mappings
Saves file such that it is applicable to node2vec implementation
Inputs:
    coinventors - pd.Df that contains coinventor edgelist
    
Output:
    coinventors - mapped EdgeList to integer
    coinventor_map - mapping for inventor id to integers
    
Optional:
    Set write == True, then writes a space separated file that is compatible
    Also, with a json file that can be natively loaded into a Python Dict for the 
    mapping
"""
def inventor_to_int(coinventors, write=False, file="file", mapping="mapping"):
    # Combines two rows of the edge list
    coinventor_list = coinventors['coinventor1'].tolist() + coinventors['coinventor2'].tolist()
    # Remove Duplicates
    coinventor_list = set(coinventor_list)
    # Converts to list for indexing
    coinventor_list = list(coinventor_list)
    
    # Create dict that maps to int
    coinventor_map = {}
    for i in range(0,len(coinventor_list)):
        coinventor_map[coinventor_list[i]] = i
    
    # Maps id to int
    for index,row in coinventors.iterrows():
        row.coinventor1 = coinventor_map[row.coinventor1]
        row.coinventor2 = coinventor_map[row.coinventor2]  
    
    # OPTIONAL: writes to compatible format
    if write:
        coinventors.to_csv(file, header=False, index=False, sep=' ')
        with open(mapping, 'w+') as fp:
            json.dump(coinventor_map, fp, sort_keys=True, indent=4)
    
    output = (coinventors, coinventor_map)
        
    return output

"""
Takes Janson-Shannon Divergence dictionary and averages the the values
for both main and subcategories

Returns a pandas dataframe

Input:
    Janson-Shannon Divergence Dictionaey
Output:
    dataframe
"""
def js_to_pd(js):
    data = {'gen': [1,2,3], 'nber': [0,0,0]}
    pd_js = pd.DataFrame.from_dict(data)
    
    for i in js:
        for key,pair in i.items():
            pd_js.loc[pd_js["gen"] == sum(key),'nber'] = pd_js.loc[pd_js["gen"] == sum(key),'nber'] + pair/2
            
    return pd_js

'''
Given (RAW) citation_tree, keep only the "oldest" generation
i.e. if a is gen 1 and 2, gen will be only 2
Also, take the direct simiarity
Input:
    citation_tree
Output:
    citation_tree
'''
def get_max_generation(citation_tree):
    # Obtain the generation based on lineage
    citation_tree['gen'] = citation_tree['similarity'].apply(lambda x: len(x))
    
    # Dropping duplicates due to different inventors
    generation = citation_tree.loc[:,['id','gen']].drop_duplicates()
    
    # Sort values based on a generation, keeping the last (the idea of the forefront of inventions)
    generation = generation.sort_values(by=['gen']).drop_duplicates(subset=['id'], keep='last')
    
    # Left join with generation -> this only keeps the max(gen) for each patent
    citation_tree = pd.merge(generation,citation_tree,on=['id','gen'], how='left')
    
    # Take the direct similarity of the max(gen)
    citation_tree['similarity'] = citation_tree['similarity'].apply(lambda x: x[0])
    
    return citation_tree

