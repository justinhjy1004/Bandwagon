"""
Author: Justin Ho
Date: July 08 2021
Contains routine functions for analysis of patent data
"""

import numpy as np
import pandas as pd
import itertools
from scipy import spatial
from scipy.spatial.distance import jensenshannon
from geopy.distance import great_circle

# NBER categories
nber_categories = pd.read_csv("Data/nber_subcategory.csv")
nber_categories = nber_categories['id'].to_numpy()
NUM_MAIN_NBER = 7

# Suppress SettingWithCopyWarning because it is annoying and I don't know how to fix it!!!
# If you do, please help!
pd.set_option('mode.chained_assignment', None)


"""
Takes a path to the file (in the format of the node2vec output)
and returns a np matrix
Input:
    path_to_fil - string of path to file
Output:
    np_data - nxd matrix with n inventors and d dimension of vector representation
"""
def convert_np(path_to_file):
    # Reading file that is compatible with node2vec output
    df = pd.read_csv(path_to_file, 
                     delimiter=" ", 
                     skiprows=1, 
                     header=None)
    
    # Sort values by the first column (int representation of the nodes)
    # This is so that the returned data is indexed
    df = df.sort_values(df.columns[0])
    
    # Drop index column
    df = df.drop(df.columns[0], axis=1)
    np_data = df.to_numpy()
    
    return np_data

"""
Given inventors 1 and 2, calculate the dot product of the inventors
Requires the coinventor_mapping dictionary and node embeddings
Return None if there are no inventors
Input: 
    coinventor_mapping - dictionary that maps inventor id to int
    node_emb - node2vec embeddings in np.ndarray represetation
    inventor1,2 - inventor id 
    measure - default with cosine
Ouput:
    sim - similarity score based on dot product
"""
def similarity_score(coinventor_mapping, node_emb, inventor1, inventor2, measure="cos"):
    # Check if both inventors are part of the dictionary
    keys = list(coinventor_mapping.keys())
    if inventor1 not in keys or inventor2 not in keys:
        if inventor1 == inventor2:
            return 1
        else:
            return None
    
    # Obtain index of inventor
    emb1 = coinventor_mapping[inventor1]
    emb2 = coinventor_mapping[inventor2]
    
    # Dot product
    if measure == "cos":
        sim = 1 - spatial.distance.cosine(node_emb[emb1],node_emb[emb2])
    else:
        sim = node_emb[emb1].dot(node_emb[emb2])
    return sim

'''
Given inventors hops relations, returns the shortest distance required to
reach given nodes
Input: 
    inventor - starting node
    related_inventors - dataframe containing hop relationships with inventor
Outputs:
    shortest_path - dataframe relating the number of hops
'''
def related_inventors_sp(inventor,related_inventors):
    # transform to set to remove duplicate hops and count the length
    related_inventors["hops"] = related_inventors["hops"].apply(lambda x: len(set(x)))
    
    # init empty dataframe
    shortest_path = pd.DataFrame(columns = ['inventor', 'related', 'hops'])
    
    # for every inventor, get shortest path
    for i in related_inventors.related.unique():
        min_hops = 0 # self similarity
        if inventor != i: # exclude self
            min_hops = related_inventors[related_inventors.related == i]['hops'].min()
        
        new_row = {'inventor': inventor, 'related': i, 'hops': min_hops}
        shortest_path = shortest_path.append(new_row, ignore_index=True)
            
    return shortest_path

'''
Given citation tree, returns direct ancestor relationship
Input:
    citation_tree
Output:
    direct_ancestor - with similarity scores
'''
def get_direct_ancestor(citation_tree):
    # takes first element in list
    citation_tree['hops'] = citation_tree['lineage'].apply(lambda x: len(x))
    citation_tree['lineage'] = citation_tree['lineage'].apply(lambda x: x[0])
    citation_tree['similarity'] = citation_tree['similarity'].apply(lambda x: x[0])
    
    direct_ancestor = citation_tree[["id", "lineage", "similarity", "hops"]]
    
    return direct_ancestor

'''
Given direct ancestor and list of patent inventors, generate pairwise combination
of inventors between the two patents
Input:
    direct_ancestor
    inventor_tree
Output:
    direct_ancestor - with combination
'''
def interpatent_inventor_combination(direct_ancestor, inventor_tree):
    combination = []
    
    # Generate combination of inventors
    for index,row in direct_ancestor.iterrows():
        citing = inventor_tree[inventor_tree['patent'] == row.id]['inventor'].tolist()[0]
        cited = inventor_tree[inventor_tree['patent'] == row.lineage]['inventor'].tolist()[0]
        combination.append([x for x in itertools.combinations(citing + cited, 2)])
        
    direct_ancestor = direct_ancestor.assign(combination = combination)
    
    return direct_ancestor

'''
Given NBER category dataframe, fills in missing values based on majority vote of direct
ancestor
Input:
    nber - dataframe containing nber information
Output:
    counts - dataframe with assigned nber
'''

def assign_missing_nber(nber,max_depth=3):
    # Locate Missing NBER assignments
    nones = nber.loc[nber['nber'].isna(),:]

    # Take non-missing NBER assignments
    nber2 = nber.loc[nber['nber'].notnull(),:]
    # Caluclate hops based on lineage
    nber2.loc[:,'hops'] = nber2.loc[:,'nber_lineage'].apply(lambda x: len(x))
    # Subset relevant columns
    nber2 = nber2.loc[:, ('id','nber','hops')]
    # Drop duplicates
    nber2 = nber2.drop_duplicates(subset=['id'])

    # If missing Assignments Exist
    if nones.shape[0] != 0:
        # Calculate hops
        nones['hops'] = nones.loc[:,'nber_lineage'].apply(lambda x: len(x))

        # Obtain NBER from direct ancestors 
        nones.loc[:,'nber'] = nones.loc[:,'nber_lineage'].apply(lambda x: x[0])
        for i in range(1,max_depth): # if remains None, repeat until root (which should have NBER assignment)
            nones.loc[nones['nber'].isna(),'nber'] = nones.loc[nones['nber'].isna(),'nber_lineage'].apply(lambda x:x[i])

        # Majority Voting Mechanism - count which is the most common
        counts = nones.groupby(['id', 'nber', 'hops']).size().reset_index(name='counts')
        # Sort counts
        counts = counts.sort_values('counts')
        # Keep Last (Largest Count/Vote)
        counts = counts.drop_duplicates(['id'], keep = 'last')
        # Subset Relevant Columns
        counts = counts[['id','nber','hops']]
        # Drop duplicates
        counts = counts.drop_duplicates(subset=['id'])
        # Concatenate with nber2
        nber2 = pd.concat([nber2,counts])

    return nber2

'''
Calculate Discrete Probability Distributions of NBER subcategories
Input:
    nber - dataframe with NBER data
Output:
    distribution - tuple with matrix representation of distribution of
                   main and sub categories for NBER
'''
def nber_distribution(nber):
    # Count sub category NBER distribution
    nber_sub_dist = nber.groupby(['nber', 'hops']).size().reset_index(name='counts')
    
    # Count main category NBER distribution
    nber_main = nber.copy()
    nber_main['main_nber'] = nber_main["nber"].apply(lambda x: int(int(x)/10))
    nber_main = nber_main.groupby(['main_nber','hops']).size().reset_index(name='counts')
    
    sub_distribution = []
    
    # Calculate distribution based on hops
    for i in range(nber_sub_dist['hops'].max()):
        sub_distribution.append([])
        
        # Select hop for NBER distribution
        hop = nber_sub_dist[nber_sub_dist['hops'] == i + 1]
        hop_count = hop['counts'].sum()
        
        # Assign probability of each NBER subcategory
        for j in range(len(nber_categories)):
            nber_count = hop.loc[hop['nber'] == str(nber_categories[j]), 'counts']
            if len(nber_count) != 0:
                sub_distribution[i].append(float(nber_count)/hop_count)
            else:
                sub_distribution[i].append(0)
                
    main_dist = []

    for i in range(nber_main['hops'].max()):
        main_dist.append([])

        hop = nber_main.loc[nber_main['hops'] == i+1]
        hop_count = hop['counts'].sum()

        for j in range(NUM_MAIN_NBER):
            nber_count = hop.loc[hop['main_nber'] == j+1, 'counts']
            if len(nber_count) != 0:
                main_dist[i].append(float(nber_count/hop_count))
            else:
                main_dist[i].append(0)
                
    return (main_dist, sub_distribution)

'''
Calculate Janson-Shannon Divergence between combinations
Input:
    distribution - discrete probability distribution matrix of hops
Ouput
    js_div - map values for Janson-Shannon Divergence
'''
def js_divergence(distribution):
    combination = [x for x in range(len(distribution[0]))]
    combination = itertools.combinations(combination, 2)
    
    main_div_map = {}
    # Calculate Janson-Shannon Divergence for hops
    for c in combination:
        main_div_map[c] = jensenshannon(distribution[0][c[0]],distribution[0][c[1]])
    
    combination = [x for x in range(len(distribution[1]))]
    combination = itertools.combinations(combination, 2)
    
    sub_div_map = {}
    # Calculate Janson-Shannon Divergence for hops
    for c in combination:
        sub_div_map[c] = jensenshannon(distribution[1][c[0]],distribution[1][c[1]])
        
    return (main_div_map, sub_div_map)

'''
Given Dataframe of citation tree with location information
calculate the distance 
Input:
    geo -  dataframe containing lat,long information
Output:
    geo - adding distance from direct ancestor
'''
def geo_distance(geo):
    # Convert lat,long into numeric and drop NA
    geo['latitude'] = pd.to_numeric(geo['latitude'])
    geo['longitude'] = pd.to_numeric(geo['longitude'])
    geo = geo.dropna()
    
    # Get direct ancestor
    geo['lineage'] = geo['lineage'].apply(lambda x: x[0])
    
    # Obtain unique location for merge
    geo_unq = geo.copy()
    geo_unq = geo_unq[['id','latitude','longitude']]
    geo_unq = geo_unq.drop_duplicates()
    
    # Merge to direct ancestor
    geo_unq = geo_unq.rename(columns={"id":"lineage", "latitude":"lineage_lat", "longitude":"lineage_long"})
    geo = geo.merge(geo_unq, on = 'lineage', how = 'left')
    geo = geo.dropna()
    
    # Calculate distance
    geo.loc[:,'distance'] = geo.loc[:,['latitude', 'longitude','lineage_lat','lineage_long']].apply(
                        lambda x: great_circle((x[0],x[1]),(x[2],x[3])).miles, axis=1)
    
    return geo

'''
For na NBER patents, assign based on majority vote of parents
Input:
    citation_tree
Output:
    citation_tree
'''
def fixing_na_nber(citation_tree):
    
    # Get NBER, similarity and lineage
    citation_tree['nber'] = citation_tree['nber_lineage'].apply(lambda x: x[0])
    
    # Return Assigned NBER
    citation_tree = pd.merge(citation_tree.drop(['nber'],axis=1),assign_missing_nber(citation_tree), on='id', how='left')
    
    return citation_tree