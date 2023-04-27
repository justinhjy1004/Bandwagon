import neo4j
import pandas as pd
from patent_neo4j.queries import direct_citation
from patent_neo4j.queries import citation_tree
from patent_neo4j.queries import patent_info
from patent_neo4j.queries import relating_coinventors
from patent_neo4j.queries import coinventors
from patent_neo4j.queries import related_inventors
from patent_neo4j.queries import inventor_tree
from patent_neo4j.queries import nber_category
from patent_neo4j.queries import geo_coordinate
from patent_neo4j.queries import inventor_profile
from patent_neo4j.queries import batch_citation_count, batch_patent_assignee, batch_patent_location, batch_patent_inventor, assignee_patents, patents_nber_year, patents_subnber_year

"""
Neo4j Connection for Patent Graph Data
Contains functions to work with the dataset
Implementation of Algorithm is done in patent_neo4j/queries.py
INITIALIZE: 
        1. uri (i.e. "neo4j://localhost:7687")
        2. user 
        3. pwd (password)
"""
class Neo4jConnection:
    def __init__(self, uri, user, pwd):
        self.uri = uri
        self.user = user
        self.pwd = pwd
        self.driver = neo4j.GraphDatabase.driver(uri, auth=(user,pwd))
        
    # Close driver
    def close(self):
        if self.driver is not None:
            self.driver.close()
    
    # Obtain Direct Descendants of node given root
    def query_direct_citation(self, root, direct_citation=direct_citation):
        with self.driver.session() as session:
            result = session.read_transaction(direct_citation, root)
        return result
    
    # Obtain citation given root, initialized with depth of 3
    def query_citation_tree(self, root, citation_tree=citation_tree, max_depth=3):
        with self.driver.session() as session:
            result = session.read_transaction(citation_tree, root, max_depth)
        return result    
    
    # Obtain Direct Information from Single Patent
    def query_patent_info(self, root, patent_info=patent_info):
        with self.driver.session() as session:
            result = session.read_transaction(patent_info, root)
        return result
    
    # Construct citation tree and relates co-inventors
    def write_relating_coinventors(self, root, relating_coinventors=relating_coinventors, max_depth=3):
        with self.driver.session() as session:
            result = session.write_transaction(relating_coinventors, root, max_depth)
        return result
    
    # Query for coinventors relationship from a citation tree
    def query_coinventors(self, root, coinventors=coinventors, max_depth=3):
        with self.driver.session() as session:
            result = session.read_transaction(coinventors, root, max_depth)
        return result
    
    def query_related_inventors(self, inventors, related_inventors=related_inventors, max_depth=3):
        with self.driver.session() as session:
            result = session.read_transaction(related_inventors, inventors, max_depth)
        return result
    
    def query_inventor_tree(self, root, inventor_tree=inventor_tree, max_depth=3):
        with self.driver.session() as session:
            result = session.read_transaction(inventor_tree, root,  max_depth)
        return result
    
    def query_nber_category(self, root, nber_category=nber_category, max_depth=3):
        with self.driver.session() as session:
            result = session.read_transaction(nber_category, root,  max_depth)
        return result
    
    def query_geo_coordinate(self, root, geo_coordinate=geo_coordinate, max_depth=3):
        with self.driver.session() as session:
            result = session.read_transaction(geo_coordinate, root,  max_depth)
        return result
    
    def query_inventor_profile(self, inventor_id, inventor_profile=inventor_profile):
        with self.driver.session() as session:
            result = session.read_transaction(inventor_profile, inventor_id)
        return result
    
    def query_batch_patent_info(self, patent_list):
        with self.driver.session() as session:
            citation_count = session.read_transaction(batch_citation_count, patent_list)
            patent_assignee = session.read_transaction(batch_patent_assignee, patent_list)
            patent_inventor = session.read_transaction(batch_patent_inventor, patent_list)
            patent_location = session.read_transaction(batch_patent_location, patent_list)
        
        return [citation_count, patent_assignee, patent_inventor, patent_location]
    
    def query_assignee_patents(self, assignee_list):
        with self.driver.session() as session:
            result = session.read_transaction(assignee_patents, assignee_list)
        return result
    
    def query_patents_nber_year(self, year):
        with self.driver.session() as session:
            result = session.read_transaction(patents_nber_year, year)
        return result
    
    def query_patents_subnber_year(self, year):
        with self.driver.session() as session:
            result = session.read_transaction(patents_subnber_year, year)
        return result
        