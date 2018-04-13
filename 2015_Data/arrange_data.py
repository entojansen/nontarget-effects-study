# -*- coding: utf-8 -*-
'''
Created on Fri Apr 13 10:33:08 2018

@author: Andrew
'''

import os

class sample:
    
    '''
    Args should be formatted as follows:
        date (as month.date.year, ex: vii.03.2015); method (malaise, pitfall, or sweep);
        block (as in block 2 if sample from 2A); site (as in site A if sample from 2A);
        orders (dictionary of orders as key, values as a tuple containing float order biomass
        and list of alphabetized family names).
    '''
    def __init__(self, date, method, block, site, orders={}, identifier='M. Andrew Jansen'):
        self.__date__ = date
        self.__method__ = method
        self.__block__ = block
        self.__site__ = site
        self.__orders__ = orders
        self.__identifier__ = identifier
        
    def date(self):
        return self.__date__
    
    def method(self):
        return self.__method__
    
    def block(self):
        return self.__block__
    
    def site(self):
        return self.__site__
    
    def orders(self):
        return self.__orders__
    
    def add_order(self, name, mass, families):
        self.__orders__[name] = (mass, families)
    
    def identifier(self):
        return self.__identifier__

def get_files(path='.\data'):
    '''
    Retrieves filenames in directory.
    '''
    filelist = os.listdir(path)
    return filelist 

def sample_data(files, master = 'master_2015.csv', families='non-target_2015.txt'):
    '''
    Returns a set of samples from 2015.
    '''
    smpl_set = set()
    
    fam_file = open(families, 'r')
    master_list = open(master, 'r')
    #create samples (of each sample type) and add them to smpl_set using this list, add data later via add_order method
    
    for name in files:
        biomass = open('.\data\\' + name, 'r')
        i = 0
        method = 'none'
        parent = 'none'
        for line in biomass:
            i += 1
            if i == 1:
                method = line.split(',')[3].lower()
            elif i > 2:
                taxon, order, date, A1, B1, C1, A2, B2, C2, A3, B3, C3, UTC1, UTC2, UTC3 = line.split(',')
                
            else:
                pass
        biomass.close()
    fam_file.close()
    return smpl_set

def main():
    print('Hello world.')

main()