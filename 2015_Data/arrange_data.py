# -*- coding: utf-8 -*-
'''
Created on Fri Apr 13 10:33:08 2018

@author: Andrew
'''

import os
from copy import deepcopy as dc

class sample:
    
    '''
    Args should be formatted as follows:
        date (as month.date.year, ex: vii.03.2015); method (malaise, pitfall, or sweep);
        block (as in block 2 if sample from 2A); site (as in site A if sample from 2A); init makes
        __orders__ (dictionary of orders as key, values as a list containing float order biomass
        and list of alphabetized family names).
    '''
    def __init__(self, date, method, block, site, identifier='M. Andrew Jansen'):
        self.__date__ = date
        self.__method__ = method
        self.__block__ = block
        self.__site__ = site
        self.__orders__ = {}
        self.__identifier__ = identifier
        
    def __str__(self):
        return '{self.__method__} {self.__block__}{self.__site__} {self.__date__}'.format(self=self)
        
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
    
    def add_order(self, taxon, mass, families):
        self.__orders__[taxon] = [mass, families]
    
    def identifier(self):
        return self.__identifier__

def get_files(path='./data'):
    '''
    Retrieves filenames in directory.
    '''
    filelist = os.listdir(path)
    return filelist 

def sample_data(files, master='master_2015.csv', families='non-target_2015.txt'):
    '''
    Returns a set of samples from 2015.
    '''
    possible = ['1A', '1B', '1C', '2A', '2B', '2C', '3A', '3B', '3C', 'UTC1', 'UTC2', 'UTC3']
    smpl_list = []
    
    fams = open(families, 'r')
    fam_file = []
    for part in fams:
        fam_file.append(part)
    master_list = open(master, 'r')
    
    for event in master_list:
        event = event.strip(' \t\n\r')
        blocksite, date_bad = event.split(',')
        day, month, year = date_bad.split('.')
        if len(day) == 1:
            day = '0'+day
        else:
            pass
        newdate = month+'.'+day+'.'+year
        smpl_list.append(sample(newdate, 'malaise', blocksite[:-1], blocksite[-1]))
        smpl_list.append(sample(newdate, 'pitfall', blocksite[:-1], blocksite[-1]))
        smpl_list.append(sample(newdate, 'sweep', blocksite[:-1], blocksite[-1]))
    
    for name in files:
        biomass = open('./data/' + name, 'r')
        i = 0
        coll_meth = 'not specified'
        parent = 'not specified'
        order_name = 'not specified'
        for line in biomass:
            i += 1
            line = line.strip(' \t\n\r')
            
            if i == 1:
                coll_meth = line.split(',')[3].lower()
            elif i > 2:
                taxon = line.split(',')[0]
                current = line.split(',')[1]
                date_bad2 = line.split(',')[2]
                day2, month2, year2 = date_bad2.split('-')
                
                if month2.lower() == 'jun':
                    month_roman = 'vi'
                elif month2.lower() == 'jul':
                    month_roman = 'vii'
                else:
                    print('unspecified month: '+date_bad2)
                
                if len(day2) == 1:
                    day2 = '0'+day2
                else:
                    pass
                
                date = month_roman+'.'+day2+'.20'+year2
                
                blocksites = line.split(',')[3:]
                
                if taxon != '':
                    parent = taxon
                else:
                    pass
                
                if current != '':
                    order_name = current
                else:
                    pass
            else:
                pass
            
            if parent.lower() == 'insecta':
                for num,entry in enumerate(blocksites):
                    family = []
                    location = possible[num]
                    for sample_event in smpl_list:
                        if ((coll_meth+' '+location+' '+date).lower()) == (str(sample_event).lower()):
                            if entry == '':
                                entry = 0
                            else:
                                entry = float(entry[0:6])
                            mass = entry
                            for ind, rows in enumerate(fam_file):
                                row = rows.strip(' \t\n\r')
                                if row.lower() == str(sample_event).lower():
                                    for next_row in fam_file[ind:]:
                                        next_row = next_row.strip(' \t\n\r')
                                        if order_name.lower() in next_row.lower():
                                            for fam_name in next_row.split(' ')[2:]:
                                                family.append(dc(fam_name.strip('()')))
                                            break
                                        elif next_row.strip(' \t\n\r') == '':
                                            break
                                        else:
                                            pass
                                    break
                                else:
                                    pass
                            sample_event.add_order(order_name, mass, dc(family))
                            break
                        else:
                            pass
            else:
                pass
        biomass.close()
    fams.close()
    master_list.close()
    return smpl_list

def output(smpl_list, outfile='sampling_data_2015.txt'):
    ###Create output file###
    outfile=open(outfile, 'w')
    for entry in smpl_list:
        print(entry.method(), entry.date(), entry.block(), entry.site(), entry.orders(), file=outfile, sep='|')
    outfile.close()

def main():
    samples = sample_data(get_files())
    output(samples)

main()