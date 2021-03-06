#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 14 14:00:58 2018

@author: flashcock
"""
import numpy
from copy import deepcopy as dc


class sample:
    """Custom class for sample events to organize data.

    Keyword Arguments:
        date (as month.date.year, ex: vii.03.2015)
        method (malaise, pitfall or sweep)
        block (as in block 2 if sample from 2A)
        site (as in site A if sample from 2A)

    init makes __orders__ (dictionary of orders as key, values as a list
    containing float order biomass and list of alphabetized family names).
    """

    def __init__(self, method, date, block, site):
        self.__method__ = method
        self.__date__ = date
        self.__block__ = block
        self.__site__ = site
        self.__orders__ = {}
        self.__identifier__ = 'M. Andrew Jansen'

    def __str__(self):
        return '{} {}{} {}'.format(self.__method__,
                                   self.__block__,
                                   self.__site__,
                                   self.__date__)

    def date(self):
        return self.__date__

    def method(self):
        return self.__method__

    def block(self):
        return self.__block__

    def site(self):
        return self.__site__

    def orders(self, key='na'):
        if key == 'na':
            return self.__orders__
        else:
            return self.__orders__[key]

    def add_order(self, taxon, mass, families):
        self.__orders__[taxon] = [mass, families]

    def import_orders(self, literal_dict):
        self.__orders__ = eval(literal_dict)

    def identifier(self):
        return self.__identifier__


def read_data(datafile='sampling_data_2015.txt'):
    """Imports data from an ordered txt file and creates a list of samples."""
    sample_list = []
    with open(datafile, 'r') as file:
        for line in file:
            method, date, block, site, orders = line.split('|')
            new_sample = sample(method, date, block, site)
            new_sample.import_orders(orders)
            sample_list.append(new_sample)
    return sample_list


def corrections(sample_list):
    """Applies corrections to misidentifications and removes immatures."""
    for event in sample_list:
        try:
            event.orders('Neuroptera')[0] += event.orders('Trichoptera')[0]
            event.orders('Trichoptera')[0] = 0
            del(event.orders()['Immatures (larvae)'])
        except KeyError:
            del(event.orders()['Immatures (larvae)'])
    return sample_list


def output(sample_list, outfile='sampling_data_2015_corrected.txt'):
    """Creates corrected output file."""
    with open(outfile, 'w') as outfile:
        for entry in sample_list:
            print(entry.method(),
                  entry.date(),
                  entry.block(),
                  entry.site(),
                  entry.orders(),
                  file=outfile, sep='|')


def tables(sample_list):
    """Generates tables of averages for bulk sample mass and diversity."""
    bulk_data = {}
    malaise_data = {}
    pitfall_data = {}
    sweep_data = {}

    schedule_2015 = {'1': ['vi.24', 'vi.25', 'vi.26', 'vi.27'],
                     '2': ['vii.01', 'vii.03', 'vii.06', 'vii.04'],
                     '3': ['vii.08', 'vii.10', 'vii.13', 'vii.11'],
                     '4': ['vii.15', 'vii.17', 'vii.20', 'vii.18'],
                     '5': ['vii.22', 'vii.24', 'vii.27', 'vii.25']}

    schedule_2016 = {'1': ['vi.22', 'vi.23', 'vi.24', 'vi.25'],
                     '2': ['vi.29', 'vi.30', 'vii.01', 'vii.02'],
                     '3': ['vii.06', 'vii.07', 'vii.08', 'vii.09'],
                     '4': ['vii.13', 'vii.14', 'vii.15', 'vii.16'],
                     '5': ['vii.20', 'vii.23', 'vii.22', 'vii.23']}  # 23 twice

    for event in sample_list:
        b = event.block()
        d = event.date()
        week = 'na'
        mass = 0
        families = 0
        year = 'na'

        if '2015' in event.date():
            schedule = schedule_2015
            year = '2015'
        else:
            schedule = schedule_2016
            year = '2016'

        for val in event.orders().values():
            mass += val[0]
            families += len(val[1])

        for key, val in schedule.items():
            if d[:-5] in val:
                week = key
            else:
                pass

        if 'malaise' in str(event).lower():
            malaise = malaise_data.get((year + '|' + b + '|' + week),
                                       [0, 0, []])
            malaise[2].append(event.site())
            malaise[0] = (malaise[0]
                          * (len(malaise[2]) - 1)
                          + mass) / len(malaise[2])
            malaise[1] = (malaise[1]
                          * (len(malaise[2]) - 1)
                          + families) / len(malaise[2])
            malaise_data[(year + '|' + b + '|' + week)] = malaise
        elif 'pitfall' in str(event).lower():
            pitfall = pitfall_data.get((year + '|' + b + '|' + week),
                                       [0, 0, []])
            pitfall[2].append(event.site())
            pitfall[0] = (pitfall[0]
                          * (len(pitfall[2]) - 1)
                          + mass) / len(pitfall[2])
            pitfall[1] = (pitfall[1]
                          * (len(pitfall[2]) - 1)
                          + families) / len(pitfall[2])
            pitfall_data[(year + '|' + b + '|' + week)] = pitfall
        elif 'sweep' in str(event).lower():
            sweep = sweep_data.get((year + '|' + b + '|' + week),
                                   [0, 0, []])
            sweep[2].append(event.site())
            sweep[0] = (sweep[0]
                        * (len(sweep[2]) - 1)
                        + mass) / len(sweep[2])
            sweep[1] = (sweep[1]
                        * (len(sweep[2]) - 1)
                        + families) / len(sweep[2])
            sweep_data[(year + '|' + b + '|' + week)] = sweep
        else:
            pass

    for key in malaise_data.keys():
        bulk_data[key] = [malaise_data[key][0]
                          + pitfall_data[key][0]
                          + sweep_data[key][0],
                          malaise_data[key][1]
                          + pitfall_data[key][1]
                          + sweep_data[key][1]]

    for key in sorted(malaise_data.keys()):
        print(key, malaise_data[key])


def dataframer(sample_list, output=False, outfile='raw_data.txt'):
    """Generates tables of averages for bulk sample mass and diversity."""
    data_out = []
    data_headers = ['method', 'year', 'month', 'day', 'block', 'site',
                    'event', 'order', 'mass', 'family_count', ['family_list']]
    data_out.append(data_headers)

    schedule_2015 = {'1': ['vi.24', 'vi.25', 'vi.26', 'vi.27'],
                     '2': ['vii.01', 'vii.03', 'vii.06', 'vii.04'],
                     '3': ['vii.08', 'vii.10', 'vii.13', 'vii.11'],
                     '4': ['vii.15', 'vii.17', 'vii.20', 'vii.18'],
                     '5': ['vii.22', 'vii.24', 'vii.27', 'vii.25']}

    schedule_2016 = {'1': ['vi.22', 'vi.23', 'vi.24', 'vi.25'],
                     '2': ['vi.29', 'vi.30', 'vii.01', 'vii.02'],
                     '3': ['vii.06', 'vii.07', 'vii.08', 'vii.09'],
                     '4': ['vii.13', 'vii.14', 'vii.15', 'vii.16'],
                     '5': ['vii.20', 'vii.23', 'vii.22', 'vii.23']}  # 23 twice

    for event in sample_list:

        if '2015' in event.date():
            schedule = schedule_2015
        else:
            schedule = schedule_2016

        month, day, year = event.date().split('.')

        for key, val in schedule.items():
            if event.date()[:-5] in val:
                week = key
            else:
                pass

        for key, val in event.orders().items():
            new_row = [event.method(), year, month, day, event.block(),
                       event.site(), week, key, val[0], len(val[1]), val[1]]
            data_out.append(new_row)

    if output:
        with open(outfile, 'w') as outfile:
            for item in data_out[0][:-1]:
                print(item, end=';', file=outfile)
            print(data_out[0][-1][0], file=outfile)
            for row in sorted(data_out[1:]):
                for item in row[:-1]:
                    print(item, end=';', file=outfile)

                if len(row[-1]) == 0:
                    print('NA', file=outfile)
                elif len(row[-1]) == 1:
                    print(row[-1][0], file=outfile)
                else:
                    for family in row[-1][:-1]:
                        print(family, end='', file=outfile)
                    print(row[-1][-1], file=outfile)
    else:
        for item in data_out[0][:-1]:
            print(item, end=';')
        print(data_out[0][-1][0])
        for row in sorted(data_out[1:]):
            for item in row[:-1]:
                print(item, end=';')

            if len(row[-1]) == 0:
                print('NA')
            elif len(row[-1]) == 1:
                print(row[-1][0])
            else:
                for family in row[-1][:-1]:
                    print(family, end='')
                print(row[-1][-1])


def main():
    samples = corrections(read_data())
    tables(samples)
    dataframer(samples, output=True)


main()

# second arrange data into a neat looking table
# third statistics, look at:
#   overall correspondence b/w sample weight and diversity, by type
#   correspondence b/w sample weight and diversity but within each order
#   comparisons between blocks for overall diversity and sample weight
#   comparisons between blocks for weight of individual pollinator orders
#   comparisons between blocks for diversity of all pollinator families
#   the same, but segregate pollinator families by order
#   maybe include a figure about sampling methods???
#   maybe do each of these for bulk and individual sampling methods???
