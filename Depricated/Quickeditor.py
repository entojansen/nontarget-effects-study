# -*- coding: utf-8 -*-
"""
Created on Sun Jan  7 20:37:27 2018

@author: Andrew
"""
def getfile():
    ###File retrieval###
    need_name=True
    while need_name:
        fns=input("Enter file name with extension(& with path if not in program directory): ")
        try:
            file=open(fns,"r")
            need_name=False
        except IOError:
            print("File name or path not valid.")
    return file

def output(file):
    ###Create output file###
    outfile=open(input("Enter name for output file: "),"w")
    for line in file:
        if ("Malaise" in line) or ("Pitfall" in line) or ("Sweep" in line):
            print(line)
            method,site,date=line.split(" ")
            day,month,year=date.split(".")
            if len(day)==1:
                newday="0"+day
            else:
                newday=day
            newline="{} {} {}.{}.{}".format(method,site,month,newday,year)
            print(newline,file=outfile,end="")
        else:
            print(line,file=outfile,end="")
    outfile.close()

def main():
   output(getfile())

main()

