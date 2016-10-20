#!/usr/bin/env python

import csv as csv
import numpy as np

#analyse male and female survivors

csv_file = csv.reader(open('train.csv','rb'))
header = csv_file.next()

print header

data=[]
for row in csv_file:
    data.append(row)

data = np.array(data)


print data

survived=(data[0::,1].astype(np.float))

number_passengers=np.size(survived)
number_survived=np.sum(survived)

proportion_survivors = number_survived/number_passengers

print "Proportion of Survivors",proportion_survivors

women_only_stats = data[0::,4] == "female"
men_only_stats = data[0::,4] != "female"

women_onboard = data[women_only_stats,1].astype(np.float)
men_onboard = data[men_only_stats,1].astype(np.float)

np.sum(women_onboard)/np.size(women_onboard) #74%
np.sum(men_onboard)/np.size(men_onboard)     #19%

# creating gender model

test_file=csv.reader(open('test.csv','rb'))
header=test_file.next()

filetoclose=open('genderbasedmodel.csv',"wb")
prediction_file=csv.writer(filetoclose)

prediction_file.writerow(["PassengerID","Survived"])

for row in test_file:
    if row[3]=='female':
        prediction_file.writerow([row[0],'1'])
    else:
        prediction_file.writerow([row[0],'0'])

