# parses out just Dissemination Area rows from a greater table

import csv

out = []
with open("dadata.csv", "r") as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        print len(row[1])
        c = 0
        if len(row[1]) > 7:
            out.append(row)
            print row
        c += 1
        if c > 3:
            break


# for row in out:
#     print row

with open("just_da_data.csv", 'w') as csvfile:
    writer = csv.writer(csvfile)
    for row in out:
        writer.writerow(row)
