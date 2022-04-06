# converts the .csv data for Census Tracts
# from http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page_Download-Telecharger.cfm?Lang=E&Tab=1&Geo1=PR&Code1=01&Geo2=PR&Code2=01&Data=Count&SearchText=Canada&SearchType=Begins&SearchPR=01&B1=All&TABID=1
# so it can be joined with boundaries in GIS
# with all the variables available for mapping 

import csv
import pandas as pd

# grab the list of variables to include
variables = []
with open("var_keys.csv", 'r') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        vname = str(row[0]) + ' - ' + row[1]
        variables.append([row[0],row[1],row[2],vname])

print variables

# df of just cts, for future joining
ctuid_list = []
# with open("trim_data.csv", 'r') as csvfile:
with open("98-401-X2016003_eng_CSV/98-401-X2016003_English_CSV_data.csv", 'r') as csvfile:
    reader = csv.DictReader(csvfile)
    c = 0

    for row in reader:
        ctuid = row['GEO_CODE (POR)']
        if ctuid not in ctuid_list:
            ctuid_list.append(ctuid)

print len(ctuid_list)
ctuid_col = []
cid = 0
for ct in ctuid_list:
    ctuid_col.append((ct,cid))
    cid += 1

df = pd.DataFrame.from_records(ctuid_col, columns=["ctuid","cid"])

# print df


c = 0

for v in variables:

    ct_v = []

    with open("98-401-X2016003_eng_CSV/98-401-X2016003_English_CSV_data.csv", 'r') as csvfile:
        reader = csv.DictReader(csvfile)


        for row in reader:

            ctuid = row['GEO_CODE (POR)']

            if row['DIM: Profile of Census Tracts (57)'] == v[1]:
                vval = row['Dim: Sex (3): Member ID: [1]: Total - Sex']

                if row['Member ID: Profile of Census Tracts (57)'] == v[0]:

                    ct_v.append((ctuid,vval))

    dfv = pd.DataFrame.from_records(ct_v, columns=["ctuid",v[3]])
    print dfv

    # print dfv

    if c == 0:
        dft = df.set_index("ctuid").join(dfv.set_index("ctuid"))
    else:
        dft = df.join(dfv.set_index("ctuid"))

    df = dft

    c += 1

    # if c > 3:
    #     break


print df
df.to_csv("test.csv")
