

# A simple procedure for classifying households when only given the ages of each member in the household
#
# First, it takes a table of individuals with a household columns and generates lists for each household with the ages of its members. e.g. [4,8,38,39,67]
#
# Then it loops over each list, returning the percent of household members in different age groups (e.g. 0-17, 18-30, etc.). Finally households are categorized into the following:
#
# 'single_person' - single person living alone
# 'one_gen' - two or more people of the same generation living together (e.g. couple, roommates)
# 'two_gen', - two generations in the same household (e.g. typical family with children)
# 'mutli_gen', - three generations in the same household (e.g. children, parents, and grandparents)
# 'other' - anything else



import csv
from collections import Counter

in_csv_name = "mini_person_table.csv"
out_csv_name = "hhld_structure.csv"

hhld_ages = []

# reading the csv and looping over the persons
with open(in_csv_name, 'r') as csvfile:
    reader = csv.DictReader(csvfile)

    hhld_num = 0

    age_list = [0]

    for row in reader:

        if row["hhld_num"] != hhld_num:

            out_row = [hhld_num, age_list]
            hhld_ages.append(out_row)

            hhld_num = row["hhld_num"]
            age_list = [int(row["age"])]

        else:

            age_list.append(int(row["age"]))

    out_row = [hhld_num, age_list]
    hhld_ages.append(out_row)


del hhld_ages[0] # remove the first 0 0 item

out_data = [["hhld_num","hhld_st_a18","hhld_st_a1830","hhld_st_a3165","hhld_st_a6675","hhld_st_76","hhld_type"]]

types = ['single_person','one_gen','two_gen','mutli_gen','other']

for row in hhld_ages:

    print(row)

    a18 = float(0)
    a1830 = float(0)
    a3165 = float(0)
    a6675 = float(0)
    a76 = float(0)
    asum = float(0)

    for a in row[1]:

        age = int(a)

        if age < 18:
            a18 += 1
            asum += 1

        elif age >= 18 and age <= 30:
            a1830 += 1
            asum += 1

        elif age > 30 and age <= 65:
            a3165 += 1
            asum += 1

        elif age > 65 and age <= 75:
            a6675 += 1
            asum += 1

        elif age > 75:
            a76 += 1
            asum += 1


    if len(row[1]) < 2:
        hhld_type = 'D_single_person'

    else:
        t = sorted(row[1])
        ts = [j-i for i, j in zip(t[:-1], t[1:])]
        print(ts)
        num_gens = (sum(x > 18 for x in ts))
        oc = 0 # sum of all differences that aren't big gen differences
        for x in ts:
            if x <= 18:
                oc += x
        print(oc)
        if oc > 20:
            hhld_type = 'E_complex'
        else:
            if num_gens == 0:
                hhld_type = 'B_one_gen'
            elif num_gens == 1:
                hhld_type = 'A_two_gen'
            elif num_gens > 1:
                hhld_type = 'C_multi_gen'

    try:

        out_row = [row[0],a18 / asum, a1830 / asum, a3165 / asum, a6675 / asum, a76 / asum, hhld_type]
        out_data.append(out_row)

    except:
        None

    print("---")


with open(out_csv_name, 'w') as csvfile:
    writer = csv.writer(csvfile)
    for row in out_data:
        writer.writerow(row)
