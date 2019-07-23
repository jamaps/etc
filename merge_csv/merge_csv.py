# merge a buch of CSV files when they all have identical structure (i.e. same columns)

import csv
import os

file_name = "name"

# change to working directory and set as workspace (if needed)
os.chdir("work_dir")
ws = os.path.dirname(os.path.realpath(__file__))
print(ws)

# loop over the CSVs, append them to one big list
big_csv = []
csv_file_names = []
for subdir, dirs, files in os.walk(ws):
    for file in files:
        if file.endswith(('.csv')): # and x == "_":
            csv_file_names.append(file)
            with open(file_name + "/" + file, "r") as csvfile:
                reader = csv.reader(csvfile)
                for row in reader:
                    big_csv.append(row)
            
print(len(csv_file_names))
print(len(big_csv))

# write it to one big file!
with open("global_1000i_10kpts_all.csv", 'w') as csvfile:
    writer = csv.writer(csvfile)
    for row in big_csv:
        writer.writerow(row)
