# taking DineSafe data from Toronto's Open Data Portal
# converting it from its current XML format into a CSV file

import xmltodict
import csv

with open('ds.xml') as fd:
    doc = xmltodict.parse(fd.read())

out_df = []
header = ["id","name","type","address","latitude","longitude","status","last_checked_date"]
out_df.append(header)

for e in doc["DINESAFE_DATA"]["ESTABLISHMENT"]:
    # e["NAME"]
    id = e["ID"]
    name = e["NAME"]
    type = e["TYPE"]
    address = e["ADDRESS"]
    latitude = e["LATITUDE"]
    longitude = e["LONGITUDE"]
    current_status = e["STATUS"]

    try:
        inspection = e["INSPECTION"]

        print(name)
        print(inspection)

        f = 0
        for i in inspection:
            print(i)
            if i == "DATE":
                f = 1


        if f == 1:
            date = inspection["DATE"]

        else:
            date = inspection[len(inspection) - 1]["DATE"]

        print(date)


    except:
        None

    out_df.append([id,name,type,address,latitude,longitude,current_status,date])


    print("------------------")


with open("dinesafe.csv", "w") as csvfile:
    writer = csv.writer(csvfile)
    for row in out_df:
        writer.writerow(row)
