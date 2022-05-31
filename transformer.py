import csv

for i in range(1, 3):
    with open(f"Table {i}.csv", newline="") as csvfile:
        spamreader = csv.reader(csvfile, delimiter=" ", quotechar="|")
        for row in spamreader:
            print(", ".join(row))
