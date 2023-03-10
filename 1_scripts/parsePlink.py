#import pandas
#time_dat = pandas.read_excel("~/Dev/school/BINP29/popgen/0_data/uncompressed/DataS1.xlsx")

def readPlink(ped_file, map_file):
    SNP_list = []
    output_line = ""
    try:
        with open(ped_file, 'r') as ped, open(map_file, 'r') as map, open("output.tsv", 'w') as output_file:
            for line in map:
                SNP_list.append(line.split("\t")[1])
            output_line = "SampleID\tGroup\tSex\tPhenotype\t" + "\t".join(str(x) for x in SNP_list) + "\n"
            output_file.write(output_line)
            for line in ped:
                line = line.strip().split(" ")
                output_line += "{}\t{}\t{}\t{}\t{}\n".format(line[1], line[0], line[4], line[5], "\t".join(str(x) for x in (line[6:])))
                output_file.write(output_line)
    except Exception as e:
        print(e)


ped_file = "/Users/robinhan/Dev/school/BINP29/popgen/0_data/uncompressed/DataS1.ped"
map_file = "/Users/robinhan/Dev/school/BINP29/popgen/0_data/uncompressed/DataS1.map"
readPlink(ped_file, map_file)

