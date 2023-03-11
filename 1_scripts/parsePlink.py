import pandas

def readExcel(excel_file, sample_list):
    time_dat = pandas.read_excel(excel_file)
    time_dat = time_dat.drop_duplicates(subset=["MasterID"])
    time_dat = time_dat.drop(columns="Index")
    sample_list = [x[0] for x in sample_list]
    criteria = time_dat["MasterID"].map(lambda x: x in sample_list)
    time_dat = time_dat[criteria]
    time_dat.replace("..", "NA")
    time_dat.to_csv('Sample_Metadata.tsv', sep='\t', index=False)

def readPlink(ped_file, map_file):
    SNP_list = []
    SNP_index = {}
    sample_list = []
    output_line = ""
    try:
        with open(gcount_file, 'r') as gcount, \
            open(map_file, 'r') as map, \
            open(ped_file, 'r') as ped:

            for index, line in enumerate(map):
                SNP_list.append([line.split("\t")[1], line.split("\t")[0]])
                SNP_index[line.split("\t")[1]] = index
                #SNP_list[line.split("\t")[1]] = [index+1, line.split("\t")[0]]

            gcount.readline()
            for line in gcount:
                SNP_list[SNP_index[line.split("\t")[1]]].extend([line.split("\t")[2], line.split("\t")[3]])
                #SNP_list[line.split("\t")[1]].extend([line.split("\t")[2], line.split("\t")[3]])

            for line in ped:
                line = line.strip()
                sample_list.append([line.split(" ")[1], *line.split(" ")[6:]])

        with open("samples.tsv", 'w') as sample_file, open("SNP_list.tsv", 'w') as SNP_file:
            # Make header
            output_line = "SNP_ID\t"
            for sample in sample_list:
                output_line += sample[0] + "\t"
            output_line.removesuffix("\t")
            output_line += "\n"
            sample_file.write(output_line)

            # Write SNP allele data
            for index, snp in enumerate(SNP_list):
                output_line = snp[0] + "\t"
                for snp_dat in sample_list:
                    output_line += snp_dat[index+1] + "\t"
                output_line.removesuffix("\t")
                output_line += "\n"
                sample_file.write(output_line)

            # Write SNP metadata
            SNP_file.write("SNP_ID\tCHR\tREF\tALT\n")
            for snp in SNP_list:
                SNP_file.write("\t".join(snp)+"\n")
    except Exception as e:
        print(e)

    return sample_list

ped_file = "/Users/robinhan/Dev/school/BINP29/popgen/0_data/uncompressed/DataS1.ped"
map_file = "/Users/robinhan/Dev/school/BINP29/popgen/0_data/uncompressed/DataS1.map"
gcount_file = "/Users/robinhan/Dev/school/BINP29/popgen/0_data/uncompressed/plink2.gcount"
excel_file = "~/Dev/school/BINP29/popgen/0_data/uncompressed/DataS1.xlsx"

sample_list = readPlink(ped_file, map_file)
readExcel(excel_file, sample_list)
