import csv

path = '/Users/dbuchan/Projects/prison_analysis/'
staff_corr = path+'staff_correlations.csv'
staff_pval = path+'staff_corr_pvalues.csv'
prisoner_corr = path+'prisoner_correlations.csv'
prisoner_pval = path+'prisoner_corr_pvalues.csv'

correlation_data = []
signif_data = []
annotated_data = []


def read_table(file):
    data = []
    with open(file) as datafile:
        datareader = csv.reader(datafile, delimiter=',', quotechar='"')
        for row in datareader:
            data.append(row)
    return(data)


def annotate_data(corrs, pvals):
    pval_data = read_table(pvals)
    corr_data = read_table(corrs)
    for i in range(1, len(pval_data)):
        for j in range(1, len(pval_data[i])):
            # print(i, j, type(pval_data[i][j]))
            if 'NA' in pval_data[i][j]:
                continue
            if float(pval_data[i][j]) <= 0.05:
                corr_data[i][j] += " *"
            if float(pval_data[i][j]) <= 0.01:
                corr_data[i][j] += "*"
            if float(pval_data[i][j]) <= 0.001:
                corr_data[i][j] += "*"
    return(corr_data)


def print_annotated(data, file):
    outputfh = open(file, "w")
    output_string = ''
    for row in data:
        output_string = ','.join(row)
        outputfh.write(output_string+"\n")
    outputfh.close()


staff_annotated = annotate_data(staff_corr, staff_pval)
prisoner_annotated = annotate_data(prisoner_corr, prisoner_pval)
print_annotated(staff_annotated, "staff_annotated.csv")
print_annotated(prisoner_annotated, "prisoner_annotated.csv")
