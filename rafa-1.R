class(geno)
str(geno)

dim(geno)
geno[,100]
table(geno[,100])
table(geno[,200])
range(geno[,sample(503,10)])

#table(as.vector(geno)) #crashes R
range(geno)


class(population)
str(population)
population[10]
table(population)

cor(geno[1,],geno[2,])
# For the 1000GP
# Load the data from Gdrive in one line
# find meaning of the population labels
# find all the rows that have no predictive power, and remove them
# write a program to correlate SNPs and the ethnicity "FIN"; cor(y_0 x[i,])
# first use for loops, then try vectorization to compute

