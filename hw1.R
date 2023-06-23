

# Turns one string vector into one binary vector, with a 1 for FIN, and 0 for the rest
fin <- as.numeric(population=="FIN")


# Turns one string vector into a data frame, with each binary vector within representing one of the ethnicities

eth_list <- unique(population)
geno_pop <- list()

for(str in eth_list){
  eth_list <- as.integer(population == str)
  geno_pop[[str]] <- eth_list
  
}

# Identical to the above, but uses vectorization instead of a for loop
eth_list <- unique(population)
geno_pop <- lapply(eth_list, function(str) as.integer(population == str))
names(geno_pop) <- eth_list

# Convert geno_pop to a matrix
# Assuming geno_pop is a data frame with binary vectors
geno_pop <- as.matrix(geno_pop)


# For each row, calculate if the SD is 0, and remove it if so
# Although it is quite slow, it can be stopped without crashing R, and will keep the rows it calculated
geno_trim <- data.frame()
for(j in 1:nrow(geno)){
  if(sd(geno[j,]) != 0){
    geno_trim <- rbind(geno_trim, geno[j,])
  }
}

# Same as above, but uses vectorization instead of a for loop
# It is significantly faster
std_devs <- apply(geno, 1, sd)
geno_trim <- geno[std_devs != 0, ]

# Showcase the fact that ones now appear more often
geno_trim[1:20,1:20]




