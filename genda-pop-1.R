# Vary the ethnicities picked

# Create a random sample of ethnicities
unipop <- unique(pop_sample)
unipop_sample <- sample(unipop, sample(3:5, 1))
#eth_to_plots(unipop_sample,n_snp = 600) #Test the generalized function


# Vary the number of SNPs used

# Creating the vector
snpnum <- seq(1000, 100, by = -100)

# Evaluating the function at each level of snpnum
results <- lapply(snpnum, function(x) eth_to_plots(unipop_sample, n_snp = x))

