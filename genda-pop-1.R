# Vary the ethnicities picked
unipop_sample <- c("CDX", "GBR", "YRI")
unipop_sample <- c("PUR","CLM","IBS")

# Create a random sample of ethnicities
unipop <- unique(pop_sample)
unipop_sample <- sample(unipop, sample(6, 1))
#eth_to_plots(unipop_sample,n_snp = 600) #Test the generalized function

# Vary the number of SNPs used

# Creating the vector
snpnum <- seq(1000, 100, by = -100)

# Evaluating the function at each level of snpnum
results <- lapply(snpnum, function(x) eth_to_plots(unipop_sample, n_snp = x)) 

eth_to_plots(unipop_sample,1000)


unipop_sample <- unipop
eth_to_plots(unipop,600) # generate objects for all 26 pops

# ---


unipop_sample <- sample(unipop,3)

eth_to_plots(unipop_sample,600)

eth_sep(score_sep,pop_sample_sep,keep_sep)
