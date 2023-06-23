

# Load necessary libraries
library(Matrix)
library(tidyverse)

# Convert the population vector to a factor
population <- as.factor(population)

# Calculate the total number of SNPs and people
num_SNPs <- nrow(geno_trim)
num_people <- ncol(geno_trim)

# Create a data frame to store the results
results <- data.frame(SNP = 1:num_SNPs, effect = numeric(num_SNPs))

# Loop through each SNP and perform a logistic regression
for (i in 1:num_SNPs) {
  # Extract SNP data from the geno_trim matrix
  SNP_data <- geno_trim[i,]
  
  # Perform logistic regression of ethnicity on SNP data
  model <- glm(population ~ SNP_data, family = "binomial")
  
  # Store the effect size (coefficient) for this SNP in the results data frame
  results$effect[i] <- coef(model)["SNP_data"]
}

# Display the results data frame
results