# Load required libraries
library(tree)

# Convert score_sep to data frame
score_df <- data.frame(score = score_sep[,1], ethnicity = factor(pop_sample_sep[keep_sep]))

# Ensure that 'score' variable is numeric
score_df$score <- as.numeric(score_df$score)

# Check for any NA values in 'score' and 'ethnicity' column
if(any(is.na(score_df$score)) | any(is.na(score_df$ethnicity))){
  stop("NA values present in 'score' or 'ethnicity'. Please remove them before proceeding.")
}

# Randomly sample half of the data for training
set.seed(1)
train = sample(1:nrow(score_df), nrow(score_df)/2)

# Fit a decision tree using score as predictor and ethnicity as response variables
fit = tree(ethnicity ~ score, score_df, subset = train, mincut = 5)

# Print a summary of the tree
summary(fit)

# Use tree for prediction on test dataset
preds <- predict(fit, newdata = score_df[-train,])

# Calculate MSE
mse <- mean((preds - score_df[-train, "score"])^2)
cat("Mean Squared Error:", mse)

# Plot the decision tree
plot(fit, type = "uniform")
text(fit, cex = 1)
