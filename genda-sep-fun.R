eth_sep <- function(score_sep_fun, pop_sample_sep_fun, keep_sep_fun) {
  
  # Load required libraries
  library(tree)
  
  # Convert score_sep_fun to data frame
  score_df <- data.frame(score = score_sep_fun, ethnicity = factor(pop_sample_sep_fun[keep_sep_fun]))
  
  # Ensure that 'score' variable is numeric
  score_df$score <- as.numeric(score_df$score.1)
  
  # Check for any NA values in 'score' and 'ethnicity' column
  if(any(is.na(score_df$score)) | any(is.na(score_df$ethnicity))){
    stop("NA values present in 'score' or 'ethnicity'. Please remove them before proceeding.")
  }
  
  # Randomly sample half of the data for training
  set.seed(1)
  train = sample(1:nrow(score_df), nrow(score_df)/2)
  
  # Hardcode nodes
  mcut <- 5
  
  # Fit a decision tree using score as predictor and ethnicity as response variables
  fit <<- tree(ethnicity ~ score, score_df, subset = train, mincut = mcut)
  
  # Get a summary of the tree model.
  fit_summary <<- summary(fit)
  
  # Print the summary.
  print(fit_summary)
  
  # Extract the number of terminal nodes from the summary.
  num_terminal_nodes <- sum(fit$frame$var == "<leaf>")
  
  preds <- predict(fit, newdata = score_df[-train,], type="class")
  
  true_values <- score_df[-train,"ethnicity"]
  
  error_rate <- mean(preds != true_values)
  
  
  
  
  
  print(paste0("Error Rate: ", error_rate))
  
  print(paste0("Number of Terminal Nodes: ", num_terminal_nodes))
  
  
  plot(fit, type="uniform")
  text(fit, cex=1)
  
  
  
  return(list(mse=mse,error_rate=error_rate,num_terminal_nodes=num_terminal_nodes))
}