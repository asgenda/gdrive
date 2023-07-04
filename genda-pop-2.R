# generate all possible combinations of 3 elements from unipop

combinations <- combn(unipop, 3)

# deterministic version
combinations <- combinations[,1:10]

# random selection version
random_columns <- sample(ncol(combinations), 10)
combinations <- combinations[,random_columns]

# define a function that applies eth_to_plots and eth_sep to each combination
process_combination <- function(combination) {
  unipop_sample <<- combination
  eth_to_plots(unipop_sample,600)
  results <- eth_sep(score_sep,pop_sample_sep,keep_sep)
  
  return(results)
}

# apply the function to each column of combinations (i.e., each unique combination)
results_list <- apply(combinations, 2, process_combination)
