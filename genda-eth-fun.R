# This function loads necessary R libraries and performs several steps of data manipulation and calculation. 
# Firstly, it computes minor allele frequencies for each SNP and removes SNPs with only one genotype.
# Then, it creates an index for each population and calculates the number of individuals per population.
# For each population, it calculates the proportions of AA, AB, BB genotypes for each SNP. 
# If minor alleles frequencies are greater than 0.5, it flips the cases.
# The function then computes allele frequency and deviation from Hardy-Weinberg equilibrium (a measure of genetic variation).
eth_setup <- function() {
  library(googledrive)
  library(matrixStats)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  #drive_find(n_max =2)
  #drive_download(as_id("1bfCl7VPwU0_4XTol1PR0J1Cl1NvrUhy9"),overwrite=TRUE) #1000genomes.Rdata
  #load("pop_gen_sample.RData")
  
  ## compute minor allele frequences
  maf <<- colSums(bed_mat)/(2*nrow(bed_mat))
  
  ## remove SNPs with only one genotype
  bed_mat <<- bed_mat[,maf > 0 & maf < 1]
  maf <<- colSums(bed_mat)/(2*nrow(bed_mat))
  
  
  ## get the column indexes for each population
  pop_indexes <<- split(seq_along(pop_sample), pop_sample)
  
  ## number of individuals per population
  n <- sapply(pop_indexes, length)
  
  ## for each population get the table of AA, AB, BB for each SNP
  # A: This step took ~5 min to finish
  tabs <<- sapply(pop_indexes, function(ind)
    apply(bed_mat[ind,], 2, function(gt) prop.table(table(factor(gt, c(0,1,2))))),
    simplify = "array")
  
  ## flip cases with mafs > 0.5
  ind <- which(maf > 0.5)
  tabs[c(1,3),ind,] <<- tabs[c(3,1),ind,]
  
  ## compute the allele frequency
  p <<- (tabs[1,,]*2 + tabs[2,,])/2
  
  ## compute deviation from hardy-weinberg equilibrium
  chisq <<- (tabs[1,,] - p^2)^2 / p + (tabs[2,,]- 
                                         2*p*(1-p))^2 / 
    (p*(1-p)*2) + (tabs[3,,] - (1-p))^2 / (p^(-4))
  chisq[p==1] <- NA_real_
  chisq <- na.omit(chisq) 
}
# Run the function
# eth_setup()

# Define a function 'eth_to_plots' that takes two arguments: a vector of ethnicities and an optional number of SNPs (default is 600)
# This function generates scatter and boxplot visualizations based on given ethnicities and SNPs.
# I generalized Rafa's function that took three ethnicities and 600 SNPs, so now it will accept any # of ethnicities
# The output plots are saved in a newly created sub-directory. If the directory already exists, it adds an incrementing suffix to create a unique name. 
# The names should be in this format: "n_snp" + all the strings in ethnicities concatenated and with dashes between them. 
#So, the output folder path could look like this: "output-eth-to-plots/600-CDX-GBR-YRI". 
#If the total length of this path exceeds 40 characters, cut it off at the end and add an incrementing number to prevent folders from being overwritten.

eth_to_plots <- function(ethnicities, n_snp = 600) {
  
  main_pops <- ethnicities
  
  ind <- match(main_pops, dimnames(tabs)[[3]])
  
  x <- tabs[1, ,ind]
  n_locs <- round(n_snp/6) 
  
  up <- sapply(1:length(main_pops), function(i){
    order(rowMeans(x[,-i]) - x[,i], decreasing = TRUE)[1:n_locs]
  })
  
  dn <- sapply(1:length(main_pops), function(i){
    order(rowMeans(x[,-i]) - x[,i])[1:n_locs]
  })
  
  keep <- which(pop_sample%in%main_pops)
  
  score <- apply(up, 2, function(ind) rowSums(bed_mat[keep,ind])) - 
    apply(dn, 2, function(ind) rowSums(bed_mat[keep,ind]))
  
  plot_scatter <- score |> as.data.frame() |> setNames(main_pops) |> mutate(pop = pop_sample[keep]) |>
    ggplot(aes_string(unipop_sample[1], unipop_sample[2], color = "pop")) +
    geom_point()
  
  plot_box <- score |> as.data.frame() |> setNames(main_pops) |> mutate(pop = pop_sample[keep]) |>
    pivot_longer(-pop, values_to = "score") |>
    ggplot(aes(pop, score, color = pop)) +
    geom_boxplot() +
    facet_wrap(~name)
  
  
  # Format output folder name
  folder_name_base <- paste("output-eth-to-plots/", n_snp , "-", paste(ethnicities,collapse="-"), sep="")
  
  # Check if length of folder name base exceeds limit
  if (nchar(folder_name_base)>40){
    folder_name_base<-substr(folder_name_base,start=1 ,stop=40)
  }
  
  i<-0
  new_folder_path<-paste0(folder_name_base,"-",i)
  
  # Check if output folder already exists and add incrementing suffix until unique name is found
  while(dir.exists(new_folder_path)){
    i<- i+1
    new_folder_path<-paste0(folder_name_base,"-",i)
  }
  
  # Create new output folder if it doesn't exist
  if(!dir.exists(new_folder_path)){
    dir.create(new_folder_path)
    #dir.create(paste0(new_folder_path,"/0")) # creating subfolder named "0"
  }
  
  
  ggsave(paste0(new_folder_path,"/group_box.png"), plot_box,width=16 , height=10 , dpi=300) # set width to 49 to vis all eth
  ggsave(paste0(new_folder_path,"/group_scatter.png"), plot_scatter,width=12 , height=12 , dpi=300)
  
  print(plot_scatter)
  
  rm(plot_scatter)
  rm(plot_box)
  
  
  print(paste("Saved 'group_box.png' and 'group_scatter.png' in the directory:", new_folder_path))
}

# Run the function
# eth_to_plots(new_pops,n_snp = 600)
