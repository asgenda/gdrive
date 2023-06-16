#install.packages("googledrive")
library(googledrive)
library(readr)
drive_find(n_max =2)
drive_download(as_id("11UCRwzRnYRsX54hgYCkyn0pcyZq_Hr0k"),overwrite=TRUE) #1000genomes.Rdata
drive_download(as_id("1pyObSyY-aAxj1KLT3Hxi1kyaDolRngrk"),overwrite=TRUE) #1000genomes.r 
drive_download(as_id("1UEdAKvzTgV5pyt8ARib3GRkwwd-AfG9x"),overwrite=TRUE) #1000genomes.pdf
load("1000genomes.Rdata")

drive_download(as_id("1oXdOYjUbz25H7NWJJX2Y-ypCPzxhHvQ2"),overwrite=TRUE) #brain_subset_metadata.csv
brain_subset_metadata <- read_csv("brain_subset_metadata.csv")
