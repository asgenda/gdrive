library(googledrive)
library(matrixStats)
library(dplyr)
library(tidyr)
library(ggplot2)

#drive_download(as_id("1bfCl7VPwU0_4XTol1PR0J1Cl1NvrUhy9"),overwrite=TRUE) #1000genomes.Rdata
load("pop_gen_sample.RData")


source("genda-eth-fun.R") # initialize functions created from rafa's code

eth_setup() # This takes a long time due to the tabs step; comment this out once initialized
