library(googledrive)
library(matrixStats)
library(dplyr)
library(tidyr)
library(ggplot2)

#drive_find(n_max =2)
#drive_download(as_id("1bfCl7VPwU0_4XTol1PR0J1Cl1NvrUhy9"),overwrite=TRUE) #1000genomes.Rdata
load("pop_gen_sample.RData")


## compute minor allele frequences
maf <- colSums(bed_mat)/(2*nrow(bed_mat))

## remove SNPs with only one genotype
bed_mat <- bed_mat[,maf > 0 & maf < 1]
maf <- colSums(bed_mat)/(2*nrow(bed_mat))


## get the column indexes for each population
pop_indexes <- split(seq_along(pop_sample), pop_sample)

## number of individuals per population
n <- sapply(pop_indexes, length)

## for each population get the table of AA, AB, BB for each SNP
# A: This step took ~5 min to finish
tabs <- sapply(pop_indexes, function(ind)
  apply(bed_mat[ind,], 2, function(gt) prop.table(table(factor(gt, c(0,1,2))))),
  simplify = "array")

## flip cases with mafs > 0.5
ind <- which(maf > 0.5)
tabs[c(1,3),ind,] <- tabs[c(3,1),ind,]

## compute the allele frequency
p <- (tabs[1,,]*2 + tabs[2,,])/2


## compute deviation from hardy-weinberg equilibrium
chisq <- (tabs[1,,] - p^2)^2 / p + (tabs[2,,]- 2*p*(1-p))^2 / 2*p*(1-p) + (tabs[3,,] - (1-p))^2 / (1-p)^2
chisq[p==1] <- 0
chisq <- colSums(chisq)


## show total deviation for different pops
totaldev <- data.frame(pop = names(chisq), chisq = chisq) |> 
  mutate(pop = reorder(pop, chisq)) |>
  ggplot(aes(pop, chisq)) + geom_col() + coord_flip()

# ---

## compute naive distance between populations based on allele frequencies
# Show this plot first
d <- dist(t(tabs[1,,] ))
plot(hclust(d))

# Might crash R
#d <- dist(t(bed_mat[,] ))
#plot(hclust(d))

## from the above we pick inviduals
## We are picking CDX, GBR, YRI
# I created a function starting here named eth_to_plots
main_pops <- c("CDX", "GBR", "YRI")

ind <- match(main_pops, dimnames(tabs)[[3]])

## For each group, pick the top snps that most distinguish
x <- tabs[1, ,ind]
n_locs <- round(600/6) ## this is the number of snps used to construct index
up <- sapply(1:3, function(i){
  order(rowMeans(x[,-i]) -  x[,i], decreasing = TRUE)[1:n_locs]
})
dn <- sapply(1:3, function(i){
  order(rowMeans(x[,-i]) -  x[,i])[1:n_locs]
})

keep <- which(pop_sample%in%main_pops)
score <- apply(up, 2, function(ind) rowSums(bed_mat[keep,ind])) - 
  apply(dn, 2, function(ind) rowSums(bed_mat[keep,ind]))
 
## with just 600 snps we can perfectly separate the groups
score |> as.data.frame() |> setNames(main_pops) |> mutate(pop =  pop_sample[keep]) |>
  ggplot(aes(CDX, GBR, color = pop)) +
  geom_point()

score |> as.data.frame() |> setNames(main_pops) |> mutate(pop =  pop_sample[keep]) |>
  pivot_longer(-pop, values_to = "score") |>
  ggplot(aes(pop, score, color = pop)) +
  geom_boxplot() +
    facet_wrap(~name)

# My function ends here ^
##How do other populations look?

new_pops <- c("CDX", "GBR", "YRI", "PUR", "MXL", "ASW")
keep <- which(pop_sample%in%new_pops)
score <- apply(up, 2, function(ind) rowSums(bed_mat[keep,ind])) - 
  apply(dn, 2, function(ind) rowSums(bed_mat[keep,ind]))

## with just 600 snps we can *not* perfectly separate the groups
score |> as.data.frame() |> setNames(main_pops) |> mutate(pop =  pop_sample[keep]) |>
  ggplot(aes(YRI, GBR, color = pop)) +
  geom_point()

score |> as.data.frame() |> setNames(main_pops) |> mutate(pop =  pop_sample[keep]) |>
  pivot_longer(-pop, values_to = "score") |>
  ggplot(aes(pop, score, color = pop)) +
  geom_boxplot() +
  facet_wrap(~name)


## Some genes are very correlated. What can we do about this?
cc <- cor(as.matrix(bed_mat[,c(up[,1],dn[,1])]))
heatmap(cc)

## Can you separate the main populations using less SNP? how low can you go?

## If you re-run the classification algorithms with PUR, CLM, IBS can define a score to separate?

## Are there any other triplets you can separate easily?

## Can you use the main scores to provide a % of each of the main populations for each person?

## Use PCA to reduce the number SNPs further.



