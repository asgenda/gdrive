
# Find ethnicites, without using ethnicity
cc = bed_mat%*%t(bed_mat)
pca <- eigen(cc)
plot(pca$vectors[,c(2,3)],col=as.numeric(as.factor(pop_sample)))
boxplot(pca$vectors[,9] ~ pop_sample)
