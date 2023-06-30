# Come up with some measure of variability
# For those groups, do the chi^2 test where you can clearly distinguish the two.
# YRI CEU CLM
# For YRI, using chi^2, find SNPs that predict this
library(matrixStats)
ind <- which(pop_sample%in%c("YRI","CEU","CLM"))
m <- as.matrix(bed_mat[ind,])
y <- pop_sample[ind]
m <- m[,colSds(m)>0]


pvals <- apply(m, 2, function(x){
  tab <- table(x>0, y=="YRI")
  p0 <- tab[1,1]/sum(tab[,1])
  p1 <- tab[1,2]/sum(tab[,2])
  c(1-p0, 1-p1, chisq.test(tab)$p.value)
})
cor((y=="YRI"), m[,3] )
