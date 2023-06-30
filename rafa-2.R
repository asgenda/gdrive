library(matrixStats)

geno_trim <- geno[apply(geno,1,function(x) mean(x>0))>0.01,]
# This doesn't work because the sparse mx is always greater than zero

y <- as.numeric(population=="FIN")
table(y)


i <- 150
x <- geno_trim[i,]>0

y <- as.numeric(population=="FIN")
table(y,x)


maf <- rowSums(geno)
geno_trim <- geno[maf>=5,]

# log (p1/p0)
# |p1-p0|

p <- mean(x)

table(y,x_star)
p_1 <- mean(x[y==1]+.5)
p_0 <- mean(x[y==0]+.5)
lr <- log2(p_1/p_0)

null <- replicate(10000, {
  x_star <- sample(c(0,1),ncol(geno_trim),replace=TRUE,prob = c(1-p,p))
  p_1 <- mean(x_star[y==1]+.5)
  p_0 <- mean(x_star[y==0]+.5)
  log2(p_1/p_0)
})


# this code is wrong, get the right code from Maya

# today, we computed the log odds, but there's another called the chi^2 stat; how does what we observed /n
# compare to the expected 
# obs - expected
# redo this code and implement a chi^2 statistic 
# genetic admixtures are very diverse/extreme; same with latin america
# Is Race Biological?
# develop an algorithm that finds SNPs that are different for ethnicities
# potential answers: I can 100% distinguish these races; No, it's a mixture; There's no way to tell, it's too mixed
# 




