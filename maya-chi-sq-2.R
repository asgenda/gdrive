i <- 140
geno_trim <- bed_mat[apply(bed_mat,1,function(x) mean(x>0))>0.01,] 

x <-geno_trim[i,] >0

x

y <- as.numeric(super_pop_sample=="EAS") #converts chr vector into num vector

tbl = table(y,x) #table of 150 SNIP (bc 150 row and greater than) vs. the num vector of FIN pop

tbl

chisq.test(tbl, y=NULL, correct = TRUE, p = rep(1/length(x), length(x)),rescale.p = FALSE, simulate.p.value = TRUE, B = 20000)

pchisq(0.24554,3,lower.tail = TRUE)
