library("stats")

##chi square test
obs=c(10,25,18,7)
chisq.test(obs)

cas=c(212,147,103,50,46,42)
chisq.test(cas)

A=c(10,25,18,7)
chisq.test(A)

M=c(212,147,103,50,46,42)
chisq.test(M)

##let r represnt the observed frequencies##
r=c(57,21,14)
chisq.test(r,p=c(0.72,0.2,0.08))

##Kolmogorov-Smirnov test
ks.sam=c(1.41,0.26,1.97,0.33,0.55,0.77,1.46,1.18)
ks.test(ks.sam[ks.sam<2],function(ks.sam)punif(ks.sam,0,2),alternative="two.sided")

l=c(1.41,0.26,1.97,0.33,0.55,0.77,1.46,1.18)
ks.test(l[l<2],function(l)punif(l,0,2),alternative="two.sided")

##runs test
##let 1 represent M and 0 represent F##
RX = factor(c(1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,1,0,1,1,1))
runs.test(RX)

N <- factor(sign(rnorm(5000)))
runs.test(N)
