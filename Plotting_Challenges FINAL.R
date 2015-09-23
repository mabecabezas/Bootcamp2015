setwd("/Users/MaBe/Desktop/Bootcamp_Mac")
snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld-1.txt',header=TRUE)
snps=as.matrix(snpsDataFrame)

compute_chisquare=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)
}

#let's make a second funtion that makes use of R's built in chisq.test function

compute_chisquare_2=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  #here we use the built-in function for the chi-sq distribution:
  exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) #note, here we don't multiply by n
  chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
  return(chisq)
}

# Apply the compute_chi_square function to each snp
chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)

#check to see that the chisquare statistcs are the same:
#first do this by computing Pearson's correlation coefficient:
cor.test(chisqs,chisqs2)

#we can also do a quick scatterplot:
plot(chisqs,chisqs2)

# Compute p-values for each chi-square value using the pchisq function
pvals=pchisq(chisqs,1,lower.tail=FALSE)




#Exercise 1a

setwd("/Users/MaBe/Desktop/Bootcamp_Mac")
snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld-1.txt',header=TRUE)
snps=as.matrix(snpsDataFrame)



#Exercise 1b

mean(pvals<0.05) #The proportion of P-values is 0.04509218
mean(pvals<0.01) #The proportion of P-values is 0.01021425
mean(pvals<0.001) #The proportion of P-values is 0.00124564



#Exercise 1c

length(pvals) # 4,014 SNPs
num_pval<-length(pvals)



#Exercise 1d

nn<-(1:4014)
exp_pvals<-nn/num_pval
#I just created my expected P-values


#Exercise 1e

sort(pvals)
sort_pvals<-sort(pvals)


#Exercise 1f
-log10(pvals)
log_sort_pvals<--log10(pvals)
-log10(exp_pvals)
log_exp_pvals<--log10(exp_pvals)


#Exercise 1g
log_sort_pvals<--log10(sort_pvals)
plot(log_exp_pvals, log_sort_pvals)


#Exercise 1h
lines(0:4,0:4,col=2, lty=2, lwd=2)
#I just add the line to show the relation between expected and observed values




#Exercise 2a
setwd("/Users/MaBe/Desktop/Bootcamp_Mac")
zz=read.table('pheno.sim.2014-1.txt',header=TRUE)
head(zz)



#Exercise 2b
xx<-zz[,2]
quantile(xx) #The value of the phenotype where 25% of the individuals fall is 4.768756
yy<-subset(zz,zz[,2]<4.768756) #I extracted IDs and phenotypes of the individuals fulfilling this criterion
controls<-yy[,1] #I extracted IDs of the individuals fulfilling this criterion


#Exercise 2c
xx<-zz[,2]
quantile(xx) #The value of the phenotype where 25% of the individuals fall is 7.354975
ww<-subset(zz,zz[,2]>7.354975) #I extracted IDs and phenotypes of the individuals fulfilling this criterion
case<-ww[,1] #I extracted IDs of the individuals fulfilling this criterion


#Exercise 2d
plot(density(xx))
abline(v=quantile(4.768756),lty=2,lwd=3,col=2)
abline(v=quantile(7.354975),lty=2,lwd=3,col=4)


#Exercise 2e
caseSNP=snps["rs7584086_T",]
case_genotypes<-caseSNP[case]


#Exercise 2f
caseSNP=snps["rs7584086_T",]
control_genotypes<-caseSNP[controls]


#Exercise 2g
Tcase<-table(case_genotypes)
#Number of cases individuals who have each genotype 1 are 12
#Number of cases individuals who have each genotype 2 are 3

#Exercise 2h
Tcontrol<-table(control_genotypes)
#Number of cases individuals who have each genotype 0 are 14
#Number of NA is 1

