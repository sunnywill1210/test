########################################################################
### Program to do two sample Maximum and Chisquare tests for CSH and ACH
########################################################################

##### Joint Test for CSH and ACH for JCP variable
rm(list=ls(all=TRUE))
library(MASS)
library(survival)
library(cmprsk)
#setwd('~/Dropbox/Dissertation/bookchapter/program')

#load data
source(".\\bmt.R")

#Specify which group variable to test
group=mini

teststat_CSHACH=matrix(NA,nrow=1,ncol=5)
pvalue_CSHACH=matrix(NA,nrow=1,ncol=5)

### Test 1: Wald test for group coefficient in cox model for cause specific hazard for event of interest### 
sdfcox1 <- coxph(Surv(time,type==1) ~ group) # CSH model
teststat_CSHACH[1]=(sdfcox1$coef/sqrt(sdfcox1$var))^2  
pvalue_CSHACH[1]=1-pchisq(teststat_CSHACH[1],1)

### Test 2: Wald test for group coefficient in cox model for all cause hazard### 
sdfcox_all <- coxph(Surv(time,type != 0) ~ group) # ACH model
teststat_CSHACH[2]=(sdfcox_all$coef/sqrt(sdfcox_all$var))^2  
pvalue_CSHACH[2]=1-pchisq(teststat_CSHACH[2],1)

### Test 3: bonferroni adjustment for test 1 and 2 ###
pvalue_CSHACH[3]=min(pvalue_CSHACH[1],pvalue_CSHACH[2])*2

### Test 4: Joint logrank test statistics ###
var_1=sdfcox1$var[1,1] 		#variance of beta_j
var_2=sdfcox_all$var[1,1]		#variance of beta_\dot
var_12=var_2

### Chisquare test ###
corr<-var_12/sqrt(var_1*var_2)

W_1=sdfcox1$coef/sqrt(var_1)
W_2=sdfcox_all$coef/sqrt(var_2)

W=matrix(c(W_1,W_2),nrow=1)
cov=matrix(c(1,corr,corr,1),nrow=2) #covariance matrix
if (det(cov)==0) {browser()}
teststat_CSHACH[4]=W%*%solve(cov)%*%t(W)   #chisquare test stat
pvalue_CSHACH[4]=1-pchisq(teststat_CSHACH[4],2)

### maximum test ###
teststat_CSHACH[5]=max(abs(sdfcox1$coef[1]/sqrt(sdfcox1$var[1,1])), abs(sdfcox_all$coef[1]/sqrt(sdfcox_all$var[1,1])))
nbootstrap=10000
sample=mvrnorm(n = nbootstrap, c(0,0), Sigma=matrix(c(1,corr,corr,1),nrow=2), tol=1e-4)
max=matrix(NA,nrow=nbootstrap,ncol=1)
for (j in 1:nbootstrap){max[j]=max(abs(sample[j,]))}
pvalue_CSHACH[5] = sum(max>teststat_CSHACH[5])/nbootstrap

colnames(pvalue_CSHACH)=c("CSH","ACH","Bonf","Chi","Max")
pvalue_CSHACH



