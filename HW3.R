#Import libraries and basic settings
library(margins)
library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(nnet)
library(mlogit)
library(survival)
setwd('Downloads/Econ 613/Data') #for my macbook
setwd('D:/ECON 613/A3/Data') # for my windows notebook
options(scipen = 200)# No scientific notation
#============================================================================================
# Import Dataset
#============================================================================================
datjss <- read_csv("datjss.csv")
datsss <- read_csv("datsss.csv")
datstu <- read_csv("datstu_v2.csv")
datsss <- datsss %>% distinct(schoolcode,.keep_all = TRUE)
#============================================================================================
#Exercise 1
#============================================================================================
#Nubmer of students, school, programs
Nstudent <- nrow(datstu) 
Nschool  <- n_distinct(datsss$schoolcode)
Nschool1 <- n_distinct(datsss$schoolname)
programs <- c(datstu$choicepgm1,datstu$choicepgm2,datstu$choicepgm3,datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)
programs <- na.omit(programs)
Nprogram <- n_distinct(programs) 

#Number of choice
Ldatstu <- datstu %>% gather('schoolcode1':'schoolcode6', key = 'school', value = 'schoice')
Ldatstu <- Ldatstu %>% gather('choicepgm1':'choicepgm6', key = 'pgm', value = 'pchoice')
Ldatstu$school <- str_sub(Ldatstu$school, start = 11, end = 11)
Ldatstu$school <- as.numeric(Ldatstu$school)
Ldatstu$pgm <- str_sub(Ldatstu$pgm, start = 10, end = 10)
Ldatstu$pgm <- as.numeric(Ldatstu$pgm)
Ldatstu_matching <- subset(Ldatstu,Ldatstu$school==Ldatstu$pgm&Ldatstu$schoice!='NA'&Ldatstu$pchoice!='NA')
Nchoice  <- nrow(unique(Ldatstu_matching[c('schoice','pchoice')]))

#Number of chosing school in the same district
Ldatstu_matching_sss <- Ldatstu_matching %>% rename(schoolcode=schoice)
Ldatstu_matching_sss <- merge(Ldatstu_matching_sss,datsss,by="schoolcode")
datasame <- Ldatstu_matching_sss %>% group_by(V1.x) %>% summarise(same = jssdistrict[1] %in% sssdistrict)
datasame$same <- as.numeric(datasame$same)
Nsame <- sum(datasame$same)

#Number of admitted student in each school
Ldatstu_matching_sss_admit <- subset(Ldatstu_matching_sss,rankplace!=99)
Ldatstu_matching_sss_admit <- subset(Ldatstu_matching_sss_admit,rankplace==school)
Nadmit <- data.frame(table(Ldatstu_matching_sss_admit$schoolcode))
Nadmit <- Nadmit %>% rename(schoolcode=Var1)
Nadmit <- Nadmit %>% rename(Nadmit=Freq)

#Cutoff
lowscore <- Ldatstu_matching_sss_admit %>% group_by(schoolcode) %>% summarise(minscore=min(score))

#Quality(meanscore)
quality  <- Ldatstu_matching_sss_admit %>% group_by(schoolcode) %>% summarise(meanscore=mean(score))
#============================================================================================
#Exercise 2
#============================================================================================
Ldatstu_matching_sss_admit$choice <- paste(Ldatstu_matching_sss_admit$schoolcode,Ldatstu_matching_sss_admit$pchoice,sep = ' ')
Ncadmit   <- data.frame(table(Ldatstu_matching_sss_admit$choice))
Ncadmit   <- Ncadmit %>% rename(choice=Var1)
Ncadmit   <- Ncadmit %>% rename(people=Freq)
clowscore <- Ldatstu_matching_sss_admit %>% group_by(choice) %>% summarise(minscore=min(score))
cquality  <- Ldatstu_matching_sss_admit %>% group_by(choice) %>% summarise(meanscore=mean(score))

schooldata <- unique(Ldatstu_matching_sss_admit[c('schoolcode','pchoice')])
schooldata$choice <- paste(schooldata$schoolcode,schooldata$pchoice,sep = ' ')
schooldata <- merge(datsss,schooldata,by='schoolcode')
schooldata <- merge(schooldata,Ncadmit,by='choice')
schooldata <- merge(schooldata,clowscore,by='choice')
schooldata <- merge(schooldata,cquality,by='choice')
schooldata <- schooldata[,-3]
#============================================================================================
#Exercise 3
#============================================================================================
Ldatstu_matching_sss_jss <- merge(Ldatstu_matching_sss,datjss,by='jssdistrict')
Ldatstu_matching_sss_jss <- Ldatstu_matching_sss_jss  %>% rename(jsslong = point_x)
Ldatstu_matching_sss_jss <- Ldatstu_matching_sss_jss  %>% rename(jsslat  = point_y)
attach(Ldatstu_matching_sss_jss)
Ldatstu_matching_sss_jss$distance <- sqrt((69.172 * (ssslong-jsslong) * cos(jsslat/57.3))^2 + (69.172 * (ssslat-jsslat))^2)
detach(Ldatstu_matching_sss_jss)
#============================================================================================
#Exercise 4
#============================================================================================
#4.1
Ldatstu_matching_sss_jss$scode_rev <- str_sub(Ldatstu_matching_sss_jss$schoolcode, start = 1, end = 3)
Ldatstu_matching_sss_jss$pchoice <- as_factor(Ldatstu_matching_sss_jss$pchoice) 
Ldatstu_matching_sss_jss$pgm_rev <- Ldatstu_matching_sss_jss$pchoice %>% fct_collapse(arts = c("General Arts","Visual Arts"),economics = c("Business","Home Economics"),science="General Science",other_level = 'others')
levels(Ldatstu_matching_sss_jss$pgm_rev)
#4.2
Ldatstu_matching_sss_jss$choice_rev <- paste(Ldatstu_matching_sss_jss$scode_rev,Ldatstu_matching_sss_jss$pgm_rev,sep = ' ')
#Remove NA in the dataset
Ldatstu_matching_sss_jss <- subset(Ldatstu_matching_sss_jss,score!='NA'& rankplace!='NA')
#4.3
lowscore_rev <- Ldatstu_matching_sss_jss %>% group_by(choice_rev) %>% summarise(minscore=min(score))
quality_rev  <- Ldatstu_matching_sss_jss %>% group_by(choice_rev) %>% summarise(meanscore=mean(score))
quality_rev$choice_rev <- as.factor(quality_rev$choice_rev)
quality_rev$choice_rev <- as.numeric(quality_rev$choice_rev)
#============================================================================================
#Exercise 5 
#============================================================================================
Ldatstu_matching_sss_jss_first <- subset(Ldatstu_matching_sss_jss,school==1)
Ldatstu_matching_sss_jss_first_high <- Ldatstu_matching_sss_jss_first[order(Ldatstu_matching_sss_jss_first$score,decreasing = TRUE), ] 
sample <- Ldatstu_matching_sss_jss_first_high[1:20000,]
#Choose a sample of size 300 (I Tried with larger samples but waited too long to get a result.)
set.seed(1232)
x <- sample(1:nrow(sample),300)
small_sample <- sample[x,]
small_sample$choice_rev <- as.factor(small_sample$choice_rev)
small_sample$choice_rev <- as.numeric(small_sample$choice_rev)
small_sample$choice_rev 
#Number of choice in the small sample
m <- n_distinct(small_sample$choice_rev)
#Likelihood Function for Multinomial Logit
mlogit_like_fun = function(param,data)
{
  score      =  data$score
  ch         =  data$choice_rev
  ni = nrow(data)
  nj = length(unique(ch))
  nj_par = nj-1
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1 = param[1:nj_par]
  pn2 = param[(nj_par+1):(2*nj_par)]
  ut[,1]<-rep(0,ni)
  for (j in 2:nj)
  {
    ut[,j] = pn1[j-1] + score*pn2[j-1]
  }
  prob  = exp(ut)       
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}
#Package Results
multinom_model <- multinom(choice_rev~score,data=small_sample,maxit=10000)
sum <- summary(multinom_model)
start_ideal <- c(sum$coefficients[,1],sum$coefficients[,2])
#Choose Starting Value
set.seed(123)
noise   = runif((m-1)*2,-0.02,0.02)
start   = start_ideal+noise
res1    = optim(start,fn=mlogit_like_fun,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),data=small_sample,hessian=TRUE)
par_m   = res1$par
#Coefficients Output
c_constant  <- c(0,par_m[1:m-1])
c_score     <- c(0,par_m[m:(-2+2*m)])
coefficients_mlogit <- cbind(c_constant,c_score)
#Marginal Effect 
#People-Choice Matrix
mlogit_prob_matrix = function(param,data)
{
  score      =  data$score
  ch         =  data$choice_rev
  ni = nrow(data)
  nj = length(unique(ch))
  nj_par = nj-1
  ut = mat.or.vec(ni,nj)
  # multinomial logit pij matrix
  pn1 = param[1:nj_par]
  pn2 = param[(nj_par+1):(2*nj_par)]
  ut[,1]<-rep(0,ni)
  for (j in 2:nj)
  {
    ut[,j] = pn1[j-1] + score*pn2[j-1]
  }
  ut  = exp(ut)       
  ut  = sweep(ut,MARGIN=1,FUN="/",STATS=rowSums(ut))
  return(ut)
}
prob_matrix<-mlogit_prob_matrix(par_m,small_sample)
prob_matrix
colnames(prob_matrix) <- c(1:m)
#Computation by formula
mlogit_me=matrix(0,nrow=300, ncol=m)
for (i in 1:300) {
  beta_bar <- sum(prob_matrix[i,]*c_score)
  mlogit_me[i,]=prob_matrix[i,]*(c_score-beta_bar)
}
mlogit_me <- apply(mlogit_me, 2, mean)
mlogit_me <- as.data.frame(mlogit_me)
colnames(mlogit_me)<-'me'
#============================================================================================
#Exercise 6 
#============================================================================================
#Get clogit dataset
small_sample_clogit <- merge(small_sample,quality_rev,by='choice_rev')
#Log like function for conditional logit model
clogit_like_fun = function(param,data){
  quality     =  data$meanscore
  ch          =  data$choice_rev
  ni = nrow(data)
  nj = length(unique(data[,1]))
  ut = mat.or.vec(ni,nj)
  data_unique=distinct(data,choice_rev,.keep_all = TRUE)
  quality_unique = data_unique[order(data_unique$choice_rev),]$meanscore
  # conditional logit
  pn1 = c(0,param[1:nj-1])
  for (j in 1:nj)
  {
    ut[,j] = pn1[j] + param[nj] * quality_unique[j] 
  }
  prob  = exp(ut)       
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}
#Optimization
set.seed(233)
start_2 = runif(m,-1,1)
res2    = optim(start_2,fn=clogit_like_fun,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),data=small_sample_clogit,hessian=TRUE)
res2$par
#Coefficients output
co_constant <- as.data.frame(c(0,res2$par[1:(m-1)]))
co_quality  <- res2$par[m]
colnames(co_constant) <- "co_constant"
view(co_constant)
#Marginal Effects for Conditional Logit
#People-choice Matrix
clogit_prob_matrix = function(param,data){
  quality     =  data$meanscore
  ch          =  data$choice_rev
  ni = nrow(data)
  nj = length(unique(data[,1]))
  ut = mat.or.vec(ni,nj)
  data_unique=distinct(data,choice_rev,.keep_all = TRUE)
  quality_unique = data_unique[order(data_unique$choice_rev),]$meanscore
  # conditional logit
  pn1 = c(0,param[1:nj-1])
  for (j in 1:nj)
  {
    ut[,j] = pn1[j] + param[nj] * quality_unique[j] 
  }
  ut  = exp(ut)       
  ut  = sweep(ut,MARGIN=1,FUN="/",STATS=rowSums(ut))
  return(ut)
}
#Pij Matrix for conditional logit
prob_matrix_clogit <- clogit_prob_matrix(res2$par,small_sample_clogit)
#Computing ME by formula
deltaijk=array(0,dim = c(300,m,m))
for (i in 1:300) {
  diag(deltaijk[i,,]) <- 1
}
clogit_me=array(0,dim=c(300,m,m))
for (i in 1:300) {
  for (j in 1:m) {
    for (k in 1:m) {
      clogit_me[i,j,k]=prob_matrix_clogit[i,j]*(deltaijk[i,j,k]-prob_matrix_clogit[i,k])*co_quality
    }
  }
}
clogit_me=apply(clogit_me,c(2,3),mean)
colnames(clogit_me)<-c(1:m)
view(clogit_me)
#============================================================================================
#Exercise 7
#============================================================================================
small_sample_noother <- subset(small_sample,pgm_rev != 'others')
small_sample_noother $ choice_rev <- as.factor(small_sample_noother$choice_rev)
m1 <- n_distinct(small_sample_noother$choice_rev)
#Package Results
multinom_model_noother <- multinom(choice_rev~score,data=small_sample_noother,maxit=10000)
sum1 <- summary(multinom_model_noother)
start_ideal_noother <- c(sum1$coefficients[,1],sum1$coefficients[,2])
#Choose Starting Value
set.seed(123)
noise_noother  = runif((m1-1)*2,-0.02,0.02)
start_noother  = start_ideal_noother+noise_noother
#Optimization
res_noother    = optim(start_noother,fn=mlogit_like_fun,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),data=small_sample_noother,hessian=TRUE)
par_m_noother  = res_noother$par
#Coefficients Output
c_constant_noother  <- c(0,par_m_noother [1:m1-1])
c_score_noother     <- c(0,par_m_noother [m1:(-2+2*m1)])
coefficients_mlogit_noother <- cbind(c_constant_noother ,c_score_noother)
#Prob Matrix
prob_matrix_noother<-mlogit_prob_matrix(par_m_noother,small_sample_noother)
colnames(prob_matrix_noother) <- c(1:53)
