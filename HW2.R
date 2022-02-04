library(readr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(fastDummies)

#Exercise 1
#Eliminate missing values of wage and age
datind2009 <- fread("A1/datind/datind2009.csv")
data09<-subset(datind2009,wage!=0)
data09<-subset(data09,age>18)
y<-data09$wage[is.na(data09$wage)==FALSE]
x1<-data09$age[is.na(data09$wage)==FALSE]
cory1x1<-cor(x1,y) 

#Regress wage on age+intercept
x<-matrix(nrow = length(x1) ,ncol=2)
x[,1]<-rep(1,nrow(data09))
x[,2]<-x1
beta12<-solve(t(x)%*%x)%*%t(x)%*%y

#Adding age^2 as independent variable
xt<-matrix(nrow = length(x1),ncol=3)
xt[,1]<-rep(1,nrow(data09))
xt[,2]<-x1
xt[,3]<-x1^2
beta13<-solve(t(xt)%*%xt)%*%t(xt)%*%y

#Define the function of standard error
std<-function(x,y){
  beta<-solve(t(x)%*%x)%*%t(x)%*%y
  yhat<-x%*%beta
  e<-y-yhat
  s2<-t(e)%*%e/(length(x1)-2)
  varcor<-solve(t(x)%*%x)*s2[1,1]
  std<-sqrt(diag(varcor))
  return(std)
}
sd<-std(x,y)
sdlist<-list("sd intercept" =sd[1],"sd x"=sd[2])

nt1<-49
nt2<-499
nsample<-nrow(data09)
output1<-matrix(nrow=nt1,ncol=2)
output2<-matrix(nrow=nt2,ncol=2)
data1<-as.matrix(data.frame(x,y))
# For replications = 49
for (i in 1:nt1) {
  svec<-sample(1:nrow(data09),nsample,rep=TRUE)
  bmat<-data1[svec,]
  x<-bmat[,1:2]
  y<-bmat[,3]
  coef<-solve(t(x)%*%x)%*%t(x)%*%y
  output1[i,]<-coef
}
bootsd1<-apply(output1,2,sd)
# For replications = 499
for (i in 1:nt2) {
  svec<-sample(1:nrow(data09),nsample,rep=TRUE)
  bmat<-data1[svec,]
  x<-bmat[,1:2]
  y<-bmat[,3]
  co<-solve(t(x)%*%x)%*%t(x)%*%y
  output2[i,]<-co
}
bootsd2<-apply(output2,2,sd)

#Exercise 2
#Reading all individual datasets
dir1<-"D:/ECON 613/A1/datind/" 
file_list1<-list.files(path = dir1, pattern = "*.csv$",recursive = TRUE,full.names = TRUE)   
datind = paste(dir1,"datind.csv")
for(i in 1:length(file_list1)){
  df1 = fread(file = file_list1[i],encoding = 'UTF-8')
  write_csv(df1,path =datind,append = TRUE, col_names = TRUE)
}
datind <- read_csv("D:/ECON 613/A1/datind/ datind.csv")
#Only using those between 2005 and 2018
datind0518<-subset(datind,year>2004&year<2019)
data <- datind0518 %>% drop_na(wage)
data<-subset(data,wage!=0)
data$ag<-cut(data$age,breaks = c(0,17,25,30,35,40,45,50,55,60,Inf))
awage<-data %>% group_by(ag,year) %>% summarize(wage=mean(wage))
ywage<-data %>% group_by(year) %>% summarize(wage=mean(wage))
 
awage %>%
  filter(!(ag %in% "(0,17]")) %>%
  ggplot() +
  aes(x = ag, weight = wage) +
  geom_bar(fill = "#4682B4") +
  theme_minimal() +
  facet_wrap(vars(year))
awage %>%
  filter(!(ag %in% "(0,17]")) %>%
  ggplot() +
  aes(x = year, y = wage) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(ag))
 

data<-dummy_cols(data, select_columns ='year',remove_first_dummy = TRUE)
data$agesq<-data$age^2
attach(data)
X<-data.frame(age,agesq,year_2006,year_2007,year_2008,year_2009,year_2010,year_2011,year_2012,year_2013,year_2014,year_2015,year_2016,year_2017,year_2018)
X<-as.matrix(X)
Y<-wage
beta23<-solve(t(X)%*%X)%*%t(X)%*%Y
beta23[1:2]
detach(data)

#Exercise 3
datind2007 <- read_csv("D:/ECON 613/A1/datind/datind2007.csv")
data07<-subset(datind2007,empstat!="Inactive"&empstat!="Retired")
data07<-subset(data07,wage!=0)
data07$labor<-ifelse(data07$empstat=="Employed",1,0)
data07$agesq<-(data07$age)^2
data07$inter<-rep(1,nrow(data07))
 
flike = function(par,x1,x2,y){
yhat= par[1]*x1 + par[2]*x2
prob= pnorm(yhat)
prob[prob>0.999999] = 0.999999
prob[prob<0.000001] = 0.000001
like = y*log(prob) + (1-y)*log(1-prob)
return(-sum(like))
}

x1=data07$inter
x2=data07$age
yvar=data07$labor

set.seed(120)
start = runif(2)
res  = optim(start,fn=flike,method="BFGS",
             control=list(trace=6,REPORT=1,maxit=10000),
             x1=x1,x2=x2,y=yvar,hessian=TRUE)

reg3 <- glm(data07$labor~data07$age,family = binomial(link = "probit"))
summary(reg3)
#Exercise 4
datind <- read_csv("D:/ECON 613/A1/datind/ datind.csv")
datind0515<-subset(datind,year>2004&year<2016)
data0515<-subset(datind0515,empstat!="Inactive"&empstat!="Retired")
data0515<-data0515 %>% drop_na(wage)
data0515<-subset(data0515,wage!=0)
data0515$labor<-data0515$empstat=="Employed"
data0515$labor<-as.numeric(data0515$labor)
data0515$inter<-rep(1,nrow(data0515))
data0515<-dummy_cols(data0515, select_columns ='year',remove_first_dummy = TRUE)
attach(data0515)
var<-c("Intercept","Age","year_2006","year_2007","year_2008","year_2009","year_2010","year_2011","year_2012","year_2013","year_2014","year_2015")
#Optimization for Probit Model
probitlike<-function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y)
{
  yhat<-par[1]*x1+par[2]*x2+par[3]*x3+par[4]*x4+par[5]*x5+par[6]*x6+par[7]*x7+par[8]*x8+par[9]*x9+par[10]*x10+par[11]*x11+par[12]*x12
  pr<-pnorm(yhat)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like<-y*log(pr)+(1-y)*log(1-pr)
  return(-sum(like))
}

set.seed(123)
init<-runif(12,-0.6,0.2)
resp<-optim(init,fn=probitlike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=inter,x2=age,x3=year_2006,x4=year_2007,x5=year_2008,x6=year_2009,x7=year_2010,x8=year_2011,x9=year_2012,x10=year_2013,x11=year_2014,x12=year_2015,y=labor,hessian=TRUE)
plist<-list("Intercept" =resp$par[1],"Age"=resp$par[2],"year_2006"=resp$par[3],"year_2007"=resp$par[4],"year_2008"=resp$par[5],"year_2009"=resp$par[6],"year_2010"=resp$par[7],"year_2011"=resp$par[8],"year_2012"=resp$par[9],"year_2013"=resp$par[10],"year_2014"=resp$par[11],"year_2015"=resp$par[12])

fisher_info_resp = solve(resp$hessian)  
prop_sigma_p  = sqrt(diag(fisher_info_resp))
zvalue<-resp$par/prop_sigma_p
pvalue<-1-abs(pnorm(zvalue)-1/2)*2
sig_p<-data.frame(var,resp$par,prop_sigma_p,zvalue,pvalue)

reg41 <- glm(labor~age+year_2006+year_2007+year_2008+year_2009+year_2010+year_2011+year_2012+year_2013+year_2014+year_2015,family = binomial(link = "probit"))
summary(reg41)
#Optimization for Logit Model
logitlike<-function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y)
{
  xbeta<-par[1]*x1+par[2]*x2+par[3]*x3+par[4]*x4+par[5]*x5+par[6]*x6+par[7]*x7+par[8]*x8+par[9]*x9+par[10]*x10+par[11]*x11+par[12]*x12
  p<-exp(xbeta)/(1+exp(xbeta))
  p[p>0.999999] = 0.999999
  p[p<0.000001] = 0.000001
  like<-y*log(p)+(1-y)*log(1-p)
  return(-sum(like))
}

set.seed(1100)
init<-runif(12,-1,1.5)
resl<-optim(init,fn=logitlike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=inter,x2=age,x3=year_2006,x4=year_2007,x5=year_2008,x6=year_2009,x7=year_2010,x8=year_2011,x9=year_2012,x10=year_2013,x11=year_2014,x12=year_2015,y=labor,hessian=TRUE)
llist<-list("Intercept" =resl$par[1],"Age"=resl$par[2],"year_2006"=resl$par[3],"year_2007"=resl$par[4],"year_2008"=resl$par[5],"year_2009"=resl$par[6],"year_2010"=resl$par[7],"year_2011"=resl$par[8],"year_2012"=resl$par[9],"year_2013"=resl$par[10],"year_2014"=resl$par[11],"year_2015"=resl$par[12])

fisher_info_resl = solve(resl$hessian)  
prop_sigma_l  = sqrt(diag(fisher_info_resl))
zvaluel<-resl$par/prop_sigma_l
pvaluel<-1-abs(pnorm(zvaluel)-1/2)*2
sig_l<-data.frame(var,resl$par,prop_sigma_l,zvaluel,pvaluel)

reg42 <- glm(labor~age+year_2006+year_2007+year_2008+year_2009+year_2010+year_2011+year_2012+year_2013+year_2014+year_2015,family = binomial(link = "logit"))
summary(reg42)
#Optimization for Linear Model
OLSE2<-function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y){
  xbeta<-par[1]*x1+par[2]*x2+par[3]*x3+par[4]*x4+par[5]*x5+par[6]*x6+par[7]*x7+par[8]*x8+par[9]*x9+par[10]*x10+par[11]*x11+par[12]*x12
  e<-y-xbeta
  e2<-e^2
  return(sum(e2))
}

init<-runif(12,-0.1,0.9)
reso<-optim(init,fn=OLSE2,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=inter,x2=age,x3=year_2006,x4=year_2007,x5=year_2008,x6=year_2009,x7=year_2010,x8=year_2011,x9=year_2012,x10=year_2013,x11=year_2014,x12=year_2015,y=labor,hessian=TRUE)
olist<-list("Intercept" =reso$par[1],"Age"=reso$par[2],"year_2006"=reso$par[3],"year_2007"=reso$par[4],"year_2008"=reso$par[5],"year_2009"=reso$par[6],"year_2010"=reso$par[7],"year_2011"=reso$par[8],"year_2012"=reso$par[9],"year_2013"=reso$par[10],"year_2014"=reso$par[11],"year_2015"=reso$par[12])
X4<-cbind(inter,age,year_2006,year_2007,year_2008,year_2009,year_2010,year_2011,year_2012,year_2013,year_2014,year_2015)
Y4<-labor

std<-function(x,y){
  beta<-solve(t(x)%*%x)%*%t(x)%*%y
  yhat<-x%*%beta
  e<-y-yhat
  s2<-t(e)%*%e/((nrow(X4)-ncol(X4)))
  varcor<-solve(t(x)%*%x)*s2[1,1]
  std<-sqrt(diag(varcor))
  return(std)
}
osd<-std(X4,Y4)
tvalue3<-reso$par/osd
pvalue3<-1-abs(pt(tvalue3,df=nrow(X4)-ncol(X4))-1/2)*2
sig_o<-data.frame(var,reso$par,osd,tvalue3,pvalue3)

reg43 <- lm(labor~age+year_2006+year_2007+year_2008+year_2009+year_2010+year_2011+year_2012+year_2013+year_2014+year_2015)
summary(reg43)
#Exercise 5
x1=inter
x2=age
x3=year_2006
x4=year_2007
x5=year_2008
x6=year_2009
x7=year_2010
x8=year_2011
x9=year_2012
x10=year_2013
x11=year_2014
x12=year_2015
y=labor

#Marginal Effect for Probit Model
par<-resp$par[1:12]
m<-vector(length=12)
ME_Probitpdf<-function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y){
  xbeta<-par[1]*mean(x1)+par[2]*mean(x2)+par[3]*mean(x3)+par[4]*mean(x4)+par[5]*mean(x5)+par[6]*mean(x6)+par[7]*mean(x7)+par[8]*mean(x8)+par[9]*mean(x9)+par[10]*mean(x10)+par[11]*mean(x11)+par[12]*mean(x12)
  pdf<-dnorm(xbeta)
}
for (i in 1:12) {
  m[i]<-ME_Probitpdf(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y)*par[i]
}
pmelist<-list("Age"=m[2],"year_2006"=m[3],"year_2007"=m[4],"year_2008"=m[5],"year_2009"=m[6],"year_2010"=m[7],"year_2011"=m[8],"year_2012"=m[9],"year_2013"=m[10],"year_2014"=m[11],"year_2015"=m[12])
#Marginal Effect for Logit Model
par<-resl$par[1:12]
m2<-vector(length=12)
ME_Logitpdf<-function(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y){
  xbeta<-par[1]*mean(x1)+par[2]*mean(x2)+par[3]*mean(x3)+par[4]*mean(x4)+par[5]*mean(x5)+par[6]*mean(x6)+par[7]*mean(x7)+par[8]*mean(x8)+par[9]*mean(x9)+par[10]*mean(x10)+par[11]*mean(x11)+par[12]*mean(x12)
  pdf<-dlogis(xbeta)
}
for (i in 1:12) {
  m2[i]<-ME_Probitpdf(par,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,y)*par[i]
}
lmelist<-list("Age"=m2[2],"year_2006"=m2[3],"year_2007"=m2[4],"year_2008"=m2[5],"year_2009"=m2[6],"year_2010"=m2[7],"year_2011"=m2[8],"year_2012"=m2[9],"year_2013"=m2[10],"year_2014"=m2[11],"year_2015"=m2[12])

#Standard Errors for MEM
ntry<-5
mem_probit<-matrix(NA,ntry,12)
set.seed(13)
for (i in 1:ntry) {
  csample<-sample(1:nrow(data0515),nrow(data0515),rep=TRUE)
  sampledata<-data0515[csample,]
  x_521<-as.matrix(sampledata[,c(9,12:22)])
  y_521<-as.matrix(sampledata[,11])
  init = runif(12)
  res_probit = optim(init,fn=probitlike,method="BFGS",
                    control=list(trace=6,REPORT=1,maxit=1000),
                    x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
                    x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                    x11=x11,x12=x12,y=y,hessian=TRUE)
  probit_521 <- res_probit$par
  for (j in 1:12){
    mem_probit[i,j]<-ME_Probitpdf(probit_521,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,yvar)*probit_521[j]
  }
}
sder1<-apply(mem_probit,2,sd)
sderp<-data.frame(var,sderp)
 

mem_logit<-matrix(NA,ntry,12)
for (i in 1:ntry) {
  csample<-sample(1:nrow(data0515),nrow(data0515),rep=TRUE)
  sampledata<-data0515[csample,]
  x_521<-as.matrix(sampledata[,c(9,12:22)])
  y_521<-as.matrix(sampledata[,11])
  init = runif(12,-0.2,1)
  res_logit = optim(init,fn=logitlike,method="BFGS",
                     control=list(trace=6,REPORT=1,maxit=1000),
                     x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,
                     x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,
                     x11=x11,x12=x12,y=y,hessian=TRUE)
  logit_521 <- res_logit$par
  for (j in 1:12){
    mem_logit[i,j]<-ME_Logitpdf(logit_521,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,yvar)*logit_521[j]
  }
}
sder2<-apply(mem_logit,2,sd)
sderl<-data.frame(var,sder2)
 
