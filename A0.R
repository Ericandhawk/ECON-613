#Exercise 1
setwd("D://ECON 613")
dir("A0")
packs<-c("Hmisc","gdata","boot","xtable","MASS","moments","snow","mvtnorm")
install.packages(packs)
library("Hmisc")
dir()
ls()
678%%9
save.image("RE.RDATA")
?mean
??cut2
log(-1)

#Exercise 2
T<-Titanic
dimnames(T)
sum(T)
sum(T[,,"Adult",])
sum(T["Crew",,,])
sum(T["3rd",,"Child",])
sum(T["2nd","Female","Adult",])
sum(T["1st","Male","Child",])
sum(T["Crew","Female",,"Yes"])
sum(T["1st","Male","Adult","Yes"])
prop.table(T["1st","Male","Adult",])
prop.table(T["1st","Female","Adult",])
prop.table(T["1st","Male","Child",])
prop.table(T["3rd","Female","Adult",])

#Exercise 3
c1<-(1:50)
c2<-seq(1,50,1)
c3<-rev(50:1)
c4<-(50:1)
c5<-seq(50,1,-1)
c6<-rev(1:50)

c8<-c(10,19,7)
c9<-rep(c8,15)
c10<-c(1,2,5,6)
c11<-rep(c10,8)
  
c7<-seq(3.1,6,0.1)
y1<-log(c7)*sin(c7)

c12<-(0:100)
c13<-sample(c12,90,FALSE)
n1<-mean(c13)
c14<-sample(c12,90,TRUE)
n2<-mean(c14)

n3<-0
for(a in 1:20){
  for(b in 1:15){
    n3<-n3+exp(sqrt(a))*log(a^5)/(5+cos(a)*sin(b))
  }}

n4<-0
for(i in 1:20){
  for(j in 1:i){
    n4<-n4+ (exp(sqrt(i))*log(i^5))/(5+exp(i*j)*cos(i)*sin(j))
  }}

y2<-exp(c7)*cos(c7)

#Exercise 4
c15<-c(0:999)
xVec<-sample(c15,1000,TRUE)
yVec<-sample(c15,1000,TRUE)
zVec<-yVec[2:1000]-xVec[1:999]
wVec<-sin(yVec[1:999])/cos(xVec[2:1000])
subX<-xVec[xVec >= 200]
indexy<-which(yVec >= 600)

#Exercise 5
A1<-cbind(c(1,5,-2),c(1,2,-1),c(3,6,-3))
B<-A1%*%A1%*%A1
c16<-A1[,1]+A1[,3]
C<-cbind(A1,c16)
A1[3,]<-A1[1,]+A1[2,]
n5<-rowMeans(A1)
n6<-colMeans(A1)

c17<-c(2,1,3)
c18<-c(1,1,1)
c19<-c(1,3,2)
A2<-rbind(c17,c18,c19)
sol1<-solve(A2,c(10,6,13))

#Exercise 6
sum2<-0
fun1<-function(a,n){
  for (i in 1:n) {
    sum2<-sum2+a^i/i
  }
  return(sum2)
}
fun1(2,10)

fun2<-function(x){
  if(x<0) {
    fx<-x^2+2*x+abs(x)
  }
  else if(0<=x&x<2){
    fx<-x^2+3+log(1+x)
  }
  else {
    fx<-x^2 + 4*x - 14
  }
  return(fx)
}
fun2(-3)
fun2(0)
fun2(3)

#Exercise 7
v1<-sample(c(1:20),36,TRUE)

v12<-v1[-1]
v13<-v1[2:36]
v2<-v1>5
v3<-as.integer(v2)
A3<-matrix(v1,nrow=6,ncol=6,byrow = TRUE)

c20<-c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))
c21<-which(is.na(c20)|is.infinite(c20))
c22<-c20[-c21]

#Exercise 8
install.packages("AER")
library(AER)
data("GSOEP9402", package = "AER")
dat<-GSOEP9402
typeof(dat)
dim(dat)
names(dat)

install.packages("ggplot2")
library(ggplot2)
mean_income<-aggregate(dat$income,list(dat$year),mean)
plot(mean_income$Group.1,mean_income$x,type = "l")
incomebyyear <- dat %>% group_by(year) %>% summarise(income=mean(income))
ggplot(incomebyyear,aes(x=year,y=income))

incomebygender<- dat %>% group_by(gender) %>% summarise(income=mean(income))
incomebyschool<- dat %>% group_by(school) %>% summarise(income=mean(income))
incomebymemployment<- dat %>% group_by(memployment) %>% summarise(income=mean(income))
incomedif<-c('male-female'=incomebygender$income[[1]]-incomebygender$income[[2]], 'gymnasium-hauptschule'=incomebyschool$income[[3]]-incomebyschool$income[[1]], 'realschule-hauptschule'=incomebyschool$income[[2]]-incomebyschool$income[[1]],'none-fulltime'=incomebymemployment$income[[3]]-incomebymemployment$income[[1]], 'none-parttime'= incomebymemployment$mean_income[[2]]-incomebymemployment$mean_income[[1]])

#Exercise 9
data("CASchools", package = "AER")
dat1<-CASchools
dat1$school= factor(dat1$school)
dat1$district= factor(dat1$district)
reg1<-lm(read~district+school+county+grades+students+teachers+ calworks+ lunch+ computer+ expenditure+income+english,data = dat1)

formula=y~x.lm(formula)
reg2<-lm(read ~ .-school, dat1[1:200,])

#Exercise 10
library(actuar)
lu<-rpareto(200,1,1)
n7<-length(lu[lu >= 10])
lu10<-which(lu>10)
lu[lu10]<-rlogis(26,6.5,0.5)

library(truncnorm)
de<-rnorm(200,1,2)
de<-log(de)
dem<-which(de<0|is.nan(de))
de[dem]<-rtruncnorm(94,a=0,mean=1,sd=1)

orig<-runif(200,0,1)
dest<-runif(200,0,1)

hist<-matrix(runif(200*200,0,1),nrow=200,ncol=200)
dist<-matrix(runif(200*200,0,1),nrow=200,ncol=200)

su1<-function(j,l){
  sujl<-log(orig[j] + dest[l] + dist[j,l])/(1+log(orig[j]+dest[l]+dist[j,l]))
  return(sujl)
}
su<-matrix(nrow = 200,ncol=200)
for (j in 1:200) {
  for (l in 1:200) {
    su[j,l]=su1(j,l)
  }
}

se1<-function(j,l){
  sejl<-exp(orig[j] + dest[l] + hist[j,l])/(1+exp(orig[j]+dest[l]+hist[j,l]))
  return(sejl)
}
se<-matrix(nrow = 200,ncol=200)
for (j in 1:200) {
  for (l in 1:200) {
    se[j,l]=se1(j,l)
  }
}

r<-0.05
qjl<-function(l,j,w){
  one<-(r+de[j])/(r+de[l])*w
  two<-lu[j]*log(w)
  three<-lu[l]*log(w)
  four<-(r+de[j])/(r+de[l])*(sum(su)-sum(diag(su)))
  five<-(sum(su)-sum(diag(su)))
  six<-(r+de[j])/(r+de[l])*(sum(se)-sum(diag(se)))
  seven<-(sum(se)-sum(diag(se)))
  q<-one+two-three+four-five+six-seven
  return(q)
}

qjlm<-matrix(nrow = 200,ncol=200)
for (l in 1:200) {
  for (j in 1:200) {
    qjlm[j,l]=qjl(j,l,9245)
  }
}

gridw = seq(9100,55240,length.out=50)

array<-array(dim=c(200,200,50))
for (l in 1:200) {
  for (j in 1:200) {
    for (k in gridw) {
    array[l,j,w]=qjl(l,j,w)
  }
  }
  }

#Exercise 11
vect<-c(1,2,3)
is.array(vect)  
is.vector(vect)  
is.matrix(vect)

x0<-rnorm(1000)
table(x0>0)[[2]]
table(x0>1)[[2]]
table(x0>2)[[2]]
table(x0>0.5)[[2]]
table(x0<0)[[2]]
table(x0>-1)[[2]]

library(Hmisc)
x1<-cut2(runif(100,0,1),g=10)
levels(x1)<-paste("q",1:10,sep="")
is.factor(x1)

table(x1=="q1")[[2]] == 10 

as.numeric(x1)

rand<-rnorm(1000)

pidx<-which(rand>0)

w1<-rand[which(rand>0)]
w2<-subset(rand,rand>0)
w3<-rand[pidx]

#Exercise 12
sum1<-0
for (i in 1:400) {
  sum1<-sum1+i^2
}

sum2<-0
for (i in 1:249) {
  sum2<-sum2+i*(i+1)
}

crra<-function(c,theta) {
  if (0.97 <= theta & theta <=1.03) {
    op<-log(c^(1-theta)/(1-theta))
  }
  else {
    op<-c^(1-theta)/(1-theta)
  }
  return(op)
}


fact<-function(n){
  return(prod(n))
}

#Exercise 13
m<-matrix(c(rnorm(20,0,10), rnorm(20,-1,10)), nrow = 20, ncol = 2)
apply(m, MARGIN=1, FUN=mean)
apply(m, MARGIN=2, FUN=mean)
apply(m, MARGIN=1, FUN=min)
apply(m, MARGIN=2, FUN=min)
apply(m, MARGIN=1, FUN=max)
apply(m, MARGIN=2, FUN=max)
apply(m, MARGIN=1, FUN=sd)
apply(m, MARGIN=2, FUN=sd)

data("iris", package = "datasets")
diris<-iris
lbys<-iris %>% group_by(Species) %>% summarise(width=mean(Sepal.Length))
wbys<-iris %>% group_by(Species) %>% summarise(width=log(mean(Sepal.Width)))

y1 = NULL; for (i in 1:100) y1[i]=exp(i)
y2 = exp(1:100)
y3 = sapply(1:100,exp)

#Exercise 14
x<-rnorm(10000)
summary(x)
dsummary<-function(vec){
  min<-summary(vec)[1]
  fd<-quantile(vec,prob=c(0,1,0.9)[1])
  fc<-summary(x)[2]
  median<-summary(vec)[3]
  mean<-summary(vec)[4]
  nd<-quantile(vec,prob=c(0,1,0.9)[2])
  tq<-summary(vec)[5]
  max<-summary(vec)[6]
  return(c(min,fd,fc,median,mean,nd,tq,max))
}

dnorm(0.5, mean=2, sd=0.25)
pnorm(2.5,mean=2,sd=0.25)
qnorm(0.95,mean=2,sd=0.25)

dt(0.5, 5)
pt(2.5, 5)
qt(0.95,5)

library(actuar)
dpareto(0.5,3,1)
ppareto(0.5,3,1)
qpareto(0.5,3,1)

#Exercise 15
v<-rnorm(100,2, 5)
n<-length(v)
m<-mean(v)
s2<-var(v)
library(moments)
sk<-skewness(v)
ku<-kurtosis(v)

#Exercise 16
data<-rbeta(10000,2,1)

X<-matrix(data,nrow = 1000,ncol = 10)

σ<-0.5
beta<-rgamma(10,2,1)

eps<-rnorm(1000)

Y<-X%*% beta + sqrt(σ)*eps

betahat<-solve(t(X)%*%X)%*%t(X)%*%Y

Yhat<-X %*% betahat
epshat<-Y-Yhat

hist(epshat, col="gray")
plot(density(epshat))

ss<-t(epsHat) %*% epsHat / (1000-10-1)
vv<-σ* solve(t(X)%*%X)

param = cbind(beta,sqrt(v)) 
fit0 = lm(Y~0+X)
summary(fit0)

confint(fit0)

sigmasq = 0.01
Y = X%*% beta + sqrt(sigmasq)*eps
fit1 = lm(Y~0+X)
confint(fit1)