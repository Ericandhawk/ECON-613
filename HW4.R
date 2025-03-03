#============================================================================================
#Basic Settings
#============================================================================================
setwd("Downloads/Econ 613/A4/Data/")
setwd("D:/ECON 613/A4/Data/")
options(scipen = 200)
#============================================================================================
#Import Packages
#============================================================================================
library(margins)
library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(mlogit)
library(nnet)
library(esquisse)
library(survival)
library(panelr)
library(micEcon)
library(plm)
library(sampleSelection)
library(AER)
#============================================================================================
#Import Datasets for Q1-Q3
dat_a4 <- read_csv("dat_A4.csv")
#============================================================================================
#Exercise 1 
#============================================================================================
#Create Variables
#============================================================================================
#Work Experience
dat_a4$age <- 2019 - dat_a4$KEY_BDATE_Y_1997 - 1
dat_a4$work_exp <- rowSums(dat_a4[,18:28],na.rm = 'TRUE')/52
#Parents Education: Change 95 to 0
c1 <- which(dat_a4$CV_HGC_BIO_DAD_1997==95)
c2 <- which(dat_a4$CV_HGC_BIO_MOM_1997==95)
c3 <- which(dat_a4$CV_HGC_RES_DAD_1997==95)
c4 <- which(dat_a4$CV_HGC_BIO_MOM_1997==95)
dat_a4$CV_HGC_BIO_DAD_1997[c1] <- 0
dat_a4$CV_HGC_BIO_MOM_1997[c2] <- 0
dat_a4$CV_HGC_RES_DAD_1997[c3] <- 0
dat_a4$CV_HGC_BIO_MOM_1997[c4] <- 0
#Personal Education
dat_a4 <- dat_a4 %>% mutate(edu = case_when(dat_a4$YSCH.3113_2019 == 1 ~ "0",
                                                                dat_a4$YSCH.3113_2019 == 2 ~ "12",
                                                                dat_a4$YSCH.3113_2019 == 3 ~ "12",
                                                                dat_a4$YSCH.3113_2019 == 4 ~ "14",
                                                                dat_a4$YSCH.3113_2019 == 5 ~ "16",
                                                                dat_a4$YSCH.3113_2019 == 6 ~ "18",
                                                                dat_a4$YSCH.3113_2019 == 7 ~ "21",
                                                                dat_a4$YSCH.3113_2019 == 8 ~ "21"))
dat_a4$edu <- as.numeric(dat_a4$edu)
#============================================================================================
#Visualization 1.3.1
#============================================================================================
#For Visualization Convenience:
dat_a4$age <- as.factor(dat_a4$age)
dat_a4$KEY_SEX_1997 <- as.factor(dat_a4$KEY_SEX_1997)
dat_a4$CV_BIO_CHILD_HH_U18_2019 <- as.factor(dat_a4$CV_BIO_CHILD_HH_U18_2019)
#============================================================================================
#Prepare Data 1.3.1
#============================================================================================
dat_a4[,30][is.na(dat_a4[,30])] <- 0
dat_a4_ig0 <- subset(dat_a4,dat_a4$YINC_1700_2019 > 0)
income_age <- dat_a4_ig0 %>% group_by(age) %>% summarise(income = mean(YINC_1700_2019))
income_gender <- dat_a4_ig0 %>% group_by(KEY_SEX_1997) %>% summarise(income = mean(YINC_1700_2019))
dat_a4_male <- subset(dat_a4_ig0,KEY_SEX_1997==1)
dat_a4_female <- subset(dat_a4_ig0,KEY_SEX_1997==2)
income_child <- dat_a4_ig0 %>% group_by(CV_BIO_CHILD_HH_U18_2019) %>% summarise(income = mean(YINC_1700_2019))
income_child <- income_child[1:10,] 
#============================================================================================
#Visualization 1.3.1
#============================================================================================
ggplot(income_age) +
  aes(x = age, weight = income) +
  geom_bar(fill = "#112446") +
  labs(
    y = "average income",
    title = "Average Income with Age"
  ) +
  theme_light() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))
ggplot(dat_a4_male) +
  aes(x = YINC_1700_2019) +
  geom_histogram(bins = 30L, fill = "#4682B4") +
  labs(
    x = "income",
    y = "density",
    title = "Male Income Density"
  ) +
  theme_minimal()
ggplot(dat_a4_female) +
  aes(x = YINC_1700_2019) +
  geom_histogram(bins = 30L, fill = "#4682B4") +
  labs(
    x = "income",
    y = "density",
    title = "Feale Income Density"
  ) +
  theme_minimal()
ggplot(income_gender) +
  aes(x = KEY_SEX_1997, fill = KEY_SEX_1997, weight = income) +
  geom_bar() +
  scale_fill_manual(
    values = c(`1` = "#F8766D",
               `2` = "#FF61C3")
  ) +
  labs(
    y = "average income",
    title = "Average Income with Gender"
  ) +
  theme_light() +
  theme(plot.title = element_text(size = 15L))
ggplot(income_child) +
  aes(
    x = CV_BIO_CHILD_HH_U18_2019,
    fill = CV_BIO_CHILD_HH_U18_2019,
    weight = income
  ) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "number of children",
    y = "average income",
    title = "Average Income with Number of Children"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, hjust = 0.5))
#============================================================================================
#Prepare data 1.3.2
#============================================================================================
s0_age <- dat_a4 %>% group_by(age) %>% summarize(s0=length(which((YINC_1700_2019==0)=='TRUE'))/length(YINC_1700_2019)) 
s0_gender <- dat_a4 %>% group_by(KEY_SEX_1997) %>% summarize(s0=length(which((YINC_1700_2019==0)=='TRUE'))/length(YINC_1700_2019)) 
#============================================================================================
#recode marital status to make it binary
#============================================================================================
dat_a4$CV_MARSTAT_COLLAPSED_2019[which(dat_a4$CV_MARSTAT_COLLAPSED_2019!=1)]<-0 
s0_child_marital <- dat_a4 %>% group_by(CV_BIO_CHILD_HH_U18_2019,CV_MARSTAT_COLLAPSED_2019) %>% summarize(s0=length(which((YINC_1700_2019==0)=='TRUE'))/length(YINC_1700_2019))  
s0_child_marital <- subset(s0_child_marital,CV_BIO_CHILD_HH_U18_2019!='NA'&CV_MARSTAT_COLLAPSED_2019!='NA')
s0_child_marital$status <- paste(s0_child_marital$CV_BIO_CHILD_HH_U18_2019,s0_child_marital$CV_MARSTAT_COLLAPSED_2019,sep = ',')
#============================================================================================
#Visualization 1.3.2
#============================================================================================
ggplot(s0_age) +
  aes(x = age, fill = age, weight = s0) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
ggplot(s0_gender) +
  aes(x = KEY_SEX_1997, fill = KEY_SEX_1997, weight = s0) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(y = "share of 0") +
  theme_minimal()
ggplot(s0_child_marital) +
  aes(x = status, fill = status, weight = s0) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
#============================================================================================
#Exercise 2
#============================================================================================
#Prepare Data 2
#============================================================================================
dat_a4_nna<-subset(dat_a4,dat_a4$YSCH.3113_2019!='NA'&dat_a4$CV_MARSTAT_COLLAPSED_2019!='NA'&CV_HGC_RES_MOM_1997!='NA')
dat_a4_nna$d1 <- 0
dat_a4_nna$d1[which(dat_a4_nna$YINC_1700_2019>0)] <- 1
dat_a4_nna$KEY_SEX_1997 = as.numeric(dat_a4_nna$KEY_SEX_1997)
dat_a4_nna$age = as.numeric(as.character(dat_a4_nna$age))
dat_a4_nna_ig0 <- subset(dat_a4_nna,dat_a4_nna$YINC_1700_2019 > 0)
#============================================================================================
#OLS, Includes variables: age/gender/exper/marital status/edu/bio mom edu
#============================================================================================
ols_1 <- lm(log(YINC_1700_2019)~age+KEY_SEX_1997+work_exp+CV_MARSTAT_COLLAPSED_2019+edu+CV_HGC_RES_MOM_1997,data=dat_a4_nna_ig0)
summary(ols_1)
#Heckman Selection Model
#============================================================================================
#Step 1: Probit Estimation of Probability
reg1 <- glm(d1~age+KEY_SEX_1997+KEY_RACE_ETHNICITY_1997+work_exp+CV_MARSTAT_COLLAPSED_2019+edu+CV_HGC_RES_MOM_1997,family = binomial(link = "probit"),data = dat_a4_nna)
summary(reg1)
#Inverse Mills Ratio
imr <- dnorm(reg1$linear.predictors)/pnorm(reg1$linear.predictors)
dat_a4_nna$imr <- imr
dat_a4_nna_ig0 <- subset(dat_a4_nna,dat_a4_nna$YINC_1700_2019 > 0)
#Step 2: Include Inverse Mills Ratio as a Regressor
ols_heckman <- lm(log(YINC_1700_2019)~age+KEY_SEX_1997+work_exp+edu+CV_MARSTAT_COLLAPSED_2019+CV_HGC_RES_MOM_1997+imr,data = dat_a4_nna_ig0)
summary(ols_heckman)
#Package Results
package_heckit=selection(d1~age+KEY_SEX_1997+KEY_RACE_ETHNICITY_1997+work_exp+CV_MARSTAT_COLLAPSED_2019+edu+CV_HGC_RES_MOM_1997,
                 log(YINC_1700_2019)~age+KEY_SEX_1997+work_exp+CV_MARSTAT_COLLAPSED_2019+edu+CV_HGC_RES_MOM_1997,
                 method = "2step",
                 data = dat_a4_nna)
summary(package_heckit)
#============================================================================================
#Exercise 3
#============================================================================================
ggplot(dat_a4_ig0) +
  aes(x = YINC_1700_2019) +
  geom_histogram(bins = 30L, fill = "#4682B4") +
  labs(
    x = "income",
    y = "density",
    title = "Histogram of Income"
  ) +
  theme_light()
#============================================================================================
#Use Tobit Model: Include variables: age/gender/experience/self education/marital status
#============================================================================================
#Prepare Data
dat_a4_nna_ig0$inter <- 1
o_x1 = dat_a4_nna_ig0$inter
o_x2 = dat_a4_nna_ig0$KEY_SEX_1997
o_x3 = dat_a4_nna_ig0$age
o_x4 = dat_a4_nna_ig0$work_exp
o_x5 = dat_a4_nna_ig0$YSCH.3113_2019
o_x6 = dat_a4_nna_ig0$CV_MARSTAT_COLLAPSED_2019
y    = dat_a4_nna_ig0$YINC_1700_2019
#Create Tobit Dummy
dat_a4_nna_ig0$d2 <- 0
dat_a4_nna_ig0$d2[which(dat_a4_nna_ig0$YINC_1700_2019<100000)] <- 1
d2 <- dat_a4_nna_ig0$d2
#Package Results
reg_tobit <- tobit(y ~ o_x2 + o_x3 + o_x4 + o_x5 + o_x6,left=-Inf,right = 100000)
summary(reg_tobit)
par <- as.vector(c(reg_tobit$coefficients,10.47996))
#Tobit Estimation
tobit_flike = function(par,x1,x2,x3,x4,x5,x6,d2,y){
  yhat = par[1]*x1 + par[2]*x2 + par[3]*x3 + par[4]*x4 + par[5]*x5 + par[6]*x6
  residual = y - yhat
  standardization = (100000-yhat)/exp(par[7])
  like = d2*log(dnorm(residual/exp(par[7]))/exp(par[7])) + (1-d2)*log(1 - pnorm(standardization))
  return(-sum(like))
}

start_2 <- par + runif(7,-10,10)
res_2 <- optim(start_2,fn=tobit_flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=o_x1,x2=o_x2,x3=o_x3,x4=o_x4,x5=o_x5,x6=o_x6,d2=d2,y=y,hessian=TRUE)
res_2$par
summary(reg_tobit)
#============================================================================================
#Not correcting for censored data
#============================================================================================
reg_ols <- lm(y ~ o_x2 + o_x3 + o_x4 + o_x5 + o_x6)
summary(reg_ols)
#============================================================================================
#Exercise 4
#============================================================================================
#Prepare Data
#============================================================================================
#Import Data and Rename Variables
#============================================================================================
dat_a4_panel <- read_csv("dat_A4_panel.csv")
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1998=CV_HIGHEST_DEGREE_9899_1998)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1999=CV_HIGHEST_DEGREE_9900_1999)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2000=CV_HIGHEST_DEGREE_0001_2000)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2001=CV_HIGHEST_DEGREE_0102_2001)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2002=CV_HIGHEST_DEGREE_0203_2002)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2003=CV_HIGHEST_DEGREE_0304_2003)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2004=CV_HIGHEST_DEGREE_0405_2004)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2005=CV_HIGHEST_DEGREE_0506_2005)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2006=CV_HIGHEST_DEGREE_0607_2006)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2007=CV_HIGHEST_DEGREE_0708_2007)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2008=CV_HIGHEST_DEGREE_0809_2008)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2009=CV_HIGHEST_DEGREE_0910_2009)
dat_a4_panel <- dat_a4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2010=CV_HIGHEST_DEGREE_1011_2010)
#============================================================================================
#Convert Wide to Long
dat_a4_panel_long <- long_panel(dat_a4_panel, prefix='_', begin  = 1997, end = 2019, label_location = "end")
dat_a4_panel_long <- subset(dat_a4_panel_long,wave!='2012' & wave!='2014' & wave!='2016' & wave!='2018')
#============================================================================================
#Create Experience, Education, and Age Variable
#============================================================================================
#Work Experience
#============================================================================================
dat_a4_panel_long <- select(dat_a4_panel_long,c(1:18,22:30))
a<-as.data.frame(dat_a4_panel_long[,c(10:16,20:27)])
a[is.na(a)]<-0
for (i in 3:17) {
  a[,i]<-as.numeric(a[,i])
}
a$work_exp <- rowSums(a[,3:17])/52
dat_a4_panel_long$work_exp <- a$work_exp
#============================================================================================
#Age
#============================================================================================
dat_a4_panel_long$age <- dat_a4_panel_long$wave - dat_a4_panel_long$KEY_BDATE_Y
#============================================================================================
#Marital Status: Recategorize
dat_a4_panel_long$CV_MARSTAT_COLLAPSED[which(dat_a4_panel_long$CV_MARSTAT_COLLAPSED!=1)] <- 0
#============================================================================================
#Between Estimator: gender/exper/edu/marital status
#============================================================================================
data <- as.matrix(dat_a4_panel_long)
data <- as.data.frame(data)
data$id <- as.numeric(data$id)
data$`YINC-1700` <- as.numeric(data$`YINC-1700`)
data$CV_HIGHEST_DEGREE_EVER_EDT <- as.numeric(data$CV_HIGHEST_DEGREE_EVER_EDT)
data$work_exp <- as.numeric(data$work_exp)
data$CV_MARSTAT_COLLAPSED <- as.numeric(data$CV_MARSTAT_COLLAPSED)
data <- select(data,id,wave,'YINC-1700',CV_HIGHEST_DEGREE_EVER_EDT,work_exp,CV_MARSTAT_COLLAPSED)
#============================================================================================
#create education variables
#============================================================================================
data <- data %>% mutate(edu = case_when(data$CV_HIGHEST_DEGREE_EVER_EDT == 0 ~ "0",
                                                            data$CV_HIGHEST_DEGREE_EVER_EDT == 1 ~ "12",
                                                            data$CV_HIGHEST_DEGREE_EVER_EDT == 2 ~ "12",
                                                            data$CV_HIGHEST_DEGREE_EVER_EDT == 3 ~ "14",
                                                            data$CV_HIGHEST_DEGREE_EVER_EDT == 4 ~ "16",
                                                            data$CV_HIGHEST_DEGREE_EVER_EDT == 5 ~ "18",
                                                            data$CV_HIGHEST_DEGREE_EVER_EDT == 6 ~ "21",
                                                            data$CV_HIGHEST_DEGREE_EVER_EDT == 7 ~ "21"))
data$edu <- as.numeric(data$edu)
data <- na.omit(data)
#============================================================================================
#Data for 4.1
#============================================================================================
data_41 <- data %>% group_by(id) %>% summarize(income=mean(`YINC-1700`,na.rm = TRUE),
                                               exper=mean(work_exp,na.rm = TRUE),
                                               edu=mean(edu,na.rm = TRUE),
                                               ms=mean(CV_MARSTAT_COLLAPSED,NA.RM=TRUE))
#============================================================================================
#Estimate
#============================================================================================
panel_between_estimator <- lm(income~edu+exper+ms,data = data_41)
summary(panel_between_estimator)
#============================================================================================
#Package Results
#============================================================================================
try=plm(data$`YINC-1700` ~ edu+work_exp+CV_MARSTAT_COLLAPSED,data=data,model = "between")
summary(try)
#============================================================================================
#Within Estimator: exper/edu/marital status
#============================================================================================
data$meanincome <- ave(data$`YINC-1700`, data$id, FUN=function(x)mean(x, na.rm=T))
data$meanedu <- ave(data$edu, data$id, FUN=function(x)mean(x, na.rm=T)) 
data$meanexper <- ave(data$work_exp, data$id, FUN=function(x)mean(x, na.rm=T)) 
data$meanms<- ave(data$CV_MARSTAT_COLLAPSED, data$id, FUN=function(x)mean(x, na.rm=T))

data$d_income <- data$`YINC-1700` - data$meanincome
data$d_edu <- data$edu - data$meanedu
data$d_exper <- data$work_exp - data$meanexper
data$d_ms <- data$CV_MARSTAT_COLLAPSED - data$meanms

panel_within_estimator <- lm(data$d_income ~ data$d_edu + data$d_exper + data$d_ms)
summary(panel_within_estimator)

try2 = plm(data$`YINC-1700` ~ edu + work_exp + CV_MARSTAT_COLLAPSED,data=data,model = "within")
summary(try2)
#============================================================================================
#First Difference Estimator
#============================================================================================
data_43 <- select(data,id,wave,'YINC-1700',edu,work_exp,CV_MARSTAT_COLLAPSED)
#Create Lag Variables
data_43$l_income <- ave(data$'YINC-1700', data$id, FUN=function(x) dplyr::lag(x))
data_43$l_edu <- ave(data$edu, data$id, FUN=function(x) dplyr::lag(x))
data_43$l_exper <- ave(data$work_exp, data$id, FUN=function(x) dplyr::lag(x))
data_43$l_ms <- ave(data$CV_MARSTAT_COLLAPSED, data$id, FUN=function(x) dplyr::lag(x))
#Create FD Variables
data_43$fd_income <- data_43$`YINC-1700`- data_43$l_income
data_43$fd_edu <- data_43$edu- data_43$l_edu
data_43$fd_exper <- data_43$work_exp- data_43$l_exper
data_43$fd_ms <- data_43$CV_MARSTAT_COLLAPSED- data_43$l_ms
#Regression
panel_fd_estimator <- lm(data_43$fd_income ~ data_43$fd_edu + data_43$fd_exper + data_43$fd_ms)
summary(panel_fd_estimator)
#Package Results
try3 = plm(data$`YINC-1700` ~ edu + work_exp + CV_MARSTAT_COLLAPSED,data=data,model = "fd")
summary(try3)

