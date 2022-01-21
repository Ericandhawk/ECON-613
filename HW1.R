#Exercise 1
library(readr)
library(datasets)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
dathh2007 <- fread("A1/dathh/dathh2007.csv")
noh2007<-length(unique(dathh2007$idmen))

dathh2005 <- fread("A1/dathh/dathh2005.csv")
nocwk<-table(dathh2005$mstatus)[2]

datind2008 <- fread("A1/datind/datind2008.csv")
noi2008<-length(datind2008$idind)

datind2016 <- fread("A1/datind/datind2016.csv")
noia2016<-table(datind2016$age>=25&datind2016$age<=35)[2] 

datind2009 <- fread("D:/ECON 613/A1/datind/datind2009.csv")
tablegp<-table(datind2009$gender,datind2009$profession)
 
datind2005 <- fread("D:/ECON 613/A1/datind/datind2005.csv") #For year 2005
mwage2005<-summary(datind2005$wage)[4]
sdwage2005<-sd(datind2005$wage,TRUE)
fqwage2005<-quantile(datind2005$wage,prob=c(0,1,0.9)[1],TRUE)
nqwage2005<-quantile(datind2005$wage,prob=c(0,1,0.9)[2],TRUE)
inratio2005<-nqwage2005/fqwage2005
Gini <- function (x,na.rm = TRUE) 
{
  x <- as.numeric(na.omit(x))
  n <- length(x)
  x <- sort(x)
  G<- sum(x * 1:n)
  Gini<- 2 * G/sum(x) - (n + 1)
  return(Gini/n)
}
gini2005<-Gini(datind2005$wage)

datind2019 <- fread("D:/ECON 613/A1/datind/datind2019.csv")# For year 2019
mwage2019<-summary(datind2019$wage)[4]
sdwage2019<-sd(datind2019$wage,TRUE)
fqwage2019<-quantile(datind2019$wage,prob=c(0,1,0.9)[1],TRUE)
nqwage2019<-quantile(datind2019$wage,prob=c(0,1,0.9)[2],TRUE)
inratio2019<-nqwage2019/fqwage2019
gini2019<-Gini(datind2019$wage)

datind2010<-fread("D:/ECON 613/A1/datind/datind2010.csv")
table(datind2010$age)
ggplot(datind2010) +
  aes(x = age) +
  geom_histogram(bins = 30L, fill = "#0C4C8A") +
  theme_minimal()
ggplot(datind2010) +
 aes(x = age, fill = gender) +
 geom_histogram(bins = 30L,position="dodge") +
 scale_fill_manual(values = c(Female = "#FA1000", 
Male = "#000000")) +
 theme_minimal()

#First find out how many people each household has, then add with those household lives in Paris.
datind2011 <- fread("D:/ECON 613/A1/datind/datind2011.csv")
dathh2011 <- fread("D:/ECON 613/A1/dathh/dathh2011.csv")
pbyhh<- datind2011 %>% group_by(idmen) %>% summarise(people=length(idmen)) #Getting how many people each household has.
dathh2011$people<-pbyhh$people 
dathh2011$location<-as.factor(dathh2011$location)
nparis<-tapply(dathh2011$people, dathh2011$location, sum)[1] 

#Exercise 2
#read and create appended household data as .csv
dir<-"D:/ECON 613/A1/dathh/" 
file_list<-list.files(path = dir, pattern = "*.csv$",recursive = TRUE,full.names = TRUE)   
dathh = paste(dir,"dathh.csv")
for(i in 1:length(file_list)){
  df = fread(file = file_list[i],encoding = 'UTF-8')
  write_csv(df,path =dathh,append = TRUE, col_names = TRUE)
}
#fread and read_csv each has some nice properties, so I combine them.
dathh1 <- fread("D:/ECON 613/A1/dathh/ dathh.csv",na.strings = '')  
dathh<- read_csv("D:/ECON 613/A1/dathh/ dathh.csv")
dathh$idmen<-dathh1$idmen
#read and create appended individual data as ".csv"
dir1<-"D:/ECON 613/A1/datind/" 
file_list1<-list.files(path = dir1, pattern = "*.csv$",recursive = TRUE,full.names = TRUE)   
datind = paste(dir1,"datind.csv")
for(i in 1:length(file_list1)){
  df1 = fread(file = file_list1[i],encoding = 'UTF-8')
  write_csv(df1,path =datind,append = TRUE, col_names = TRUE)
}
#fread and read_csv each has some nice properties, so I combine them.
datind1 <- fread("D:/ECON 613/A1/datind/ datind.csv",na.strings = '')
datind <- read_csv("D:/ECON 613/A1/datind/ datind.csv")
datind$idind<-datind1$idind
datind$idmen<-datind1$idmen
#Merge the above 2 datasheets
data<-merge(dathh, datind, by = c("idmen","year")) 

d1<-data.frame(table(data$idmen,data$year)) #Get how many times each "idmen" appear in each year]
ng4<-d1 %>% group_by(Var2) %>% summarise(ng4=length(which(Freq>4))) 

#find out how many times "Unemployed" appear for unique "idmen"
d2<-data %>% group_by(year) %>% summarise(unemploy=n_distinct(idmen[which(empstat=="Unemployed")]))

#The idea is that in each year, I find the number of people and number of profession in each household.
#If the number of people exceeds the number of profession, then there are at least two people have the same profession.
data1 <- data %>% drop_na(profession)
prof<-data1 %>% group_by(idmen,year) %>% summarise(nprof=length(profession))
dprof<-data1 %>% group_by(idmen,year) %>% summarise(dprof=n_distinct(profession))
pro<-data.frame(prof$idmen,prof$year,prof$nprof,dprof$dprof)
pro$true<-pro$prof.nprof>pro$dprof.dprof
pro$true<-as.numeric(pro$true)
nd<-tapply(pro$true, pro$prof.year, sum) 

d3<-data %>% group_by(year) %>% summarise(nwkids=length(which(mstatus=="Couple, with Kids")))
d4<-data %>% group_by(year) %>% summarise(nparis=length(which(location=="Paris")))

#I firstly find out each year how many people lives in the household that has most members.
d5<-data %>% group_by(idmen,year) %>% summarise(nofp=(length(idmen)))
d6<-d5 %>% group_by(year) %>% summarise(nofp=max(nofp))
m2004<-d5$idmen[which(d5$year==2004&d5$nofp==d6$nofp[1])]
m2005<-d5$idmen[which(d5$year==2005&d5$nofp==d6$nofp[2])]
m2006<-d5$idmen[which(d5$year==2006&d5$nofp==d6$nofp[3])] 
m2007<-d5$idmen[which(d5$year==2007&d5$nofp==d6$nofp[4])]
m2008<-d5$idmen[which(d5$year==2008&d5$nofp==d6$nofp[5])]
m2009<-d5$idmen[which(d5$year==2009&d5$nofp==d6$nofp[6])]
m2010<-d5$idmen[which(d5$year==2010&d5$nofp==d6$nofp[7])]
m2011<-d5$idmen[which(d5$year==2011&d5$nofp==d6$nofp[8])]
m2012<-d5$idmen[which(d5$year==2012&d5$nofp==d6$nofp[9])]
m2013<-d5$idmen[which(d5$year==2013&d5$nofp==d6$nofp[10])]
m2014<-d5$idmen[which(d5$year==2014&d5$nofp==d6$nofp[11])]
m2015<-d5$idmen[which(d5$year==2015&d5$nofp==d6$nofp[12])]
m2016<-d5$idmen[which(d5$year==2016&d5$nofp==d6$nofp[13])]
m2017<-d5$idmen[which(d5$year==2017&d5$nofp==d6$nofp[14])]
m2018<-d5$idmen[which(d5$year==2018&d5$nofp==d6$nofp[15])]
m2019<-d5$idmen[which(d5$year==2019&d5$nofp==d6$nofp[16])]
l1<-list(m2004,m2005,m2006,m2007,m2008,m2009,m2010,m2011,m2012,m2013,m2014,m2015,m2016,m2017,m2018,m2019)
names(l1) <- 2004:2019

#Only keep the data with year 2010 and 2011, and if idmen appears two times, then it is in both years.
d11<-subset(d1, Var2=="2010" | Var2=="2011")
d12<-subset(d11,Freq!=0)
d13<-data.frame(table(d12$Var1))
nboth<-n_distinct(which(d13$Freq==2))

#Exercise 3
d31<-subset(d1,d1$Freq!=0)
d31$Var2<-as.character(d31$Var2)
d31$Var2<-as.numeric(d31$Var2)
d31$Var1<-as.character(d31$Var1)
enter<-d31 %>% group_by(Var1) %>% summarise(enteryear=min(Var2))
exit<- d31 %>% group_by(Var1) %>% summarise(exityear=max(Var2))
duration<-d31 %>% group_by(Var1) %>% summarise(duration=length(Var1))
yearspent<-data.frame(idmen=enter$Var1,enteryear=enter$enteryear,exityear=exit$exityear,duration=duration$duration)
tableyear<-table(yearspent$duration)
ggplot(yearspent) +
  aes(x = duration) +
  scale_x_continuous(breaks=1:9)+
  geom_histogram(bins = 30L, fill = "#4682B4") +
  labs(x = "Number of people", y = "years spent") +
  theme_minimal()

data$datenttrue<-data$datent==data$year
data$datenttrue<-as.numeric(data$datenttrue)
share<-data %>% group_by(year) %>% summarise(sofd=mean(datenttrue,na.rm = TRUE))
share %>%
  filter(!is.na(year)) %>%
  filter(!is.na(sofd)) %>%
  ggplot() +
  aes(x = year, y = sofd) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()
data32<- data[1:10,] %>% select(c("idmen","year","idind","datenttrue"))

data$movetrue<-data$myear==data$year|data$move==2
data$movetrue<-as.numeric(data$movetrue)
data$movetrue[is.na(data$movetrue)]<-0
share1<-data %>% group_by(year) %>% summarise(sofm=mean(movetrue,na.rm = TRUE))
share1 %>%
  filter(!is.na(year)) %>%
  ggplot() +
  aes(x = year, y = sofm) +
  geom_line(size = 1L, colour = "#112446") +
  scale_x_continuous(breaks=2004:2019)+
  theme_minimal() 
data33<-data[1:10,] %>% select(c("idmen","year","idind","movetrue"))
#In order to mix the plot, I create a new dataframe.
share2<-matrix(nrow=32,ncol=3)
share2[,1]<-rep(2004:2019,2)
share2[1:16,2]<-rep("1",16)
share2[17:32,2]<-rep("2",16)
c1<-share$sofd[1:16]
c2<-share1$sofm[1:16]
share2[1:16,3]<-c1
share2[17:32,3]<-c2
share2<-data.frame(share2)
share2$X3<-as.numeric(share2$X3)
ggplot(share2) +
  aes(x = X1, fill = X2, weight = X3) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 2) +
  labs(
    x = "Year",
    y = "Share of Migration",
    caption = "1: Migration defined in 3.2 2: Migration defined in 3.3",
    fill = "Category of Migration"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 12L),
    axis.title.y = element_text(size = 14L),
    axis.title.x = element_text(size = 14L)
  ) 

#we use the migration defined as in 3.3
data351 <- data %>% drop_na(profession)
data351$profession <-as.numeric(data351$profession) 

#I select data in two neighboring years and people who migrate in the latter year.
#I change profession to numeric variable and for any individual, the difference not equal to 0 means change of profession.
#After finding individual, I judge whether they come from the same household, by comparing the first 16 digit of "idind"
ncp<-matrix(nrow=15,ncol=2)
ncp[1:15,1]<-2005:2019
for (i in 2004:2018) {
  data35<-subset(data,year==i|year==i+1&movetrue==1)
  data35$empstat<-as.factor(data35$empstat)
  data35$empstat<-as.numeric(data35$empstat)
  a<-data35 %>% group_by(idind) %>% summarise(nc=(profession[2]-profession[1]),np=empstat[2]-empstat[1])
  a1<-subset(a,nc!=0|np!=0)
  a1$idind<-str_sub(a1$idind, start = 1, end = 16)
  ncp[i-2003,2]<-n_distinct(a1$idind)
}

data35<-subset(data,year==2004|year==2005&movetrue==1)
data35$empstat<-as.factor(data35$empstat)
data35$empstat<-as.numeric(data35$empstat)
a<-data35 %>% group_by(idind) %>% summarise(nc=(profession[2]-profession[1]),np=empstat[2]-empstat[1])
a1<-subset(a,nc!=0|np!=0)
a1$idind<-str_sub(a1$idind, start = 1, end = 16)
ncp[i-2003,2]<-n_distinct(a1$idind)

#Exercise 4
da4<-data.frame(table(data$idind,data$year))
da4$Var2<-as.character(da4$Var2)
da4$Var2<-as.numeric(da4$Var2)
rofatt<-matrix(nrow=15,ncol=2)
rofatt[1:15,1]<-2005:2019
#in any given two years, after group_by(idmen), left with a vector with 2 numbers. only structure with (1,0) fits the definition of attrition.

for (i in 2004:2018) {
  da444<-subset(da4,Var2==i|Var2==i+1)
  att<-da444 %>% group_by(Var1) %>% summarise(att=Freq[2]-Freq[1])
  rofatt[i-2003,2]<-length(which(att$att==-1))/length(which(da444$Var2==i&da444$Freq==1))
}

      