# packages 
# install.packages("reshape")
# install.packages("ggpubr")
# install.packages("ggcorrplot")
# install.packages("corrplot")
# install.packages("moments")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("strucchange")
# install.packages("faraway")
# install.packages("lattice")
# install.packages("ggpubr")
# install.packages("farver")

library(reshape)
library(ggpubr)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(moments)
library(ggplot2)
library(tidyverse)
library(strucchange)
require("faraway")
library(faraway)
library(lattice)
library(ggpubr)
library(farver)


# Loading the data
data <- read.csv(file.choose(), header = T)

# preparing the data
data <- data[complete.cases(data),] # in case there are NA values
data[data == "Developed"] <- 1 # handling categorial values
data[data == "Developing"] <- 2
data$Alcohol[data$Alcohol == '5'] <- 4
hist(data$Life.expectancy, xlab = "Life expectancy")

#----------------------------------------<Part A>------------------------------------------------

# QUESTION 3 - how the x variables effect the y variable

data$color <- ifelse(data$Status == "1", "orange", "blue") # difference between developed and developing countries

plot(x=data$Status, y=data$Life.expectancy, xlab="status", ylab="Life expectancy")
plot(x=data$Adult.Mortality, y=data$Life.expectancy, xlab="Adult.Mortality", ylab="Life expectancy", col=data$color)
plot(x=data$infant.deaths, y=data$Life.expectancy, xlab="infant.deaths", ylab="Life expectancy", col=data$color)
plot(x=data$percentage.expenditure, y=data$Life.expectancy, xlab="percentage.expenditure", ylab="Life expectancy", col=data$color)
plot(x=data$Alcohol, y=data$Life.expectancy, xlab="Alcohol", ylab="Life expectancy", col=data$color)
plot(x=data$Hepatitis.B, y=data$Life.expectancy, xlab="Hepatitis.B", ylab="Life expectancy", col=data$color)
plot(x=data$Measles, y=data$Life.expectancy, xlab="Measles", ylab="Life expectancy", col=data$color, axes = )
plot(x=data$BMI, y=data$Life.expectancy, xlab="BMI", ylab="Life expectancy", col=data$color)
plot(x=data$under.five.deaths, y=data$Life.expectancy, xlab="under.five.deaths", ylab="Life expectancy", col=data$color)
plot(x=data$Polio, y=data$Life.expectancy, xlab="Polio", ylab="Life expectancy", col=data$color)
plot(x=data$Total.expenditure, y=data$Life.expectancy, xlab="Total.expenditure", ylab="Life expectancy", col=data$color)
plot(x=data$Diphtheria, y=data$Life.expectancy, xlab="Diphtheria", ylab="Life expectancy", col=data$color)
plot(x=data$HIV.AIDS, y=data$Life.expectancy, xlab="HIV.AIDS", ylab="Life expectancy", col=data$color)
plot(x=data$GDP, y=data$Life.expectancy, xlab="GDP", ylab="Life expectancy", col=data$color)
plot(x=data$Population, y=data$Life.expectancy, xlab="Population", ylab="Life expectancy", col=data$color)
plot(x=data$thinness.9.19.years, y=data$Life.expectancy, xlab="thinness.9.19.years", ylab="Life expectancy", col=data$color)
plot(x=data$thinness.5.9.years, y=data$Life.expectancy, xlab="thinness.5.9.years", ylab="Life expectancy", col=data$color)
plot(x=data$Income.composition.of.resources, y=data$Life.expectancy, xlab="Income.composition.of.resources", ylab="Life expectancy", col=data$color)
plot(x=data$Schooling, y=data$Life.expectancy, xlab="Schooling", ylab="Life expectancy", col=data$color)



# QUESTION 4 - correlation between X variables 

cor_data <- data[, c("Life.expectancy", "Alcohol", "Adult.Mortality", "infant.deaths", "percentage.expenditure", "Hepatitis.B", "Measles", "BMI", "under.five.deaths", "Polio", "Total.expenditure", "Diphtheria", "HIV.AIDS", "GDP", "Population", "thinness.9.19.years", "thinness.5.9.years", "Income.composition.of.resources", "Schooling")]
cor_mat <- cor(cor_data) # cor function 

cor_mat[cor_mat>-0.5 & cor_mat<0.5] <- NA # filter strong relations between varabiles

cor_mat[cor_mat>0.3] <- NA # filter weak relations between varabiles
cor_mat[cor_mat<-0.3] <- NA 

# write.csv(cor_mat, file = "cor_weak.csv")


# QUESTION 5 - Descriptive analysis

summary(data$Adult.Mortality)
summary(data$infant.deaths)
summary(data$percentage.expenditure)
summary(data$Hepatitis.B)
summary(data$Measles)
summary(data$BMI)
summary(data$under.five.deaths)
summary(data$Polio)
summary(data$Total.expenditure)
summary(data$Diphtheria)
summary(data$HIV.AIDS)
summary(data$GDP)
summary(data$Population)
summary(data$thinness.9.19.years)
summary(data$thinness.5.9.years)
summary(data$Income.composition.of.resources)
summary(data$Schooling)
summary(data$Life.expectancy)

sd(data_set$Adult.Mortality)
skewness(data_set$Adult.Mortality)
hist(data_set$Adult.Mortality)

sd(data_set$infant.deaths)
skewness(data_set$infant.deaths)
hist(data_set$infant.deaths)

sd(data_set$percentage.expenditure)
skewness(data_set$percentage.expenditure)
hist(data_set$percentage.expenditure)

sd(data_set$Hepatitis.B)
skewness(data_set$Hepatitis.B)
hist(Hepatitis.B)

sd(data_set$Measles)
skewness(data_set$Measles)
hist(data_set$Measles)

sd(data_set$BMI)
skewness(data_set$BMI)
hist(data_set$BMI)

sd(data_set$under.five.deaths)
skewness(data_set$under.five.deaths)
hist(data_set$under.five.deaths)

sd(data_set$Polio)
skewness(data_set$Polio)
hist(data_set$Polio)

sd(data_set$Total.expenditure)
skewness(data_set$Total.expenditure)
hist(data_set$Total.expenditure)

sd(data_set$Diphtheria)
skewness(data_set$Diphtheria)
hist(data_set$Diphtheria)

sd(data_set$HIV.AIDS)
skewness(data_set$HIV.AIDS)
hist(data_set$HIV.AIDS)

sd(data_set$GDP)
skewness(data_set$GDP)
hist(data_set$GDP)

sd(data_set$Population)
skewness(data_set$Population)
hist(data_set$Population)

sd(data_set$thinness..1.19.years)
skewness(data_set$thinness..1.19.years)
hist(data_set$thinness..1.19.years)

sd(data_set$thinness.5.9.years)
skewness(data_set$thinness.5.9.years)
hist(data_set$thinness.5.9.years)

sd(data_set$Income.composition.of.resources)
skewness(data_set$Income.composition.of.resources)
hist(data_set$Income.composition.of.resources)

sd(data_set$Schooling)
skewness(data_set$Schooling)
hist(data_set$Schooling)

sd(data_set$Life.expectancy)
skewness(data_set$Life.expectancy)
hist(data_set$Life.expectancy)

# discriptive plots for the descrets veriables

ggboxplot(data, x = "Alcohol", y = "Life.expectancy", 
          color = "Alcohol") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggboxplot(data, x = "Status", y = "Life.expectancy", 
          color = "Status") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# discriptive distributions for the descrets veriables

ggplot(data, aes(x = Alcohol, fill = Alcohol)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Alcohol count",
       title = "Alcohol distribution")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data, aes(x = Status, fill = Status)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Status count",
       title = "Status distribution")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Question 6 - exceptional analysis *********************

bp1<-boxplot(data$Life.expectancy, main='Life expectancy')$out

bp2<-boxplot(data$Adult.Mortality, main='Adult.Mortality')$out

bp3<-boxplot(data$infant.deaths, main='infant.deaths')$out

bp4<-boxplot(data$percentage.expenditure, main='percentage.expenditure')$out

bp5<-boxplot(data$Hepatitis.B, main='Hepatitis.B')$out

bp6<-boxplot(data$Measles, main='Measles')$out

bp7<-boxplot(data$BMI, main='BMI')$out

bp8<-boxplot(data$under.five.deaths, main='under.five.deaths')$out

bp9<-boxplot(data$Polio, main='Polio')$out

bp10<-boxplot(data$Total.expenditure, main='Total.expenditure')$out

bp11<-boxplot(data$Diphtheria, main='Diphtheria')$out

bp12<-boxplot(data$HIV.AIDS, main='HIV.AIDS')$out

bp13<-boxplot(data$GDP, main='GDP')$out

bp14<-boxplot(data$Population, main='Population')$out

bp15<-boxplot(data$thinness..1.19.years, main='thinness..1.19.years')$out

bp16<-boxplot(data$thinness.5.9.years, main='thinness.5.9.years')$out

bp17<-boxplot(data$Income.composition.of.resources, main='Income.composition.of.resources')$out

bp18<-boxplot(data$Schooling, main='Schooling')$out

# Question 7 - density and comulative

hist(data$Adult.Mortality,prob=TRUE, main='Adult Mortality',xlab = 'Adult Mortality',col="pink") 
lines(density(data$Adult.Mortality),col="blue",lwd=2) 
plot(ecdf(data$Adult.Mortality),col="blue", xlab = "Adult mortality", ylab = "Comulative value", main="Adult mortality comulative function")

hist(data$Total.expenditure,prob=TRUE, main='Total expenditure',xlab = 'Total expenditure',col="pink") 
lines(density(data$Total.expenditure),col="blue",lwd=2) 
plot(ecdf(data$Total.expenditure),col="blue", xlab = "Total expenditure", ylab = "Comulative value", main="Total expenditure comulative function")

hist(data$BMI,prob=TRUE, main='BMI',xlab = 'BMI',col="pink") 
lines(density(data$BMI),col="blue",lwd=2) 
plot(ecdf(data$BMI),col="blue", xlab = "BMI", ylab = "Comulative value", main="BMI comulative function")


# Question 8 - relationships 

cov(data)
cor(data)
ggcorrplot(data)
corrplot(cor_data ,method = "circle")

plot(x=data$Life.expectancy,y=data$Adult.Mortality,xlab="Life expectancy ",ylab="Adult Mortality")
plot(x=data$Life.expectancy,y=data$Schooling,xlab="Life.expectancy",ylab="Schooling")
plot(x=data$Life.expectancy,y=data$HIV.AIDS,xlab="Life.expectancy",ylab="HIV.AIDS")
plot(x=data$GDP,y=data$percentage.expenditure,xlab="GDP",ylab="percentage.expenditure")
plot(x=data$infant.deaths,y=data$under.five.deaths,xlab="infant deaths",ylab="under five deaths")


# Question 9 - common values

#----------------schooling--------

schoole6yers <- (sqldf("select Schooling
              from data
              where Schooling<='6'"))

lengthSchoole6yers <- length(schoole6yers$Schooling)


schoole6.12yers <- (sqldf("select Schooling
              from data
              where Schooling>'6' and Schooling<=12"))

lengthSchoole6.12yers <- length(schoole6.12yers$Schooling)


schoole12yers <- (sqldf("select schoole6yers
              from data
              where Schooling>'12'"))

lengthSchoole12yers <- length(schoole12yers$Schooling)

paste (lengthSchoole6yers, lengthSchoole6.12yers, lengthSchoole12yers)

#----------------BMI--------------

BMI25 <- (sqldf("select BMI
              from data
              where BMI>'25'"))

lengthBMI25  <- length(BMI25$BMI)


BMI18.5.25 <- (sqldf("select BMI
              from data
              where BMI>='18.5' and BMI<=25"))

lengthBMI18.5.25 <- length(BMI18.5.25$BMI)


BMI18.5 <- (sqldf("select BMI
              from data
              where BMI<'18.5'"))

lengthBMI18.5 <- length(BMI18.5$BMI)

paste (lengthBMI25, lengthBMI18.5.25, lengthBMI18.5)

#-----------------Diphtheria & under five deaths----------------------------------

colnames(data)[9] <- "underfivedeaths"

Diphtheria50 <- (sqldf("select count(*)
              from data
              where Diphtheria>='50'and underfivedeaths =='0'"))

DiphtheriaUnder50 <- (sqldf("select count(*)
              from data
              where Diphtheria<'50' and underfivedeaths =='0' "))

#------------------------

Diphtheria502 <- (sqldf("select count(*)
              from data
              where Diphtheria>='50'and underfivedeaths <'50'"))

DiphtheriaUnder502 <- (sqldf("select count(*)
              from data
              where Diphtheria<'50' and underfivedeaths <'50' "))

#------------------------

Diphtheria503 <- (sqldf("select count(*)
              from data
              where Diphtheria>='50'and underfivedeaths >='50' and underfivedeaths <'100'"))

DiphtheriaUnder503 <- (sqldf("select count(*)
              from data
              where Diphtheria<'50' and underfivedeaths >='50' and underfivedeaths <'100' "))

#------------------------

Diphtheria504 <- (sqldf("select count(*)
              from data
              where Diphtheria>='50'and underfivedeaths >='100' and underfivedeaths <'200'"))

DiphtheriaUnder504 <- (sqldf("select count(*)
              from data
              where Diphtheria<'50' and underfivedeaths >='100' and underfivedeaths <'200' "))

#------------------------

Diphtheria505 <- (sqldf("select count(*)
              from data
              where Diphtheria>='50'and underfivedeaths >='200'"))

DiphtheriaUnder505 <- (sqldf("select count(*)
              from data
              where Diphtheria<'50' and underfivedeaths >='200'"))

paste(Diphtheria50, DiphtheriaUnder50, Diphtheria502, DiphtheriaUnder502, Diphtheria503, DiphtheriaUnder503, Diphtheria504, DiphtheriaUnder504, Diphtheria505, DiphtheriaUnder505)

#-----------------status & alcohol ----------------------------------

Status1 <- (sqldf("select Alcohol 
              from data
              where Status=='1'"))

Status1.1<- (sqldf("select count(*) 
              from Status1
              where Alcohol=='1'"))
paste(Status1.1)

Status1.2<- (sqldf("select count(*) 
              from Status1
              where Alcohol=='2'"))
paste(Status1.2)

Status1.3<- (sqldf("select count(*) 
              from Status1
              where Alcohol=='3'"))
paste(Status1.3)

Status1.4<- (sqldf("select count(*) 
              from Status1
              where Alcohol=='4'"))
paste(Status1.4)

Status1.5<- (sqldf("select count(*) 
              from Status1
              where Alcohol=='5'"))
paste(Status1.5)

#--------------------------------------------

Status2 <- (sqldf("select Alcohol
              from data
              where Status=='2'"))


Status2.1<- (sqldf("select count(*) 
              from Status2
              where Alcohol=='1'"))
paste(Status2.1)

Status2.2<- (sqldf("select count(*) 
              from Status2
              where Alcohol=='2'"))
paste(Status2.2)

Status2.3<- (sqldf("select count(*) 
              from Status2
              where Alcohol=='3'"))
paste(Status2.3)

Status2.4<- (sqldf("select count(*) 
              from Status2
              where Alcohol=='4'"))
paste(Status2.4)

Status2.5<- (sqldf("select count(*) 
              from Status2
              where Alcohol=='5'"))
paste(Status2.5)

#---------------------------------------------------------------<Part B>----------------------------------------------------------------

# QUESTION 2.1 - הסרת משתנים

#-----------------Pearson correlation test---------------

data$color <- ifelse(data$Status == "1", "orange", "blue") # difference between developed and developing countries


cor.test(data$Life.expectancy, data$Status, method = "pearson")
plot(x=data$Adult.Mortality, y=data$Life.expectancy, xlab="Adult.Mortality", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$Adult.Mortality, method = "pearson")
plot(x=data$Adult.Mortality, y=data$Life.expectancy, xlab="Adult.Mortality", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$infant.deaths, method = "pearson")
plot(x=data$infant.deaths, y=data$Life.expectancy, xlab="infant.deaths", ylab="Life expectancy", col=data$color)
#------------------infant deaths Exceptions__________________________________
harigim<-boxplot(data$infant.deaths, main='infant.deaths')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$infant.deaths < min_harigim)
cor.test(no_harigim$Life.expectancy, no_harigim$infant.deaths, method = "pearson")
plot(x=no_harigim$infant.deaths, y=no_harigim$Life.expectancy, xlab="infant.deaths", ylab="Life expectancy", col=data$color)


cor.test(data$Life.expectancy, data$Alcohol, method = "pearson")
plot(x=data$Alcohol, y=data$Life.expectancy, xlab="Alcohol", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$percentage.expenditure, method = "pearson")
plot(x=data$percentage.expenditure, y=data$Life.expectancy, xlab="percentage.expenditure", ylab="Life expectancy", col=data$color)
#------------------percentage expenditure Exceptions-------------------

harigim<-boxplot(data$percentage.expenditure, main='percentage.expenditure')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$percentage.expenditure < min_harigim)
cor.test(no_harigim$Life.expectancy, no_harigim$percentage.expenditure, method = "pearson")
plot(x=no_harigim$percentage.expenditure, y=no_harigim$Life.expectancy, xlab="percentage.expenditure", ylab="Life expectancy", col=data$color)


cor.test(data$Life.expectancy, data$Hepatitis.B, method = "pearson")
plot(x=data$Hepatitis.B, y=data$Life.expectancy, xlab="Hepatitis.B", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$Measles, method = "pearson")
plot(x=data$Measles, y=data$Life.expectancy, xlab="Measles", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$BMI, method = "pearson")
plot(x=data$BMI, y=data$Life.expectancy, xlab="BMI", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$under.five.deaths, method = "pearson")
plot(x=data$under.five.deaths, y=data$Life.expectancy, xlab="under.five.deaths", ylab="Life expectancy", col=data$color)

#------------------under five deaths Exceptions----------------------

harigim<-boxplot(data$under.five.deaths, main='under.five.deaths')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$under.five.deaths < min_harigim)
cor.test(no_harigim$Life.expectancy, no_harigim$under.five.deaths, method = "pearson")
plot(x=no_harigim$under.five.deaths, y=no_harigim$Life.expectancy, xlab="under.five.deaths", ylab="Life expectancy", col=data$color)

#--------------------------------------------------------------------

cor.test(data$Life.expectancy, data$Polio, method = "pearson")
plot(x=data$Polio, y=data$Life.expectancy, xlab="Polio", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$Total.expenditure, method = "pearson")
plot(x=data$Total.expenditure, y=data$Life.expectancy, xlab="Total.expenditure", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$Diphtheria, method = "pearson")
plot(x=data$Diphtheria, y=data$Life.expectancy, xlab="Diphtheria", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$HIV.AIDS, method = "pearson")
plot(x=data$HIV.AIDS, y=data$Life.expectancy, xlab="HIV.AIDS", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$GDP, method = "pearson")
plot(x=data$GDP, y=data$Life.expectancy, xlab="GDP", ylab="Life expectancy", col=data$color)


#------------------GDP Exceptions----------------------

harigim<-boxplot(data$GDP, main='GDP')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$GDP < min_harigim)
cor.test(no_harigim$Life.expectancy, no_harigim$GDP, method = "pearson")
plot(x=no_harigim$GDP, y=no_harigim$Life.expectancy, xlab="GDP", ylab="Life expectancy", col=data$color)

#------------------------------------------------------

cor.test(data$Life.expectancy, data$Population, method = "pearson")
plot(x=data$Population, y=data$Life.expectancy, xlab="Population", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$thinness.9.19.years, method = "pearson")
plot(x=data$thinness.9.19.years, y=data$Life.expectancy, xlab="thinness.9.19.years", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$Income.composition.of.resources, method = "pearson")
plot(x=data$Income.composition.of.resources, y=data$Life.expectancy, xlab="Income.composition.of.resources", ylab="Life expectancy", col=data$color)

cor.test(data$Life.expectancy, data$Schooling, method = "pearson")
plot(x=data$Schooling, y=data$Life.expectancy, xlab="Schooling", ylab="Life expectancy", col=data$color)


# QUESTION 2.2 - התאמת משתנים
##---------------HIV to category-------------------
data[data$HIV.AIDS<2.5,"HIV_category"]<- "low"
data[data$HIV.AIDS>=2.5 & data$HIV.AIDS<5,"HIV_category"]<- "medium"
data[data$HIV.AIDS>=5,"HIV_category"]<- "high"
data$HIV_category<- factor(data$HIV_category, levels=c("low","medium","high"))

ggboxplot(data, x="HIV_category", y="Life.expectancy", add="jitter", color = "HIV_category")
ggscatter(no_harigim, x="HIV.AIDS", y="Life.expectancy", add="reg.line",conf.int = TRUE, add.params = list(color = "blue",fill = "lightgray"))+stat_cor(method = "pearson")

aov.test1 <- aov(Life.expectancy~HIV_category,data = data)
summary(aov.test1)

aov.test1 <- aov(Life.expectancy~HIV.AIDS,data = data)
summary(aov.test1)

summary(lm(data$Life.expectancy~data$HIV_category, data))$r.squared

summary(lm(data$Life.expectancy~data$HIV.AIDS, data))$r.squared

##---------------GDP  to category-------------------
data[data$GDP<10000,"GDP_category"]<- "low"
data[data$GDP>=10000 & data$GDP<25000,"GDP_category"]<- "medium"
data[data$GDP>=25000,"GDP_category"]<- "high"
data$GDP_category<- factor(data$GDP_category, levels=c("low","medium","high"))

ggboxplot(data, x="GDP_category", y="Life.expectancy", add="jitter", color = "GDP_category")
ggscatter(data, x="GDP", y="Life.expectancy", add="reg.line",conf.int = TRUE, add.params = list(color = "blue",fill = "lightgray"))+stat_cor(method = "pearson")

aov.test1 <- aov(Life.expectancy~GDP_category,data = data)
summary(aov.test1)

aov.test1 <- aov(Life.expectancy~GDP,data = data)
summary(aov.test1)

summary(lm(data$Life.expectancy~data$GDP_category, data))$r.squared

summary(lm(data$Life.expectancy~data$GDP, data))$r.squared

# QUESTION 2.4 - משתני אינטראקציה

# life expectancy by infant death * status V
harigim<-boxplot(data$infant.deaths, main='infant.deaths')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$infant.deaths < min_harigim)

x1 <- lm(formula=no_harigim$Life.expectancy~no_harigim$infant.deaths*factor(no_harigim$Status))
summary(x1)

plot(no_harigim$infant.deaths[no_harigim$Status=='2'],no_harigim$Life.expectancy[no_harigim$Status=='2'],
     col="green",xlab="Infant Death",ylab = "Life Expectancy")
points(no_harigim$infant.deaths[no_harigim$Status=='1'], no_harigim$Life.expectancy[no_harigim$Status=='1'],
       col="blue")
abline(a=81.2203 ,b= -0.30720,col="Blue")
abline(a=81.2203-8.89505 , b=-0.3072+0.02355, col="green")

# life expectancy by HIV * GDP ***********************************
x2 <- lm(formula=data$Life.expectancy~data$HIV.AIDS*factor(data$GDP_category))
summary(x2)

plot(data$HIV.AIDS[data$GDP_category=='low'],data$Life.expectancy[data$GDP_category=='low'],
      col="green",xlab="HIV.AIDS",ylab = "Life Expectancy")
points(data$HIV.AIDS[data$GDP_category=='medium'], data$Life.expectancy[data$GDP_category=='medium'],
       col="blue")
points(data$HIV.AIDS[data$GDP_category=='high'], data$Life.expectancy[data$GDP_category=='high'],
       col="red")

abline(a= 71.6657 ,b= -3.0189,col="green")
abline(a= 71.6657+8.7574, b=-3.0189-28.0580, col="blue")
abline(a= 71.6657+11.5612 , b=-3.0189+0.02355, col="red")


# life expectancy by thinness.9.19.years * GDP V

harigim<-boxplot(data$thinness.9.19.years, main='thinness.9.19.years')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$thinness.9.19.years < min_harigim)

x3 <- lm(formula=no_harigim$Life.expectancy~no_harigim$thinness.9.19.years*factor(no_harigim$GDP_category))
summary(x3)

plot(no_harigim$thinness.9.19.years[no_harigim$GDP_category=='low'],no_harigim$Life.expectancy[no_harigim$GDP_category=='low'],
     col="green",xlab="thinness.9.19.years",ylab = "Life Expectancy")
points(no_harigim$thinness.9.19.years[no_harigim$GDP_category=='medium'], no_harigim$Life.expectancy[no_harigim$GDP_category=='medium'],
       col="blue")
points(no_harigim$thinness.9.19.years[no_harigim$GDP_category=='high'], no_harigim$Life.expectancy[no_harigim$GDP_category=='high'],
       col="red")

abline(a=75.5084 ,b= -1.5662,col="green")
abline(a=75.5084+4.3831, b=-1.5662+0.4141, col="blue")
abline(a=75.5084+8.1045 , b=-1.5662+0.8606, col="red")

# life expectancy by thinness.9.19.years * status
harigim<-boxplot(data$thinness.9.19.years, main='thinness.9.19.years')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$thinness.9.19.years < min_harigim)

x4 <- lm(formula=no_harigim$Life.expectancy~no_harigim$thinness.9.19.years*factor(no_harigim$Status))
summary(x4)

plot(no_harigim$thinness.9.19.years[no_harigim$Status=='2'],no_harigim$Life.expectancy[no_harigim$Status=='2'],
     col="green",xlab="thinness.9.19.years Death",ylab = "Life Expectancy")
points(no_harigim$thinness.9.19.years[no_harigim$Status=='1'], no_harigim$Life.expectancy[no_harigim$Status=='1'],
       col="blue")
abline(a=87.519 ,b=  -5.015,col="Blue")
abline(a=87.519-12.538 , b= -5.015+3.580, col="green")

# life expectancy by Alcohol * Adult.Mortality

harigim<-boxplot(data$Adult.Mortality, main='Adult.Mortality')$out
min_harigim <- min(harigim)
no_harigim <- subset(data, data$Adult.Mortality < min_harigim)

x5 <- lm(formula=no_harigim$Life.expectancy~no_harigim$Adult.Mortality*factor(no_harigim$Alcohol))
summary(x5)

plot(no_harigim$Adult.Mortality[no_harigim$Alcohol=='1'],no_harigim$Life.expectancy[no_harigim$Alcohol=='1'],
     col="green",xlab="Adult.Mortality",ylab = "Life Expectancy")
points(no_harigim$Adult.Mortality[no_harigim$Alcohol=='2'], no_harigim$Life.expectancy[no_harigim$Alcohol=='2'],
       col="blue")
points(no_harigim$Adult.Mortality[no_harigim$Alcohol=='3'], no_harigim$Life.expectancy[no_harigim$Alcohol=='3'],
       col="red")
points(no_harigim$Adult.Mortality[no_harigim$Alcohol=='4'], no_harigim$Life.expectancy[no_harigim$Alcohol=='4'],
       col="orange")

abline(a=76.061884 ,b= -0.047869,col="green")
abline(a=76.061884+3.184495, b=-0.047869+0.010676, col="blue")
abline(a=76.061884+8.442530 , b=-0.047869-0.025921, col="red")
abline(a=76.061884+9.952198 , b=-0.047869-0.011757, col="orange")

# QUESTION 3.1 - משתני מודל
I1<-factor(data$Status)
I4<-factor(data$Alcohol)
I14<-factor(data$GDP_category)

x2<-data$Adult.Mortality
x3<-data$infant.deaths
x5<-data$percentage.expenditure
x6<-data$Hepatitis.B
x7<-data$Measles
x8<-data$BMI
x9<-data$under.five.deaths
x10<-data$Polio
x11<-data$Total.expenditure
x12<-data$Diphtheria
x13<-data$HIV.AIDS
x15<-data$Population
x16<-data$thinness.9.19.years
x17<-data$thinness.5.9.years
x18<-data$Income.composition.of.resources
x19<-data$Schooling

y<-data$Life.expectancy


Full<-lm (y ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19,data)
summary(Full)

Emp<-lm (y~1, data)
summary(Emp)


#AIC----------------

fwdAIC <-step(Emp, direction = 'forward', scope = ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7
              +x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19)
summary(fwdAIC)

bcdAIC <-step(Full, direction = 'backward',scope =~1 )
summary(bcdAIC)

swAIC <-step(Emp, direction = 'both', scope = ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19)
summary(swAIC)

#BIC----------------

fwdBIC <-step(Emp, direction = 'forward', k = log(131), scope = ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19)
summary(fwdBIC)

bcdBIC <-step(Full, direction = 'backward',k = log(131), scope =~1 )
summary(bcdBIC)

swBIC <-step(Emp, direction = 'both', k = log(131), scope = ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19)
summary(swBIC)

ourModel<-lm (y ~ x2+x3+x9+x11+x13+x19+I4*x2 ,data)
summary(ourModel)

# QUESTION 3.2 - בדיקת הנחות המודל
# what we chose----------------
bcdAIC <-step(Full, direction = 'backward',scope =~1 )
summary(bcdAIC)

yPredicted<-fitted(bcdAIC) # predicted values
data$residuals<-residuals(bcdAIC) # residuals
s.e_res <- sqrt(var(data$residuals))
data$stan_residuals<-(residuals(bcdAIC)/s.e_res)

plot(data$stan_residuals ~ yPredicted, xlab="Fitted values", 
     ylab="Residuals", main="Residuals VS Fitted")
abline(a=0, b=0)

#---------שוויון שונויות 

sortedData <- data[order(data[,2]),]
model.first <- lm (y ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19 ,data = sortedData,subset = 1:29)
model.last <- lm (y ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19 ,data = sortedData, subset = 59:87)        
var.test(model.first, model.last)

#---------לינאריות  
install.packages("strucchange")
library(strucchange)
sctest(bcdAIC,type ="Chow")

#---------נורמאליות 
qqnorm(data$stan_residuals)
abline(a=0, b=1)
hist(data$stan_residuals, xlab ="Normalized error", 
     main="Histogram of normalized error")

#KS  & SW test
ks.test(x= data$stan_residuals,y="pnorm",alternative = "two.sided", exact = NULL)
shapiro.test(data$stan_residuals)

# QUESTION 3.3 - שימוש במודל הנבחר
# I1*x10 +I2*x3 + I9*x5 + x3+ x4 +x5 +x6 + x10 ,data = dataset)
# y ~ x2+x3+x9+x11+x13+x18+I4*x2 ,data)
# Full<-lm (y ~ I1*x3+I4*x2+I14*x16+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x15+x16+x17+x18+x19,data)

predict (ourModel,data.frame(I4='3',x2= 52, x3=72, x9= 89, x11=4.32 ,x13=1.8,x19=11.8), interval = "predict")
predict (ourModel,data.frame(I4='1', x2= 11, x3=21, x9= 24, x11=7.21 ,x13=0.1,  x19=14.4), interval = "predict")
predict (Full,data.frame(I1="2", I4="1", I14="low", x2=271, x3=64, x5=73.523582, x6=62, x7=492, x8=18.6,
x9=86, x10=58, x11=8.18, x12=62, x13=0.1, x15=327582, x16=17.5, x17=17.5, x18=0.476, x19=10.0), interval = "predict")



#------------------------------Part B4-----------------------------------
newdata <- data[,c(20,2,3,4,9,11,13,19)]

cor(newdata)


#X^2
xsquaredData <- data[,c(20,2,3,4,9,11,13,19)]
xsquaredData$Life.expectancy <- (xsquaredData$Life.expectancy)^2
xsquaredData$Adult.Mortality <- (xsquaredData$Adult.Mortality)^2
xsquaredData$infant.deaths <- (xsquaredData$infant.deaths)^2
xsquaredData$under.five.deaths <- (xsquaredData$under.five.deaths)^2
xsquaredData$Total.expenditure <- (xsquaredData$Total.expenditure)^2
xsquaredData$HIV.AIDS <- (xsquaredData$HIV.AIDS)^2
xsquaredData$Schooling <- (xsquaredData$Schooling)^2

cor(xsquaredData)

#ln(x)
xlnData <- data[,c(20,2,3,4,9,11,13,19)]
xlnData$Life.expectancy <- log(xlnData$Life.expectancy)
xlnData$Adult.Mortality <- log(xlnData$Adult.Mortality)
xlnData$infant.deaths <- log(xlnData$infant.deaths)
xlnData$under.five.deaths <- log(xlnData$under.five.deaths)
xlnData$Total.expenditure <- log(xlnData$Total.expenditure)
xlnData$Schooling <- log(xlnData$Schooling)

cor(xlnData)

#sqrtx
sqrtxData <- data[,c(20,2,3,4,9,11,13,19)]
sqrtxData$Life.expectancy <- sqrt(sqrtxData$Life.expectancy)
sqrtxData$Adult.Mortality <- sqrt(sqrtxData$Adult.Mortality)
sqrtxData$infant.deaths <- sqrt(sqrtxData$infant.deaths)
sqrtxData$under.five.deaths <- sqrt(sqrtxData$under.five.deaths )
sqrtxData$Total.expenditure <- sqrt(sqrtxData$Total.expenditure)
sqrtxData$Schooling <- sqrt(sqrtxData$Schooling )

cor(sqrtxData)


#new model check


FullTrans<-lm (y ~ I1*sqrt(x3)+I4*(x2)^2+I14*x16+(x2)^2+sqrt(x3)+x5+x6+x7+x8+x9+x10+(x11)^2+x12+log(x13)+x15+x16+x17+x18+(x19)^2,data)
summary(FullTrans)

EmpTrans<-lm (y~1, data)
summary(EmpTrans)


#AIC----------------

fwdAICnew <-step(EmpTrans, direction = 'forward', scope = ~ I1*sqrt(x3)+I4*(x2)^2+I14*x16+(x2)^2+sqrt(x3)+x5+x6+x7+x8+x9+x10+(x11)^2+x12+log(x13)+x15+x16+x17+x18+(x19)^2)
summary(fwdAICnew)

bcdAICnew <-step(FullTrans, direction = 'backward',scope =~1 )
summary(bcdAICnew)

swAICnew <-step(EmpTrans, direction = 'both', scope = ~ I1*sqrt(x3)+I4*(x2)^2+I14*x16+(x2)^2+sqrt(x3)+x5+x6+x7+x8+x9+x10+(x11)^2+x12+log(x13)+x15+x16+x17+x18+(x19)^2)
summary(swAIC)

#BIC----------------

fwdBICnew <-step(EmpTrans, direction = 'forward', k = log(131), scope = ~ I1*sqrt(x3)+I4*(x2)^2+I14*x16+(x2)^2+sqrt(x3)+x5+x6+x7+x8+x9+x10+(x11)^2+x12+log(x13)+x15+x16+x17+x18+(x19)^2)
summary(fwdBIC)

bcdBICnew <-step(FullTrans, direction = 'backward',k = log(131), scope =~1 )
summary(bcdBIC)

swBICnew <-step(EmpTrans, direction = 'both', k = log(131), scope = ~I1*sqrt(x3)+I4*(x2)^2+I14*x16+(x2)^2+sqrt(x3)+x5+x6+x7+x8+x9+x10+(x11)^2+x12+log(x13)+x15+x16+x17+x18+(x19)^2)
summary(swBIC)

ourModelnew<-lm (y ~ x2+x3+x9+x11+x13+x19+I4*x2 ,data)
summary(ourModel)

