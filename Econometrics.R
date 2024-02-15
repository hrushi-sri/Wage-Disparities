#set the current working directory

library(broom)
library(zoo)
library(lmtest)
library(corrplot)
library(ggplot2)

data <- read.csv("cps91.csv")
head(data)

names(data) #columns of the data set

colSums(is.na(data)) #number of null values in the data set
data=subset(data,select=-c(husunion,union,lwage)) #dropped varibles with a lot of null values
data$hours<-ifelse(is.na(data$hours)==1,mean(data$hours,na.rm = TRUE),data$hours) #filled number of hours with mean hours

#analysing null values in hrwage
sum(data$hours==0,na.rm = TRUE)
data$hrwage<-ifelse(is.na(data$hrwage)==1,0,data$hrwage)

#as we can see that the whole data consists of women in the labourforce we can remove the column inlf
data=subset(data,select = -c(X,inlf))

#finding hourly wage of husband
data$hushrwage<-data$husearns/data$hushrs
data$hushrwage<-ifelse(data$hushrwage==Inf,0,data$hushrwage)
data$hushrwage<-ifelse(is.na(data$hushrwage)==1,0,data$hushrwage)


#<<< Correlation Matrix >>>>
matrix<-round(cor(data),2)
# Create a correlation matrix plot
corrplot(matrix, method = "color")

#from correlation matrix husexper and husage are directly correlated and wife's exper and age are directly correlated so we can drop husage and age.
#from correlation matrix black and husblack , hisp and hushisp are highly correlated with 0.95 and 0.75 relationship. So we drop one of each.
data<-subset(data,select=-c(husage,age,husblck,hushisp))
data$wagegap<-data$husearns-data$earns
data<-subset(data, select =-c(earns,husearns))
dim(data) #we are left with 16 variables in the data frame.
names(data)
#modeling with all the variables 
model1<- lm(wagegap~.,data=data)
tidy(model1)

#from the above model the insignificant variables at 5% level are
# kidge6, black, hispanic, kidlt6, educ,
sum(data$wagegap==0)

model2<- lm(wagegap~.-expersq-kidge6-hispanic-black-educ-kidlt6,data=data)
tidy(model2)

anova(model1,model2)


# Round the estimates in the summary output
rounded_summary <- summary(model2)
rounded_summary$coefficients <- round(rounded_summary$coefficients, 3)
print(rounded_summary)


#wagegap variable analysis

ggplot(data, aes(x =data$wagegap)) +
  geom_histogram(colour='dark red')+
  labs( title = "Distribution of wage gap variable")

####Plots bw significant independent and dependent

#1
ggplot(data, aes(x = faminc, y = wagegap, group=faminc)) +
  geom_boxplot(colour='dark red')+
  labs(x = "faminc", y = "wagegap", title = "Box Plot for faminc V/S wagegap")


#2
ggplot(data, aes(x = exper, y = wagegap, group=exper)) +
  geom_boxplot(colour='dark red')+
  labs(x = "exper", y = "wagegap", title = "Box Plot for experience V/S wagegap")

#3
ggplot(data, aes(x = huseduc, y = wagegap, group=huseduc)) +
  geom_boxplot(colour='dark red')+
  labs(x = "husband education", y = "wagegap", title = "Box Plot for husband education V/S wagegap")

#4
ggplot(data, aes(x = hushrs, y = wagegap, group=hushrs)) +
  geom_boxplot(colour='dark red')+
  labs(x = "hushrs", y = "wagegap", title = "Box Plot for husband hours V/S wagegap")



# Perform the Breusch-Pagan test for homoskedasticity
bptest(model2)
