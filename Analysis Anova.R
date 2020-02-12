#import dataset
DB=read.csv("Diabetes_In_Indian_Women.csv")
head(DB,5)
summary(DB$Glucose)
summary(DB$Age)
summary(DB$Insulin)
summary(DB$BloodPressure)
summary(DB$BMI)
#Distribution of Glucose
hist(DB$Glucose,main="Histogram of Glucose", 
     xlab="Glucose", 
     border="red", 
     col="light blue",
     las=1)
#Distribution of BloodPressure
hist(DB$BloodPressure,main="Histogram of BloodPressure", 
     xlab="BloodPressure", 
     border="black", 
     col="red",
     las=1)
#Distribution of BMI
hist(DB$BMI,main="Histogram of BMI", 
     xlab="BMI", 
     border="black", 
     col="green",
     las=1)
#Distribution of Age
hist(DB$Age,main="Histogram of Age", 
     xlab="Age", 
     border="black", 
     col="brown",
     las=1)
#Distribution of Insulin
hist(DB$Insulin,main="Histogram of Insulin", 
     xlab="Insulin", 
     border="black", 
     col="yellow",
     las=1)
#Linear model for data
model=lm(Outcome~.,data=DB)
model
summary(model)
anova(model)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
#step regression model
stepAIC(model,direction="both")
g=glm(Outcome~.^2,data=DB)
g
summary(g)
anova(g)
#2way anova for Glucose n Pregnancies
summary(aov(DB$Outcome~DB$Pregnancies+DB$Glucose))
#2way anova for Glucose n BP
summary(aov(DB$Outcome~DB$Glucose+DB$BloodPressure))
#2way anova for Glucose n BMI
summary(aov(DB$Outcome~DB$Glucose+DB$BMI))
#2way anova for Pregancies n BMI
summary(aov(DB$Outcome~DB$Pregnancies+DB$BMI))
#2way anova for pedigreedfunction n BMI
summary(aov(DB$Outcome~DB$DiabetesPedigreeFunction+DB$BMI))
#Duncan's test 
library(stats)
fglucose=factor(DB$Glucose)
#finsulin=factor(DB$Insulin)
fbmi=factor(DB$BMI)
fbp=factor(DB$BloodPressure)
library(DescTools)
library(agricolae)
#Tukey's test #for Glucose
plot(TukeyHSD(aov(DB$Outcome~fglucose),"fglucose"))
#for bmi
plot(TukeyHSD(aov(DB$Outcome~fbmi),"fbmi"))
#for bp
plot(TukeyHSD(aov(DB$Outcome~fbp),"fbp"))

