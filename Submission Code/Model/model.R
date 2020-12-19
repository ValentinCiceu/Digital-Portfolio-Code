# the following is code used for the model section of the portfolio. You can see the rough work on the sandbox directory
# Essential libraries used for this assignment
library(e1071)   
library(psych)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(sqldf) # you might not have this pacakage installed. please run install.packages("sqldf")
library(semTools)
library(pastecs)
library(sjstats) 
library(userfriendlyscience)
library(foreign)
library(lm.beta)
library(stargazer)
library(broom)
library(Epi)
library(arm)
library(DescTools)
library(car)
library(generalhoslem)
library(regclass)

# Make sure the data is in the local directory
data <- read.csv("sperformance-dataset.csv", na.strings=c("na"," ","",".","NA"), header = TRUE)

# removing the 0 score from the grades
norm_score_data <- sqldf("select * from data where mg1 > 0 and mg2 > 0 and mg3 > 0 and pg1 > 0 and pg2 > 0 and pg3 > 0")

# simple linear regression model
model1<-lm(norm_score_data$mG3~norm_score_data$mG1)
anova(model1)
summary(model1)
lm.beta::lm.beta(model1)
stargazer(model1, type="text") 
# result is 1.425 + 0.901


# Multiple linear regression on the grades
model2<-lm(norm_score_data$mG3~norm_score_data$mG2+norm_score_data$mG1)
anova(model2)
summary(model2)
stargazer(model2, type="text") #Tidy output of all the required stats
lm.beta(model2)
stargazer(model1, model2, type="text") #Quick model comparison

model_metrics <- augment(model2)


# homocedasticity graph
options(repr.plot.width = 16, repr.plot.height = 10)
par(mfrow = c(2, 2))
plot(model2)
plot(model2, 1)
plot(model2, 2)
plot(model2, 3)
plot(model2, 4)

# copy of the main data to use dummy variables
norm_score_data_dummy <- norm_score_data

# building second model, using dummy variables
# Multiple linear regression0
# long travel time (31 mins above), short travel time(30 mins below)
#numeric: 1 – < 15 min., 2 – 15 to 30 min., 3 – 30 min. to 1 hour or 4 – > 1 hour
norm_score_data_dummy$traveltimeLong = ifelse(norm_score_data_dummy$traveltime.m >=3,1,0)
norm_score_data_dummy$traveltimeShort = ifelse(norm_score_data_dummy$traveltime.m <=2,1,0)


model3<-lm(formula = mG3 ~ traveltimeLong + traveltimeShort ,data = norm_score_data_dummy)
stargazer(model3, type="text") #Quick model comparison
summary(model3)


# dummy variables for studytime
norm_score_data_dummy$studytimeLong = ifelse(norm_score_data_dummy$studytime.m >=3,1,0)
norm_score_data_dummy$studytimeShort = ifelse(norm_score_data_dummy$studytime.m <=2,1,0)

# checking significance of studytime
model4<-lm(norm_score_data_dummy$mG3~norm_score_data_dummy$studytimeLong+norm_score_data_dummy$studytimeShort)
stargazer(model4, type="text") #Quick model comparison
summary(model4)


#dummy data for male and Female
# norm_score_data_dummy$sex = ifelse(norm_score_data_dummy$sex == "M",0,ifelse(norm_score_data_dummy$sex == "F",1,NA))
norm_score_data_dummy$genderMale = ifelse(norm_score_data_dummy$sex == "M",1,0)
norm_score_data_dummy$genderFemale = ifelse(norm_score_data_dummy$sex == "F",1,0)


# checking significance of studytime
modelSex<-lm(norm_score_data_dummy$mG3~norm_score_data_dummy$genderMale+norm_score_data_dummy$genderFemale)
stargazer(modelSex, type="text") #Quick model comparison
summary(modelSex)


# testing model
# checking significance of sex
model5<-lm(norm_score_data_dummy$mG3~norm_score_data_dummy$sex)
stargazer(model5, type="text") #Quick model comparison
summary(model5)

final_model = lm(formula=mG3 ~ genderMale + studytimeLong + traveltimeLong , data = norm_score_data_dummy)
stargazer(final_model, type="text") #Quick model comparison
summary(final_model)

# Showing assumption scores
plot(final_model, 1)
plot(final_model, 2)
plot(final_model, 3)
plot(final_model, 4)
##########################################################################################LOGISTIC REGRESSION MODELLING############################################

# Logistic regression model base on studytime correlating to pass or fail.
logistic_data <- norm_score_data

# first create dummy variable for pass and fail
logistic_data$passed = ifelse(logistic_data$mG3 >=10,1,0)
logistic_data$studytimeLong = ifelse(logistic_data$studytime.m >=3,1,0)
logistic_data$traveltimeLong = ifelse(logistic_data$traveltime.m >=3,1,0)
#Make sure categorical data is used as factors
logmodel1 <- glm(passed ~ studytimeLong + traveltimeLong + sex, data = logistic_data, na.action = na.exclude, family = binomial(link=logit))
#Full summary of the model
summary(logmodel1)
#Chi-square plus significance
lmtest::lrtest(logmodel1)
#Chi-square and Pseudo R2 calculation 
modelChi <- logmodel1$null.deviance - logmodel1$deviance
modelChi

pseudo.R2 <- modelChi / logmodel1$null.deviance
pseudo.R2

chidf <- logmodel1$df.null - logmodel1$df.residual
chidf


chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob


#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=logistic_data$passed ~ logistic_data$studytimeLong + logistic_data$sex, plot="ROC")


#Pseudo Rsquared 
DescTools::PseudoR2(logmodel1, which="CoxSnell")
DescTools::PseudoR2(logmodel1, which="Nagelkerke")

#Summary of the model with co-efficients
stargazer(logmodel1, type="text")


#confusion matrix
regclass::confusion_matrix(logmodel1)

#Collinearity
vifmodel<-car::vif(logmodel1)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
#Tolerance
1/vifmodel


#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok
generalhoslem::logitgof(logistic_data$passed, fitted(logmodel1))



# building logisitic regression on mG1, mG2, mG3
#Make sure categorical data is used as factors
logmodel2 <- glm(passed ~ mG1 + mG2 , data = logistic_data, na.action = na.exclude, family = binomial(link=logit))
summary(logmodel2)

#Chi-square plus significance
lmtest::lrtest(logmodel2)

#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=logistic_data$passed ~ logistic_data$mG1 + logistic_data$mG2, plot="ROC")

#Pseudo Rsquared 
DescTools::PseudoR2(logmodel2, which="CoxSnell")
DescTools::PseudoR2(logmodel2, which="Nagelkerke")
#Summary of the model with co-efficients
stargazer(logmodel2, type="text")

#confusion matrix
regclass::confusion_matrix(logmodel2)

#Collinearity
vifmodel<-car::vif(logmodel2)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
#Tolerance
1/vifmodel

#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok
generalhoslem::logitgof(logistic_data$passed, fitted(logmodel2))
