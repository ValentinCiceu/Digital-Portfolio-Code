# Code for the explore section. For rough work please look at the sandbox directory
# Essential pacakages needed for this section
library(e1071)   
library(psych)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(sqldf)
library(semTools)
library(pastecs)
library(sjstats) 
library(userfriendlyscience)

data <- read.csv("sperformance-dataset.csv", na.strings=c("na"," ","",".","NA"), header = TRUE)
# using sql sytax to make filtering easier to write, quicker and more readable
hyp1_score_data <- sqldf("select mg1,mg2,mg3,pg1,pg2,pg3 from data where mg1 > 0 and mg2 > 0 and mg3 > 0 and pg1 > 0 and pg2 > 0 and pg3 > 0")

# Better form of summary statisitcs in R usinf the psych library
describe(hyp1_score_data)

# To build visual normalisation charts
plot_list <- list()
for(i in colnames(hyp1_score_data)){
  print(mean(hyp1_score_data[,i]))
  gg <- ggplot(hyp1_score_data , aes_string(i))  
  gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
  gg<-gg+scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
  gg<-gg+stat_function(fun=dnorm, color="red",args=list(mean=mean(hyp1_score_data[,i], na.rm=TRUE), sd=sd(hyp1_score_data[,i], na.rm=TRUE)))
  plot_list[[i]] <- gg
} # end of loop

plot_grid(plotlist = plot_list)


# qq plot Ignore the error message, diagram still pops up
qqnorm(hyp1_score_data$mG1 ,main='mG1')+qqline(hyp1_score_data$mG1, col=2) #show a line on theplot

qqnorm(hyp1_score_data$mG2,main='mG2')+qqline(hyp1_score_data$mG2, col=2)


qqnorm(hyp1_score_data$mG3,main='mG3')+qqline(hyp1_score_data$mG3, col=2)



# Get the Kurtosis and Skew values of mG1
mG1_skew <- semTools::skew(hyp1_score_data$mG1)
mG1_kurt <- semTools::kurtosis(hyp1_score_data$mG1)

# standardise the values
mG1_skew[1]/mG1_skew[2]
mG1_kurt[1]/mG1_kurt[2]


# Get the Kurtosis and Skew values of mG2
mG2_skew <- semTools::skew(hyp1_score_data$mG2)
mG2_kurt <- semTools::kurtosis(hyp1_score_data$mG2)

# standardise the values
mG2_skew[1]/mG2_skew[2]
mG2_kurt[1]/mG2_kurt[2]



# Get the Kurtosis and Skew values of mG3
mG3_skew <- semTools::skew(hyp1_score_data$mG3)
mG3_kurt <- semTools::kurtosis(hyp1_score_data$mG3)

# standardise the values
mG3_skew[1]/mG3_skew[2]
mG3_kurt[1]/mG3_kurt[2]

mG1<- abs(scale(hyp1_score_data$mG1))

FSA::perc(as.numeric(mG1), 1.96, "gt")
FSA::perc(as.numeric(mG1), 3.29, "gt")


mG2<- abs(scale(hyp1_score_data$mG2))

FSA::perc(as.numeric(mG2), 1.96, "gt")
FSA::perc(as.numeric(mG2), 3.29, "gt")

mG1<- abs(scale(hyp1_score_data$mG3))

FSA::perc(as.numeric(mG1), 1.96, "gt")
FSA::perc(as.numeric(mG1), 3.29, "gt")

#Scatterplot relationship 
scatter <- ggplot(hyp1_score_data, aes(hyp1_score_data$mG1, hyp1_score_data$mG3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "First grades in maths (mG1)", y = "Final Grades Maths (mG3)") 

#Scatterplot relationship 
scatter <- ggplot(hyp1_score_data, aes(hyp1_score_data$mG2, hyp1_score_data$mG3))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "First grades in maths (mG2)", y = "Final Grades Maths (mG3)") 

#Pearson test between mG1 to mG3
stats::cor.test(hyp1_score_data$mG1, hyp1_score_data$mG3, method='pearson')


#Pearson test between mG2 and mG3
stats::cor.test(hyp1_score_data$mG2, hyp1_score_data$mG3, method='pearson')


# performing T-test
# First prepare the test
t_test_data <- sqldf("select * from data where mg1 > 0 and mg2 > 0 and mg3 > 0 and pg1 > 0 and pg2 > 0 and pg3 > 0")


# Performing T-test
# Describe the variables
psych::describeBy(t_test_data$mG3, t_test_data$sex, mat=TRUE)
# Using levene's test to test variance
car::leveneTest(mG3 ~ sex, data=t_test_data)

# Perfomring the T-test
stats::t.test(mG3~sex,var.equal=TRUE,data=t_test_data)


# Performing Cohen's d
res <- stats::t.test(mG3~sex,var.equal=TRUE,data=t_test_data)
effcd=round((2*res$statistic)/sqrt(res$parameter),2)
effectsize::t_to_d(t = res$statistic, res$parameter)

# T-Test for the second hypothesis involving more than two groups
# Describe the variables
psych::describeBy(t_test_data$mG3, t_test_data$guardian.m, mat=TRUE)

# Using levene's test to test variance
car::leveneTest(mG3 ~ guardian.m, data=t_test_data)


# Anova test for Hypothesis dealing with mG1 andf traveltime.m
# Check the statistical description of variable of interest
psych::describeBy(t_test_data$mG3, t_test_data$traveltime.m, mat=TRUE)


# performing Barrets test for homogenity of variance
stats::bartlett.test(mG3~ traveltime.m, data=t_test_data)


# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(t_test_data$traveltime.m),y=t_test_data$mG3,posthoc='Tukey')
anova_result


# access values in order to get f-statisitc on nect step
res2<-stats::aov(mG3~ traveltime.m, data = t_test_data)


fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat

# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value


# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta



# Anova test for studytime and Final grade in portuguese
# Check the statistical description of variable of interest
psych::describeBy(t_test_data$mG3, t_test_data$studytime.p, mat=TRUE)
# performing Barrets test for homogenity of variance
stats::bartlett.test(mG3~ studytime.p, data=t_test_data)
# One-way Anova test 
anova_result<-userfriendlyscience::oneway(as.factor(t_test_data$studytime.p),y=t_test_data$mG3,posthoc='Tukey')
anova_result
# access values in order to get f-statisitc on nect step
res2<-stats::aov(mG3~ studytime.p, data = t_test_data)
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
# Get the p-value
anova_p_value<-summary(res2)[[1]][["Pr(>F)"]][[1]]
anova_p_value
# Calculating the effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta