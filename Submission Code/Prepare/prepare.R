# Prepare section of the assignment. This is the cleaned version. For rough work look at the sandbox directory 
library(e1071)   
library(psych)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)

#make sure the data is in the local directory with this file
data <- read.csv("sperformance-dataset.csv", na.strings=c("na"," ","",".","NA"), header = TRUE)
# get useful statistical descriptions of the data
summary(data)
# Better form of summary statisitcs in R usinf the psych library
describe(data)

# Distribution graphs to get a sense of how the numerical values are represented
gg<-ggplot(data,aes(age))
gg<-gg+labs(x="age")
gg<-gg+geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg<-gg+scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg<-gg+stat_function(fun=dnorm, color="red",args=list(mean=mean(data$age, na.rm=TRUE), sd=sd(data$age, na.rm=TRUE)))
gg

# plotting for key variables of interest
col_names = c("absences.m" , "absences.p", "pG1","pG2","pG3","mG1","mG2","mG3")
# hold all the plots created in the loop
plot_list <- list()
for(i in col_names){
  print(mean(data[,i]))
  gg <- ggplot(data , aes_string(i))  
  gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
  gg<-gg+scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
  gg<-gg+stat_function(fun=dnorm, color="red",args=list(mean=mean(data[,i], na.rm=TRUE), sd=sd(data[,i], na.rm=TRUE)))
  plot_list[[i]] <- gg
} # end of loop
plot_grid(plotlist = plot_list)



#Boxplots for more information visually
# hold all theplots created in the loop
box_plot_list <- list()
for(i in col_names){
  print(i)
  gg <- ggplot(data, aes_string(y=i)) + geom_boxplot() + theme(text = element_text(size=20))
  box_plot_list[[i]] <- gg
} # end of loop
plot_grid(plotlist = box_plot_list)


# Categorical data description
cat_data <- data[,-which(names(data) %in% c("absences.m" , "absences.p", "pG1","pG2","pG3","mG1","mG2","mG3"))]


# Categorical data barcharts
bar_cat_plot_list <- list()
col_names <- colnames(cat_data)
for(i in col_names){
  #     print(i)
  gg <- ggplot(cat_data, aes_string(x = i)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=5))
  bar_cat_plot_list[[i]] <- gg
} # end of loop

plot_grid(plotlist = bar_cat_plot_list)









