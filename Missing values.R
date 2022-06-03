library(tidyverse)

##Load the Framingham Heart Study which is a longitudinal study 
Data <- read.csv("frmgham2.csv")

##Total number of missing values in the data set
sum(is.na.data.frame(Data))

##Identify number of missing values for each variable in the data set
NA_column<- lapply(1:ncol(Data),function(i){sum(is.na(Data[,i]))}) %>% unlist

##Find indices for the variables that contains missing values
index_NA_col <- which(NA_column>0)

##Corresponding missing values for the variables in index_NA_col
NA_column_minus_zero<- NA_column[index_NA_col]

##Names of the variables with missing values 
names_NA_column<- Data %>% select(all_of(index_NA_col)) %>% names

##Creates data frame for the variables with missing values and 
#corresponding number of missing values
NA_dataframe<- data.frame(names_NA_column,NA_column_minus_zero) 
colnames(NA_dataframe) <- c("Variable Name","Number of Missing Values")

##Arrange the variables in increasing order 
NA_dataframe<- NA_dataframe %>% arrange(across("Number of Missing Values"))
NA_dataframe %>% view()



