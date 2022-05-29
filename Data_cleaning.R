## Data cleaning

# Packages
library(tidyverse)
library(missForest)

##Load the Framingham Hart study which is a Longitudinal study
Data <- read.csv("frmgham2.csv")

## Convert the variables in 'cols' to factors
cols <- c("sex","cursmoke", "diabetes","bpmeds","educ","prevchd","prevap","prevmi",
          "prevstrk","prevhyp","death","angina","hospmi","MI_FCHD",
          "anychd","stroke","cvd","hyperten")

Data <- Data %>% mutate(across(cols,as.factor))

# Change levels for sex to 0,1 instead of 1,2
levels(Data$sex) <- c(0,1)

## Impute for the missing values of variables hdlc and ldlc corresponding to the maximal period (for each individual)
# Extract the rows corresponding to the maximal period from each individual
Data_period_max <- Data %>% group_by(randid) %>% filter(period==max(period)) %>% ungroup()

# Use missForest for imputation of the missing values of 'Data_period_max' 
# we do the calculations parallel to save time 
library(doParallel)
cor<- detectCores() 
registerDoParallel(cores=cor-1)
set.seed(12)
Data_period_max<- Data_period_max %>% as.data.frame()
Data_period_max_NA <- Data_period_max %>% missForest(maxiter = 10,parallelize ="forests")
Data_period_max_NA_imp <- Data_period_max_NA$ximp # extract the imputed values


# Variables hdlc and ldlc were integers in Data so we round off the values to the 
# nearest integer and convert them to intergers (class)
Data_period_max_NA_imp[,c("hdlc","ldlc")] <- round(Data_period_max_NA_imp[,c("hdlc","ldlc")])
Data_period_max_NA_imp[,"hdlc"] <- Data_period_max_NA_imp[,"hdlc"] %>% as.integer()
Data_period_max_NA_imp[,"ldlc"] <- Data_period_max_NA_imp[,"ldlc"] %>% as.integer()


# Extract the columns randid, period, hdlc and ldlc
Data_hdlc_ldlc <- Data_period_max_NA_imp %>% select(c(randid, period, hdlc,ldlc))

# Left join the original Data with Data_hdlc_ldlc such that the variables hdlc and ldlc
# from Data_hdlc_ldlc will be used instead of the original hdlc and ldlc variables
Data_join <- Data %>% left_join(Data_hdlc_ldlc, by=c("randid","period"), 
                                suffix=c("","_rf")) %>% select(-c("hdlc","ldlc"))


## Add rows for unattended examination/periods which are not due to right censoring
# identify which individuals have less than three corresponding rows 
rep_period_Data <- rle(Data$randid)
id_period <- rep_period_Data$values[which(rep_period_Data$lengths<3)]

# Extract these individuals from Data
Data_less3 <- Data %>% slice(which(randid %in% id_period))

# identify which individuals did not attend the third examinaiton
Data_minus_3 <- Data_less3 %>% group_by(randid) %>% filter(max(period)!=3) %>% ungroup()
id3 <- Data_minus_3$randid %>% unique()

# Add rows such that each individual has three rows; one for each examination/period
# notice: all variables of these rows except randid and period consist of missing values
Data_NA_new <- Data_join %>% complete(randid,period)

# Extract the rows corresponding to individuals who did not attend the third examination
# since these correspond to a right censoring
rows3 <- Data_NA_new %>% slice(which(randid %in% id3)) %>% filter(period==3)

# Remove these rows from Data_NA_new
Data_com_3 <- Data_NA_new %>% anti_join(rows3)

# Hence, we have remove all added rows corresponding to a right censoring in period three

# Identify which of the rigt censored individuals (id3) were already right censored in period 2 two
Data_com_3_NA2 <- Data_com_3 %>% slice(which(randid %in% id3)) %>% filter(period==2)
id2 <- which(is.na(Data_com_3_NA2$sex))

# Extract the rows
rows2 <- Data_com_3_NA2[id2,]

# Remove these rows from Data_com_3
Data_com_2 <- Data_com_3 %>% anti_join(rows2)

# Now we have remove all added rows corresponding to a right censoring

## Fill in values for fixed variables and impute for the remaining missing values

# Fill in the values for hdlc_rf and ldlc_rf (which we consider as fixed variables) 
Data_com_2 <- Data_com_2 %>% fill(c(hdlc_rf,ldlc_rf),.direction="up")

# Identify the remaining fixed variables(name_const)
wh <- which(names(Data_com_2) %in% c("randid","totchol","age","sysbp","diabp","cursmoke","cigpday","bmi","diabetes","bpmeds","heartrte","glucose","prevchd","prevap","prevmi","prevstrk","prevhyp","time", "hdlc_mice","ldlc_mice", "period"))
name_const <- names(Data_com_2)[-wh] %>% as.vector

# Fill in the values of these fixed variables 
Data_com_2 <- Data_com_2 %>% fill(name_const,.direction="down")

# Test which periods missing values for time and age occur since these can be calculated based
# on other periods
sum(is.na(Data_com_2$time))
sum(is.na(Data_com_2$age))
which(is.na(Data_com_2$age))==which(is.na(Data_com_2$time)) # same individuals have missing values of both variables
sum(Data_com_2[which(is.na(Data_com_2$time)), "period"]==2) # all missing values occur for period 2

# Calculate the values for the missing values of time and age
Dat1_time_age <- NULL
rand_ny <- Data_com_2$randid %>% unique()
for(i in rand_ny){
  Dat <- Data_com_2 %>% slice(which(Data_com_2$randid==i))
  if(any(is.na(Dat$time))){
    Dat[2,"time"] <- round((Dat[3,"time"]-Dat[1,"time"])/2)
    Dat[2,"age"] <- Dat[3,"age"]-6}
  Dat1_time_age <- rbind(Dat1_time_age,Dat)
}
Dat1_time_age <- Dat1_time_age %>% as.data.frame()

# Use missForest to impute for missing values
registerDoParallel(cores=cor-1)
set.seed(12)
Data_long <- Dat1_time_age %>% missForest(maxiter = 10,parallelize ="forests")
#save(Data_long,file="Data_long.Rdata")
Data_long_1 <- Data_long$ximp %>% as.data.frame() # extract the missing values

## Convert totchol,cigpday,heartrte,glucose to integer class
Data_long_1[,c("totchol","cigpday","heartrte","glucose")] <- round(Data_long_1[,c("totchol","cigpday","heartrte","glucose")])
Data_long_1[,"totchol"] <- Data_long_1[,"totchol"] %>% as.integer()
Data_long_1[,"cigpday"] <-Data_long_1[,"cigpday"] %>% as.integer()
Data_long_1[,"heartrte"] <- Data_long_1[,"heartrte"] %>% as.integer()
Data_long_1[,"glucose"] <-Data_long_1[,"glucose"] %>% as.integer()

## Round the variables sysbp and diabp to one dicimal and the variable bmi 
# to two dicimals 
Data_long_1[,c("sysbp","diabp")] <- round(Data_long_1[,c("sysbp","diabp")],1)
Data_long_1[,"bmi"] <- round(Data_long_1[,"bmi"],2)


## Convert from long format to wide format

Data_wide <- Data_long_1 %>% gather("var","obs", totchol,age,sysbp,diabp,cursmoke,cigpday,bmi,diabetes,bpmeds,heartrte,glucose,
                                  prevchd,prevap,prevmi,prevstrk,prevhyp,time) %>% unite("var_p",var,period,sep="_") %>% spread(var_p,obs)



## Converts variables in Data_wide to other classes 

# Selection of variables which we convert to factors 

cols_factor <-c("diabetes_1","diabetes_2","diabetes_3",
                "bpmeds_1","bpmeds_2","bpmeds_3","prevchd_1","prevchd_2","prevchd_3",
                "prevap_1","prevap_2","prevap_3","prevmi_1","prevmi_2","prevmi_3","prevstrk_1",
                "prevstrk_2","prevstrk_3","prevhyp_1","prevhyp_2","prevhyp_3")
Data_wide_1 <- Data_wide %>% mutate(across(cols_factor,as.factor))

# Selection of variables which we convert to numeric  

cols_num <- c("sysbp_1","sysbp_2","sysbp_3","diabp_1","diabp_2","diabp_3",
              "bmi_1","bmi_2","bmi_3","cursmoke_1","cursmoke_2","cursmoke_3","stroke")


Data_wide_1 <- Data_wide_1 %>% mutate(across(cols_num,as.numeric))

# stroke as a binary variable which atains values 0 or 1.
Data_wide_1$stroke <- Data_wide_1$stroke -1


# Selection of variables which we convert to integers 

cols_int <- c("totchol_1","totchol_2","totchol_3","age_1","age_2","age_3",
              "cigpday_1","cigpday_2","cigpday_3",
              "heartrte_1","heartrte_2","heartrte_3","glucose_1","glucose_2",
              "glucose_3","time_1","time_2","time_3")

Data_wide_1 <- Data_wide_1 %>% mutate(across(cols_int,as.integer))

## sort the variables 
Data_wide_1 <- Data_wide_1 %>% select(ends_with("_1"), ends_with("_2"), ends_with("_3"), everything()) %>% select(randid:timehyp, everything())


#save(Data_wide_1, file = "Data_wide_1.Rdata")
