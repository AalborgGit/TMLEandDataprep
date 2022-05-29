
#source("Data_cleaning.R")

## Longitudinal TMLE

# Packages
library(ltmle)
library(xgboost)


## Censoring variable between first and second examination c2 
# and second and third examination c3
c2 <- c3 <- rep(1,length=nrow(Data_wide_1)) %>% as.vector
for(i in 1:nrow(Data_wide_1)){
  if(is.na(Data_wide_1[i,"age_2"])){
     c2[i]<-0
  }
  if(is.na(Data_wide_1[i,"age_3"])){
    c3[i]<-0
  }
}

# the ltmle function requires that the censoring variable are factors 
# with levels censored and un censored 
# covert censoring variables to factors with these levels
c2 <- BinaryToCensoring(is.uncensored=c2)
c3 <- BinaryToCensoring(is.uncensored=c3)

Data_wide_cens <- cbind(Data_wide_1,c2,c3)

# the ltmle function requires a specific order of the variables
# thus we order the data set 
Data_wide_cens_order <- Data_wide_cens[,c(2:8,10:19,71:72,24,20:23,25:34,36,73,41,
                                          37:40,42:53,74,58,54:57,59:70,9)]

## Converts all factor variables to integers
cols_int <-c("sex","educ","death","angina","hospmi","MI_FCHD","anychd","cvd","hyperten",
             "bpmeds_1","bpmeds_2","bpmeds_3","diabetes_1","diabetes_2","diabetes_3",
             "prevap_1","prevap_2","prevap_3","prevchd_1","prevchd_2","prevchd_3","prevmi_1","prevmi_2","prevmi_3","prevstrk_1",
             "prevstrk_2","prevstrk_3","prevhyp_1","prevhyp_2","prevhyp_3")

Data_wide_cens_order_1 <- Data_wide_cens_order %>% mutate(across(cols_int,as.integer))

## The treatment A, censoring C variables and the outcome Y are specified 
anodes <- c("cursmoke_1","cursmoke_2","cursmoke_3")
cnodes <- c("c2","c3")
ynodes <- "stroke"
lnodes <- names(Data_wide_cens_order_1)[c(21:35,38:53,56:71)]

 
 ##Library of methods which the super learners shall use in L-TMLE to estimate 
 # Q and g
 sl_libs_G_g <- c('SL.randomForest','SL.xgboost')
 
 ## L-TMLE estimate of the average causal effect 
 set.seed(7)
 ltmle_est <- ltmle(Data_wide_cens_order_1, Anodes=anodes, Ynodes = ynodes, Cnodes=cnodes, 
                               Lnodes = lnodes, survivalOutcome = T, abar=list(c(1,1,1),c(0,0,0)), SL.library = sl_libs_G_g)
 
 summary(ltmle_est)
