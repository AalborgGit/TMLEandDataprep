library(tidyverse)
library(SuperLearner)# super learner package 
library(tmle3) #package with CV-tmle as difault
library(sl3) #super learner package  

#This code is based on #https://tlverse.org/tlverse-handbook/sl3.html 
#and https://tlverse.org/tlverse-handbook/tmle3.html


# Creates the data frame Data_wide_1_tmle from Data_wide_1 data frame 

cols_int <- c("stroke")

Data_wide_1_tmle <- Data_wide_1 %>% mutate(across(cols_int,as.integer))

# Converts cursmoke_1 variable, treatment A, to factor

Data_wide_1_tmle$cursmoke_1 <- as.factor(Data_wide_1_tmle$cursmoke_1)
levels(Data_wide_1_tmle$cursmoke_1) <- c("treated","untreated")

# Selects the baseline covariates W, cursmoke_1 as A and stroke as Y

Data_wide_1_tmle_subset <- Data_wide_1_tmle %>% select(names(Data_wide_1_tmle)[c(2:34,36,71:72)])

# The observed data is on the form O=(W,A,Y) which we define as the variable notes  
notes <- list(W=names(Data_wide_1_tmle_subset)[c(1:7,9:22,24:36)],Y="stroke",A="cursmoke_1")


# We now use the sl3 super learner package 

# First we get an overview of which methods fit for categorical and continuous variables
sl3_list_properties()
sl3_list_learners("categorical")
sl3_list_learners("continuous")


# Define the methods for the Super learners

lrnr_xgbo <- make_learner(Lrnr_xgboost)

lrnr_rf <- make_learner(Lrnr_randomForest)

lrnr_mean <- make_learner(Lrnr_mean)

lrn_ipredbagg <- Lrnr_pkg_SuperLearner$new("SL.ipredbagg")

# learner list for Super learners for Q and g 
sl_Y <- Lrnr_sl$new(
  learners = list(lrnr_xgbo,lrnr_rf,lrnr_mean,lrn_ipredbagg)
)
sl_A <- Lrnr_sl$new(
  learners = list(lrnr_xgbo,lrnr_rf,lrnr_mean,lrn_ipredbagg)
)
learner_list <- list(A = sl_A, Y = sl_Y)

# spec objekt for the tmle3 function to estimate average causal effect with CV-TMLE
ate_spec <- tmle_ATE(treatment_level="treated",control_level="untreated")

# Estimate the average causal effect with CV-TMLE 
set.seed(12)
tmle_est_cv <-tmle3(tmle_spec=ate_spec,data=Data_wide_1_tmle_subset,node_list=notes,learner_list =learner_list)

tmle_est_cv 
## Now use TMLE instead of CV-TMLE 
tmle_task <- ate_spec$make_tmle_task(Data_wide_1_tmle_subset,notes)

set.seed(12)
initial_likelihood <- ate_spec$make_initial_likelihood(tmle_task,learner_list)

# argument to not use the default CV-TMLE 
targeted_likelihood_no_cv <-Targeted_Likelihood$new(initial_likelihood,updater = list(cvtmle = FALSE))

tmle_params <- ate_spec$make_params(tmle_task, targeted_likelihood_no_cv)

# TMLE estimate of the average causal effect 
set.seed(12)
tmle_est_ucv <- fit_tmle3(tmle_task, targeted_likelihood_no_cv, tmle_params,
  targeted_likelihood_no_cv$updater
)

tmle_est_ucv

