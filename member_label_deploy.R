library(RMySQL);library(plyr);library(dplyr);library(tidyr);library(ggplot2);library(reshape);library(reshape2);library(VIM)
library(mice);library(ROSE);library(fpc);library(VIM);library(mice);library(ROCR);library(randomForest);library(nnet)

rm(list = ls()); gc()
options(stringsAsFactors = F)

# set_env
setwd("D:/my_project/R_Project/mossel_test")
mossel <- dbConnect(MySQL(), user = "root", password = "chen1986", host = "127.0.0.1", db = "mossel")

# model
source("loss_warning_v7.R")
res_lw <- loss_warning_func(mydate = as.Date("2016-08-01"))

source("member_value_history_func_v4.R")
source("member_value_future_func_v2.R")
source("member_value_predict_func.R")
res_value <- member_value_predict(mydate = as.Date("2016-08-01"))

source("short_term_prefer_func_v2.R")
res_short <- short_term_prefer(my_date = as.Date("2016-08-01"))

source("long_term_prefer_func.R")
res_long <- long_term_prefer(my_date = as.Date("2016-08-01"))

source("edm_sensitivity_func.R")
res_edm <- edm_sensitivity(my_date = as.Date("2016-08-01"))


