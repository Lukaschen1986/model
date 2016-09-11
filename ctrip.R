library(RMySQL);library(plyr);library(dplyr);library(tidyr);library(ggplot2);library(reshape);library(reshape2);library(VIM)
library(mice);library(ROSE);library(fpc);library(VIM);library(mice);library(ROCR);library(randomForest);library(nnet)

rm(list = ls()); gc()
options(stringsAsFactors = F)

# load_data
# jdfeycdata_train <- read.table(file = "jdfeycdata_train.txt", header = T)
# jdfeycdata_train$total_quantity <- jdfeycdata_train$b_quantity_app+jdfeycdata_train$c_quantity_app+jdfeycdata_train$e_quantity_app
# jdfeycdata_train$checkindate <- as.Date(jdfeycdata_train$checkindate)
# jdfeycdata_train$b_bfcms_app <- NULL; jdfeycdata_train$c_bfcms_app <- NULL; jdfeycdata_train$e_bfcms_app <- NULL
# jdfeycdata_train_2 <- subset(jdfeycdata_train, b_price_after_app > 100 & c_price_after_app > 100 & e_price_after_app > 100)
# save(jdfeycdata_train_2, file = "jdfeycdata_train.RData")
load("D:/my_project/R_Project/ctrip/jdfeycdata_train.RData")
# load("D:/kuaipan/TEST/huawei/jdfeycdata_train.RData")
jdfeycdata_train_2$b_hotel <- as.character(jdfeycdata_train_2$b_hotel)

# filter_data
quantity_agg <- aggregate(total_quantity ~ b_hotel, data = jdfeycdata_train_2, FUN = sum)
quantity_agg <- arrange(quantity_agg, desc(total_quantity))
hotel_list <- quantity_agg[1:round(nrow(quantity_agg)/4),1]

quantity_predict <- function(mydata = jdfeycdata_train_2, hotel_num){
          # func
          data_divide <- function(df, divide_1 = 0.8){
                    idx <- sample(1:nrow(df), nrow(df)*divide_1)
                    train <- df[idx,]
                    valid <- df[-idx,]
                    
                    res <- list(train = train, valid = valid)
                    return(res)
          }
          # filter_hotel
          df <- subset(mydata, b_hotel == hotel_num)
          # df <- subset(jdfeycdata_train_2, b_hotel == 473698)
          df$weekday <- format(df$checkindate, format = "%a")
          df$weekday_2[df$weekday %in% c("周一","周二","周三","周四","周五")] <- 0
          df$weekday_2[df$weekday %in% c("周六","周日")] <- 1
          
          roomid_list <- data.frame(table(df$masterbasicroomid))
          roomid_list$Var1 <- as.integer(as.character(roomid_list$Var1))
          
          text_1 <- c(); text_2 <- c()
          for(i in 1:(nrow(roomid_list)-1)){
                    text_1 <- rbind(text_1, paste("df$roomid_dumm_", i, " <- 0", sep = ""))
                    text_2 <- rbind(text_2, paste("df$roomid_dumm_", i, "[df$masterbasicroomid == ", roomid_list[i,1], "] <- 1", sep = ""))
                    text <- rbind(text_1, text_2)
          }
          for(j in 1:nrow(text)){eval(parse(text = text[j,]))}
          
          df_2 <- subset(df, checkindate <= "2016-03-24")
          df_3 <- data_divide(df = df_2, divide_1 = 0.8)
          train <- df_3$train
          valid <- df_3$valid
          test <- subset(df, checkindate >= "2016-03-25")
          
          if(nrow(train) <= 30 | nrow(valid) <= 15 | nrow(test) <= 10){return(NULL)}
          
          train_2 <- cbind(total_quantity = train$total_quantity, 
                           select(train, -checkindate,-masterhotelid,-b_hotel,-city,-masterbasicroomid,-star,-b_quantity_app,-c_quantity_app,-e_quantity_app,-roomquantity,-total_quantity,-weekday))
          valid_2 <- cbind(total_quantity = valid$total_quantity, 
                           select(valid, -checkindate,-masterhotelid,-b_hotel,-city,-masterbasicroomid,-star,-b_quantity_app,-c_quantity_app,-e_quantity_app,-roomquantity,-total_quantity,-weekday))
          test_2 <- cbind(total_quantity = test$total_quantity, 
                          select(test, -checkindate,-masterhotelid,-b_hotel,-city,-masterbasicroomid,-star,-b_quantity_app,-c_quantity_app,-e_quantity_app,-roomquantity,-total_quantity,-weekday))
          
          y_train <- train_2$total_quantity
          X_train <- as.matrix(cbind(b0 = 1, scale(train_2[,2:4]), train_2[,5:ncol(train_2)]))
          K_train <- (5 + 0.0001*X_train%*%t(X_train))^2
          
          y_valid <- valid_2$total_quantity
          X_valid <- as.matrix(cbind(b0 = 1, 
                                     (valid_2[,2:4]-apply(train_2[,2:4],2,mean))/apply(train_2[,2:4],2,sd), 
                                     valid_2[,5:ncol(valid_2)]))
          K_valid <- (5 + 0.0001*X_valid%*%t(X_train))^2
          
          y_test <- test_2$total_quantity
          X_test <- as.matrix(cbind(b0 = 1, 
                                    (test_2[,2:4]-apply(train_2[,2:4],2,mean))/apply(train_2[,2:4],2,sd), 
                                    test_2[,5:ncol(test_2)]))
          K_test <- (5 + 0.0001*X_test%*%t(X_train))^2
          
          # ridge
          lam <- exp(1)^seq(from = -10, to = 5, length.out = 1000)
          n <- length(lam); p <- ncol(X_train)
          w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
          
          for(i in 1:length(lam)){
                    w[i,] <- t(solve(t(X_train)%*%X_train+diag(ncol(X_train))*lam[i]) %*% t(X_train) %*% log(y_train))
          }
          pred_valid <- round(exp(X_valid %*% t(w)))
          
          err_valid <- c()
          for(j in 1:ncol(pred_valid)){
                    err_valid[j] <- sum(abs(pred_valid[,j]-y_valid)/y_valid)/length(y_valid)
          }
          plot(err_valid, type = "b")
          err_valid_final <- min(err_valid)
          lam_ridge <- lam[which.min(err_valid)]
          w_ridge <- w[which.min(err_valid),]
          pred_ridge <- round(exp(X_valid %*% w_ridge))
          
          # svr
          lam <- exp(1)^seq(from = -10, to = 5, length.out = 1000)
          n <- length(lam); p <- ncol(K_train)
          b <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
          
          for(i in 1:length(lam)){
                    b[i,] <- t(solve(K_train+diag(ncol(K_train))*lam[i])%*%log(y_train))
          }
          pred_valid <- round(exp(K_valid%*%t(b)))
          
          err_valid <- c()
          for(j in 1:ncol(pred_valid)){
                    err_valid[j] <- sum(abs(pred_valid[,j]-y_valid)/y_valid)/length(y_valid)
          }
          plot(err_valid, type = "b")
          err_valid_final <- min(err_valid)
          lam_svr <- lam[which.min(err_valid)]
          b_svr <- b[which.min(err_valid),]
          pred_svr <- round(exp(K_valid%*%b_svr))
          
          # possion
          vctr_idnt <- function(x){x/sqrt(sum(x^2))}
          lam <- seq(from = lam_ridge/5, to = lam_ridge*50, length.out = 15)
          pred_valid <- c(); w_2 <- c()
          
          for(j in 1:length(lam)){
                    n <- 10000; p <- ncol(X_train)
                    w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
                    e_in <- c(1); yita <- 0.1 # 0.001, 0.01, 0.03, 0.1, 0.3, 1, 3, 10
                    
                    for(i in 2:nrow(w)){
                              w[i,] <- w[i-1,] - yita*vctr_idnt(-t(y_train)%*%X_train+t(exp(X_train%*%w[i-1,]))%*%X_train+lam[j]*w[i-1,])
                              e_in[i] <- -sum(y_train*X_train%*%w[i,] - exp(X_train%*%w[i,]))+(t(w[i,])%*%w[i,])*lam[j]
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    plot(e_in[-1], type = "b")
                    w_2 <- rbind(w_2, w[which.min(e_in[-1])+1,])
                    pred_valid <- cbind(pred_valid, round(exp(X_valid%*%w_2[j,])))
          }
          
          err_valid <- c()
          for(j in 1:ncol(pred_valid)){
                    err_valid[j] <- sum(abs(pred_valid[,j]-y_valid)/y_valid)/length(y_valid)
          }
          plot(err_valid, type = "b")
          err_valid_final <- min(err_valid)
          lam_pssn <- lam[which.min(err_valid)]
          w_pssn <- w_2[which.min(err_valid),]
          pred_pssn <- round(exp(X_valid %*% w_pssn))
          
          # blending
          a <- ifelse(sum(pred_ridge) == 0, 0, 1)
          b <- ifelse(sum(pred_svr) == 0, 0, 1)
          c <- ifelse(sum(pred_pssn) == 0, 0, 1)
          num <- a+b+c
          
          fr <- function(x){
                    x1 <- x[1]; x2 <- x[2]; x3 <- x[3]
                    sum(abs((pred_ridge*x1+pred_svr*x2+pred_pssn*x3)/num-y_valid)/y_valid)/length(y_valid)
          }
          opt <- optim(par = c(1,1,1), fn = fr, gr = NULL, method = "BFGS")
          
          # predict
          predict_ridge <- round(exp((X_test%*%w_ridge)))
          predict_svr <- round(exp((K_test%*%b_svr)))
          predict_pssn <- round(exp((X_test%*%w_pssn)))
          predict_blending <- round((predict_ridge*opt$par[1]+predict_svr*opt$par[2]+predict_pssn*opt$par[3])/num)
          
          err_ridge <- sum(abs(predict_ridge-y_test)/y_test)/length(y_test)
          err_svr <- sum(abs(predict_svr-y_test)/y_test)/length(y_test)
          err_pssn <- sum(abs(predict_pssn-y_test)/y_test)/length(y_test)
          err_blending <- sum(abs(predict_blending-y_test)/y_test)/length(y_test)
          
          err_data <- data.frame(b_hotel = unique(df$b_hotel),
                                 n_roomid = length(unique(df$masterbasicroomid)),
                                 n_train = nrow(X_train),
                                 n_valid = nrow(X_valid),
                                 n_test = nrow(X_test),
                                 avg_quan_train = round(mean(y_train)),
                                 avg_quan_valid = round(mean(y_valid)),
                                 avg_quan_test = round(mean(y_test)),
                                 err_ridge = err_ridge,
                                 err_svr = err_svr,
                                 err_pssn = err_pssn,
                                 err_blending = err_blending)
          return(err_data)
}
quantity_predict(mydata = jdfeycdata_train_2, hotel_num = hotel_list[1])

# loop
err_df <- c()
for(i in 1:length(hotel_list)){
          res <- quantity_predict(mydata = jdfeycdata_train_2, hotel_num = hotel_list[i])
          err_df <- rbind(err_df, res)
          # write.table(res, file = "err_df.csv", append = T, quote = F, sep = ",", row.names = F, col.names = F)
}
write.csv(err_df, file = "err_df_2.csv", row.names = F)

# parallel
library(parallel)
cl <- makeCluster(getOption(x = "cl.cores", default = detectCores()))
clusterExport(cl, "select")

t_start <- Sys.time()
writes <- parApply(cl, as.matrix(data.frame(hotel_num = hotel_list)), 
                   MARGIN = 1, 
                   FUN = quantity_predict, 
                   mydata = jdfeycdata_train_2)
delta_t <- Sys.time()-t_start
stopCluster(cl)

result <- c()
for(i in 1:length(writes)){
          result <- rbind(result, writes[[i]])
}
