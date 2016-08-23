member_value_predict <- function(mydate){
          # 历史数据验证，获取最优系数
          statistic_date <- mydate
          # statistic_date = as.Date("2016-08-01") 
          histy_date_7 <- statistic_date-90+1-as.POSIXlt(statistic_date-90)$mday
          histy_date_6 <- histy_date_7-as.POSIXlt(histy_date_7-1)$mday
          histy_date_5 <- histy_date_6-as.POSIXlt(histy_date_6-1)$mday
          histy_date_4 <- histy_date_5-as.POSIXlt(histy_date_5-1)$mday
          histy_date_3 <- histy_date_4-as.POSIXlt(histy_date_4-1)$mday
          histy_date_2 <- histy_date_3-as.POSIXlt(histy_date_3-1)$mday
          histy_date_1 <- histy_date_2-as.POSIXlt(histy_date_2-1)$mday
          
          res_history_1 <- member_value_history(mydate = histy_date_1, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1)
          res_history_2 <- member_value_history(mydate = histy_date_2, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1)
          res_history_3 <- member_value_history(mydate = histy_date_3, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1) 
          res_history_4 <- member_value_history(mydate = histy_date_4, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1)
          res_history_5 <- member_value_history(mydate = histy_date_5, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1)
          res_history_6 <- member_value_history(mydate = histy_date_6, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1)
          res_history_7 <- member_value_history(mydate = histy_date_7, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1)
          
          res_furure <- member_value_future(res_history_1,res_history_2,res_history_3,res_history_4,res_history_5,res_history_6,res_history_7)
          
          res_history_valid <- member_value_history(mydate = as.Date(statistic_date), w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1)
          res_history_valid <- merge(res_history_valid, members, by = "member_id", all.x = T)
          res_history_valid$regtime <- as.Date(res_history_valid$regtime)
          res_history_valid <- subset(res_history_valid, regtime < histy_date_7 & member_id %in% res_furure$member_id)
          
          res_furure_2 <- merge(res_furure, res_history_valid, by = "member_id")
          #res_furure_3 <- merge(res_furure, res_history_valid, by = "member_id", all.x = T)
          #res_furure_2$score[res_furure_2$score %in% NA] <- res_furure_2$score_pred_10[res_furure_2$score %in% NA]
          #res_furure_2$label[res_furure_2$label %in% NA] <- 4
          
          fr <- function(x){
                    x1 <- x[1]
                    sum((res_furure_2$score_pred_10*x1-res_furure_2$score)^2)/nrow(res_furure_2)
          }
          optim_func <- optim(par = c(1), fn = fr, method = "BFGS")
          
          # 验证准确率
          res_furure_2$score_wt <- res_furure_2$score_pred_10*optim_func$par[1]
          sse <- sum((res_furure_2$score_wt-res_furure_2$score)^2)/nrow(res_furure_2)
          
          dist <- (max(res_furure_2$score_wt)-min(res_furure_2$score_wt))/3
          c1 <- min(res_furure_2$score_wt)+dist
          c2 <- min(res_furure_2$score_wt)+dist*2
          res_furure_2$label_pred[res_furure_2$score_wt >= c2] <- 1
          res_furure_2$label_pred[res_furure_2$score_wt >= c1 & res_furure_2$score_wt < c2] <- 2
          res_furure_2$label_pred[res_furure_2$score_wt < c1] <- 3
          
          # table(res_furure_2$label,res_furure_2$label_pred)
          accu <- sum(res_furure_2$label_pred == res_furure_2$label)/nrow(res_furure_2)
          
          # predict
          histy_date_2nd <- statistic_date-as.POSIXlt(statistic_date-1)$mday
          histy_date_1st <- histy_date_2nd-as.POSIXlt(histy_date_2nd-1)$mday
          
          res_history_pred_1st <- member_value_history(mydate = histy_date_1st, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1) # "2016-06-01"
          res_history_pred_2nd <- member_value_history(mydate = histy_date_2nd, w1 = 0.3, w2 = 0.4, w3 = 0.2, w4 = 0.1) # "2016-07-01"
          # res_history_valid <- member_value_history(mydate = as.Date(statitic_date), w1 = 0.1, w2 = 0.25, w3 = 0.15, w4 = 0.1, w5 = 0.4) # "2016-08-01"
          res_furure_real <- member_value_future(res_history_4,res_history_5,res_history_6,res_history_7,res_history_pred_1st,res_history_pred_2nd,res_history_valid)
          
          # 计算预测值
          res_furure_real$score_wt <- res_furure_real$score_pred_10*optim_func$par[1]
          res_furure_real$label_pred <- dist_func(res_furure_real$score_wt)
          table(res_furure_real$label_pred)
          
          res <- list(sse = sse,
                      accu = accu,
                      member_list = res_furure_real[,c("member_id","score_wt","label_pred")])
          return(res)
}
