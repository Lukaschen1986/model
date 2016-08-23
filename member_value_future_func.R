dist_func <- function(x){
          dist <- (max(x,na.rm = T)-min(x,na.rm = T))/3
          c1 <- min(x, na.rm = T)+dist
          c2 <- min(x, na.rm = T)+dist*2
          
          y <- c()
          y[x >= c2] <- 1
          y[x >= c1 & x < c2] <- 2
          
          if(min(x) %in% NA){
                    y[x < c1] <- 3
                    y[x %in% NA] <- 4
          }else{
                    y[x > min(x, na.rm = T) & x < c1] <- 3
                    y[x == min(x, na.rm = T)] <- 4
          }
          return(y)
}

member_value_future <- function(df1, df2, df3, df4, df5, df6, df7){
          # load_data
          member_sql <- paste("select member_id, member_lv_id, point, experience, sex, 
                     from_unixtime(regtime,'%Y-%m-%d') as regtime
                    from sdb_b2c_members where regtime <> '0'")
          members <- dbGetQuery(mossel, member_sql)
          
          # merge_data
          # df1 = res_history_1; df2 = res_history_2; df3 = res_history_3; df4 = res_history_4; df5 = res_history_5; df6 = res_history_6; df7 = res_history_7
          members_s <- merge(members, df1[,c("member_id","score")], by = "member_id", all.x = T); names(members_s)[7] <- c("s1")
          members_s <- merge(members_s, df2[,c("member_id","score")], by = "member_id", all.x = T); names(members_s)[8] <- c("s2")
          members_s <- merge(members_s, df3[,c("member_id","score")], by = "member_id", all.x = T); names(members_s)[9] <- c("s3")
          members_s <- merge(members_s, df4[,c("member_id","score")], by = "member_id", all.x = T); names(members_s)[10] <- c("s4")
          members_s <- merge(members_s, df5[,c("member_id","score")], by = "member_id", all.x = T); names(members_s)[11] <- c("s5")
          members_s <- merge(members_s, df6[,c("member_id","score")], by = "member_id", all.x = T); names(members_s)[12] <- c("s6")
          members_s <- merge(members_s, df7[,c("member_id","score")], by = "member_id", all.x = T); names(members_s)[13] <- c("s7")
          
          members_dead <- subset(members_s, 
                                 members_s[,7] %in% NA 
                                 & members_s[,8] %in% NA 
                                 & members_s[,9] %in% NA
                                 & members_s[,10] %in% NA
                                 & members_s[,11] %in% NA
                                 & members_s[,12] %in% NA
                                 & members_s[,13] %in% NA)
          members_alive <- subset(members_s, !member_id %in% members_dead$member_id)
          
          df <- members_alive[,c("member_id","s1","s2","s3","s4","s5","s6","s7")]
          
          # 计算区间
          df_discrete <- data.frame(member_id = df$member_id,
                                    s1 = dist_func(df$s1),
                                    s2 = dist_func(df$s2),
                                    s3 = dist_func(df$s3),
                                    s4 = dist_func(df$s4),
                                    s5 = dist_func(df$s5),
                                    s6 = dist_func(df$s6),
                                    s7 = dist_func(df$s7))
          
          # 计算状态转化矩阵
          dm <- matrix(data = rep(0,16), nrow = 4, ncol = 4)
          rownames(dm) <- c(1,2,3,4)
          colnames(dm) <- c(1,2,3,4)
          
          for(i in 1:nrow(df_discrete)){
                    for(j in 2:(ncol(df_discrete)-1)){
                              dm[df_discrete[i,j], df_discrete[i,j+1]] <- dm[df_discrete[i,j], df_discrete[i,j+1]] + 1
                    }
          }
          dm_p <- dm/apply(dm, 1, sum)
          
          # 定义历史数据最后一期类别初始值
          dm_s7 <- matrix(data = rep(0, nrow(df_discrete)*4), nrow = nrow(df_discrete), ncol = 4)
          dm_s7[,1][df_discrete$s7 == 1] <- 1
          dm_s7[,2][df_discrete$s7 == 2] <- 1
          dm_s7[,3][df_discrete$s7 == 3] <- 1
          dm_s7[,4][df_discrete$s7 == 4] <- 1
          
          # 计算未来三期转化概率
          dm_s8_pred <- dm_s7 %*% dm_p
          dm_s9_pred <- dm_s8_pred %*% dm_p
          dm_s10_pred <- dm_s9_pred %*% dm_p
          
          # 计算历史数据最后一期得分均值
          df_s7 <- cbind(df[,c("member_id","s7")], label = df_discrete[,c("s7")])
          df_s7$s7[df_s7$s7 %in% NA] <- 0
          
          avg_7 <- data.frame(score = tapply(df_s7$s7, df_s7$label, mean))
          avg_7$rowname <- row.names(avg_7)
          mean_7 <- as.matrix(data.frame(L1 = if(1 %in% avg_7$rowname){subset(avg_7, rowname == 1)$score}else{0},
                                         L2 = if(2 %in% avg_7$rowname){subset(avg_7, rowname == 2)$score}else{0},
                                         L3 = if(3 %in% avg_7$rowname){subset(avg_7, rowname == 3)$score}else{0},
                                         L4 = if(4 %in% avg_7$rowname){subset(avg_7, rowname == 4)$score}else{0}))
          df$score_pred_8 <- t(mean_7 %*% t(dm_s8_pred))
          df$score_pred_9 <- t(mean_7 %*% t(dm_s9_pred))
          df$score_pred_10 <- t(mean_7 %*% t(dm_s10_pred))
          # table(df$score_pred_10)
          
          # df$label_pred <- dist_func(df$score_pred_10)
          # table(df$label_pred)
          
          res <- df[,c("member_id","score_pred_10")]
          return(res)
}
