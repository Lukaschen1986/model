na_func <- function(df){
          for(i in 1:ncol(df)){
                    df[,i][df[,i] %in% NA] <- 0
          }
          return(df)
}

scale_func <- function(df){
          for(i in 2:ncol(df)){
                    df[,i] <- (df[,i]-min(df[,i]))/(max(df[,i])-min(df[,i]))
          }
          return(df)
}

short_term_prefer <- function(my_date){
          # set time
          statistic_date <- my_date
          # statistic_date <- as.Date("2016-08-01")
          begin_date <- statistic_date-90+1-as.POSIXlt(statistic_date-90)$mday
          end_date <- statistic_date-as.POSIXlt(statistic_date)$mday
          
          sql_1 <- paste("select order_id, member_id, final_amount, cost_freight, pmt_order, 
                         from_unixtime(createtime,'%Y-%m-%d') as create_date 
                         from sdb_b2c_orders 
                         where pay_status = '1' 
                         and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
                         "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "';", sep = "")
          
          sql_2 <- paste("select a.member_id, a.final_amount, a.cost_freight, a.pmt_order, b.*, a.create_date
                         from
                         (select order_id, member_id, final_amount, cost_freight, pmt_order, 
                         from_unixtime(createtime,'%Y-%m-%d') as create_date
                         from sdb_b2c_orders 
                         where pay_status = '1'
                         and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
                         "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "')a
                         inner join
                         (select order_id, type_id, product_id, bn, nums, g_price, amount from sdb_b2c_order_items)b 
                         on a.order_id = b.order_id;", sep = "")
          
          member_sql <- paste("select member_id, member_lv_id, email, point, experience, sex, 
                              from_unixtime(regtime,'%Y-%m-%d') as regtime
                              from sdb_b2c_members where regtime <> '0'")
          
          orders <- dbGetQuery(mossel, sql_1)
          orders$order_id <- as.character(orders$order_id)
          orders$member_id <- as.character(orders$member_id)
          
          order_items <- dbGetQuery(mossel, sql_2)
          order_items$order_id <- as.character(order_items$order_id)
          order_items$member_id <- as.character(order_items$member_id)
          
          sku <- read.csv(file = "sku.csv")
          
          order_items <- merge(x = order_items, y = sku, by = "bn")
          order_items$unit_mult <- order_items$nums*order_items$unit
          
          members <- dbGetQuery(mossel, member_sql)
          scrtry_email <- read.csv(file = "scrtry_email.csv")
          scrtry_member <- merge(members, scrtry_email, by = "email")
          
          # delete outliers
          mult_agg <- aggregate(unit_mult ~ order_id, data = subset(order_items, sku_type != 4), FUN = sum)
          mult_agg$scale <- scale(mult_agg$unit_mult, center = T, scale = T)
          mult_agg <- subset(mult_agg, scale < 2 & unit_mult >= 1)
          
          order_agg <- aggregate(order_id ~ member_id, data = orders, FUN = length)
          order_agg$scale <- scale(order_agg$order_id, center = T, scale = T)
          order_agg <- subset(order_agg, scale < 2 & order_id >= 1)
          
          orders <- subset(orders, order_id %in% mult_agg$order_id & final_amount > 0 & !member_id %in% scrtry_member$member_id & member_id %in% order_agg$member_id)
          order_items <- subset(order_items, order_id %in% mult_agg$order_id & final_amount > 0 & !member_id %in% scrtry_member$member_id & sku_type != 4 & member_id %in% order_agg$member_id)
          
          member_list <- data.frame(member_id = unique(orders$member_id))
          
          # category_prefer
          agg_wine_mult <- aggregate(unit_mult ~ member_id, data = subset(order_items, sku_type %in% c(1,2,5,7)), sum)
          names(agg_wine_mult) <- c("member_id", "wine_unit_mult")
          agg_beef_mult <- aggregate(unit_mult ~ member_id, data = subset(order_items, sku_type %in% c(8,9)), sum)
          names(agg_beef_mult) <- c("member_id", "beef_unit_mult")
          agg_white_mult <- aggregate(unit_mult ~ member_id, data = subset(order_items, type_id == 3), sum)
          names(agg_white_mult) <- c("member_id", "white_unit_mult")
          
          agg_wine_order <- aggregate(sku_type ~ member_id + order_id, data = subset(order_items, sku_type %in% c(1,2,5,7)), length)
          agg_wine_order <- aggregate(order_id ~ member_id, data = agg_wine_order, length)
          names(agg_wine_order) <- c("member_id", "wine_unit_order")
          agg_beef_order <- aggregate(sku_type ~ member_id + order_id, data = subset(order_items, sku_type %in% c(8,9)), length)
          agg_beef_order <- aggregate(order_id ~ member_id, data = agg_beef_order, length)
          names(agg_beef_order) <- c("member_id", "beef_unit_order")
          agg_white_order <- aggregate(sku_type ~ member_id + order_id, data = subset(order_items, type_id == 3), length)
          agg_white_order <- aggregate(order_id ~ member_id, data = agg_white_order, length)
          names(agg_white_order) <- c("member_id", "white_unit_order")
          
          df_merge <- merge(member_list, agg_wine_mult, by = "member_id", all = T)
          df_merge <- merge(df_merge, agg_wine_order, by = "member_id", all = T)
          df_merge <- merge(df_merge, agg_beef_mult, by = "member_id", all = T)
          df_merge <- merge(df_merge, agg_beef_order, by = "member_id", all = T)
          df_merge <- merge(df_merge, agg_white_mult, by = "member_id", all = T)
          df_merge <- merge(df_merge, agg_white_order, by = "member_id", all = T)
          
          df_merge <- na_func(df = df_merge)
          df_merge <- scale_func(df = df_merge)
          df_merge$score_wine <- df_merge$wine_unit_mult*0.3+df_merge$wine_unit_order*0.7
          df_merge$score_beef <- df_merge$beef_unit_mult*0.3+df_merge$beef_unit_order*0.7
          df_merge$score_white <- df_merge$white_unit_mult*0.3+df_merge$white_unit_order*0.7
          
          wine_prefer <- subset(df_merge, score_wine >= quantile(df_merge$score_wine, probs = 0.5) & score_wine > score_beef & score_wine > score_white & score_wine > 0)$member_id
          beef_prefer <- subset(df_merge, score_beef >= quantile(df_merge$score_beef, probs = 0.5) & score_beef > score_wine & score_beef > score_white & score_beef > 0)$member_id
          white_prefer <- subset(df_merge, score_white >= quantile(df_merge$score_white, probs = 0.5) & score_white > score_wine & score_white > score_beef & score_white > 0)$member_id
          
          member_list$category_prefer <- NULL
          member_list$category_prefer[member_list$member_id %in% wine_prefer] <- 1
          member_list$category_prefer[member_list$member_id %in% beef_prefer] <- 2
          member_list$category_prefer[member_list$member_id %in% white_prefer] <- 3
          member_list$category_prefer[member_list$category_prefer %in% NA] <- 4
          table(member_list$category_prefer)
          
          # categroup_prefer:wine
          # agg_wine_group <- aggregate(unit_mult ~ member_id + sku_type, data = subset(order_items, sku_type %in% c(1,2,5)), sum)
          agg_wine_group <- aggregate(unit_mult ~ member_id + sku_type, 
                                      data = subset(order_items, sku_type %in% c(1,2,5,7) 
                                                    & member_id %in% subset(member_list, category_prefer == 1)$member_id), 
                                      sum)
          agg_wine_group_cast <- cast(agg_wine_group, member_id ~ sku_type, value = "unit_mult", fill = 0) 
          agg_wine_group_cast$group_max <- apply(agg_wine_group_cast,1,max)
          agg_wine_group_cast_logic <- agg_wine_group_cast[,2:5] == agg_wine_group_cast[,6]
          
          categroup_prefer_wine <- c()
          for(i in 1:nrow(agg_wine_group_cast_logic)){
                    if(agg_wine_group_cast_logic[i,1]==T & sum(agg_wine_group_cast_logic[i,])==1){
                              categroup_prefer_wine[i]<-1
                    }else if(agg_wine_group_cast_logic[i,2]==T & sum(agg_wine_group_cast_logic[i,])==1){
                              categroup_prefer_wine[i]<-2
                    }else if(agg_wine_group_cast_logic[i,3]==T & sum(agg_wine_group_cast_logic[i,])==1){
                              categroup_prefer_wine[i]<-5
                    }else if(agg_wine_group_cast_logic[i,4]==T & sum(agg_wine_group_cast_logic[i,])==1){
                              categroup_prefer_wine[i]<-7
                    }else{
                              categroup_prefer_wine[i]<-NA
                    }
          }
          agg_wine_group_cast <- cbind(agg_wine_group_cast, categroup_prefer_wine)
          
          # categroup_prefer:beef
          agg_beef_group <- aggregate(unit_mult ~ member_id + sku_type, 
                                      data = subset(order_items, sku_type %in% c(8,9) 
                                                    & member_id %in% subset(member_list, category_prefer == 2)$member_id), 
                                      sum)
          agg_beef_group_cast <- cast(agg_beef_group, member_id ~ sku_type, value = "unit_mult", fill = 0) 
          agg_beef_group_cast$group_max <- apply(agg_beef_group_cast,1,max)
          agg_beef_group_cast_logic <- agg_beef_group_cast[,2:3] == agg_beef_group_cast[,4]
          
          categroup_prefer_beef <- c()
          for(i in 1:nrow(agg_beef_group_cast_logic)){
                    if(agg_beef_group_cast_logic[i,1]==T & sum(agg_beef_group_cast_logic[i,])==1){
                              categroup_prefer_beef[i]<-8
                    }else if(agg_beef_group_cast_logic[i,2]==T & sum(agg_beef_group_cast_logic[i,])==1){
                              categroup_prefer_beef[i]<-9
                    }else{
                              categroup_prefer_beef[i]<-NA
                    }
          }
          agg_beef_group_cast <- cbind(agg_beef_group_cast, categroup_prefer_beef)
          
          member_list_2 <- merge(member_list, agg_wine_group_cast[,c("member_id","categroup_prefer_wine")], by = "member_id", all = T)
          member_list_2 <- merge(member_list_2, agg_beef_group_cast[,c("member_id","categroup_prefer_beef")], by = "member_id", all = T)
          member_list_2$categroup_prefer <- apply(member_list_2[,3:4], 1, sum, na.rm = T)
          member_list_2 <- member_list_2[,c("member_id","category_prefer","categroup_prefer")]
          
          # price_prefer
          orders$price_prefer[orders$final_amount <= 100] <- 1
          orders$price_prefer[orders$final_amount > 100 & orders$final_amount <= 200] <- 2
          orders$price_prefer[orders$final_amount > 200 & orders$final_amount <= 300] <- 3
          orders$price_prefer[orders$final_amount > 300 & orders$final_amount <= 500] <- 4
          orders$price_prefer[orders$final_amount > 500 & orders$final_amount <= 700] <- 5
          orders$price_prefer[orders$final_amount > 700 & orders$final_amount <= 1000] <- 6
          orders$price_prefer[orders$final_amount > 1000 & orders$final_amount <= 1500] <- 7
          orders$price_prefer[orders$final_amount > 1500 & orders$final_amount <= 2000] <- 8
          orders$price_prefer[orders$final_amount > 2000 & orders$final_amount <= 3000] <- 9
          orders$price_prefer[orders$final_amount > 3000] <- 10
          
          price_prefer <- cast(orders, member_id ~ price_prefer, value = "order_id", fill = 0, fun.aggregate = length)
          price_prefer$prefer_max <- apply(price_prefer,1,max)
          price_prefer_logic <- data.frame(member_id = price_prefer[,1], price_prefer[,2:11] == price_prefer[,12])
          price_prefer_logic$price_num <- apply(price_prefer_logic[,-1],1,sum)
          price_prefer_tab <- data.frame(table(price_prefer_logic$price_num))
          
          ## price_prefer_12
          if(c(1) %in% price_prefer_tab[,1] & c(2) %in% price_prefer_tab[,1]){
                    price_prefer_12 <- subset(price_prefer_logic, price_num == 1 | price_num == 2)
                    price_prefer_label <- c()
                    for(i in 1:nrow(price_prefer_12)){
                              if(price_prefer_12[i,2] == T){price_prefer_label[i] <- 1}
                              if(price_prefer_12[i,3] == T){price_prefer_label[i] <- 2}
                              if(price_prefer_12[i,4] == T){price_prefer_label[i] <- 3}
                              if(price_prefer_12[i,5] == T){price_prefer_label[i] <- 4}
                              if(price_prefer_12[i,6] == T){price_prefer_label[i] <- 5}
                              if(price_prefer_12[i,7] == T){price_prefer_label[i] <- 6}
                              if(price_prefer_12[i,8] == T){price_prefer_label[i] <- 7}
                              if(price_prefer_12[i,9] == T){price_prefer_label[i] <- 8}
                              if(price_prefer_12[i,10] == T){price_prefer_label[i] <- 9}
                              if(price_prefer_12[i,11] == T){price_prefer_label[i] <- 10}
                    }
                    price_prefer_12 <- cbind(price_prefer_12, price_prefer_label)
          }else{price_prefer_12 <- NULL}
          
          ## price_prefer_3
          if(c(3) %in% price_prefer_tab[,1]){
                    price_prefer_3 <- subset(price_prefer_logic, price_num == 3)
                    for(i in 1:nrow(price_prefer_3)){price_prefer_3[i,max(which(price_prefer_3[i,2:11] == T))+1] <- F}
                    price_prefer_label <- c()
                    for(i in 1:nrow(price_prefer_3)){
                              if(price_prefer_3[i,2] == T){price_prefer_label[i] <- 1}
                              if(price_prefer_3[i,3] == T){price_prefer_label[i] <- 2}
                              if(price_prefer_3[i,4] == T){price_prefer_label[i] <- 3}
                              if(price_prefer_3[i,5] == T){price_prefer_label[i] <- 4}
                              if(price_prefer_3[i,6] == T){price_prefer_label[i] <- 5}
                              if(price_prefer_3[i,7] == T){price_prefer_label[i] <- 6}
                              if(price_prefer_3[i,8] == T){price_prefer_label[i] <- 7}
                              if(price_prefer_3[i,9] == T){price_prefer_label[i] <- 8}
                              if(price_prefer_3[i,10] == T){price_prefer_label[i] <- 9}
                              if(price_prefer_3[i,11] == T){price_prefer_label[i] <- 10}
                    }
                    price_prefer_3 <- cbind(price_prefer_3, price_prefer_label)
          }else{price_prefer_3 <- NULL}
          
          ## price_prefer_4
          if(c(4) %in% price_prefer_tab[,1]){
                    price_prefer_4 <- subset(price_prefer_logic, price_num == 4)
                    for(i in 1:nrow(price_prefer_4)){price_prefer_4[i,max(which(price_prefer_4[i,2:11] == T))+1] <- F}
                    price_prefer_label <- c()
                    for(i in 1:nrow(price_prefer_4)){
                              if(price_prefer_4[i,2] == T){price_prefer_label[i] <- 1}
                              if(price_prefer_4[i,3] == T){price_prefer_label[i] <- 2}
                              if(price_prefer_4[i,4] == T){price_prefer_label[i] <- 3}
                              if(price_prefer_4[i,5] == T){price_prefer_label[i] <- 4}
                              if(price_prefer_4[i,6] == T){price_prefer_label[i] <- 5}
                              if(price_prefer_4[i,7] == T){price_prefer_label[i] <- 6}
                              if(price_prefer_4[i,8] == T){price_prefer_label[i] <- 7}
                              if(price_prefer_4[i,9] == T){price_prefer_label[i] <- 8}
                              if(price_prefer_4[i,10] == T){price_prefer_label[i] <- 9}
                              if(price_prefer_4[i,11] == T){price_prefer_label[i] <- 10}
                    }
                    price_prefer_4 <- cbind(price_prefer_4, price_prefer_label)  
          }else{price_prefer_4 <- NULL}
          
          ## price_prefer_5
          if(c(5) %in% price_prefer_tab[,1]){
                    price_prefer_5 <- subset(price_prefer_logic, price_num == 5)
                    for(i in 1:nrow(price_prefer_5)){
                              price_prefer_5[i,max(which(price_prefer_5[i,2:11] == T))+1] <- F
                              price_prefer_5[i,max(which(price_prefer_5[i,2:11] == T))+1] <- F
                    }
                    price_prefer_label <- c()
                    for(i in 1:nrow(price_prefer_5)){
                              if(price_prefer_5[i,2] == T){price_prefer_label[i] <- 1}
                              if(price_prefer_5[i,3] == T){price_prefer_label[i] <- 2}
                              if(price_prefer_5[i,4] == T){price_prefer_label[i] <- 3}
                              if(price_prefer_5[i,5] == T){price_prefer_label[i] <- 4}
                              if(price_prefer_5[i,6] == T){price_prefer_label[i] <- 5}
                              if(price_prefer_5[i,7] == T){price_prefer_label[i] <- 6}
                              if(price_prefer_5[i,8] == T){price_prefer_label[i] <- 7}
                              if(price_prefer_5[i,9] == T){price_prefer_label[i] <- 8}
                              if(price_prefer_5[i,10] == T){price_prefer_label[i] <- 9}
                              if(price_prefer_5[i,11] == T){price_prefer_label[i] <- 10}
                    }
                    price_prefer_5 <- cbind(price_prefer_5, price_prefer_label)
          }else{price_prefer_5 <- NULL}
          
          price_prefer <- rbind(price_prefer_12, price_prefer_3, price_prefer_4, price_prefer_5)
          
          member_list_3 <- merge(member_list_2, price_prefer[,c("member_id","price_prefer_label")], by = "member_id", all = T)
          names(member_list_3) <- c("member_id","category_prefer","categroup_prefer","price_prefer")
          
          member_list_3$etl_date <- statistic_date
          
          for(i in 1:ncol(member_list_3)){
                    member_list_3[,i] <- as.character(member_list_3[,i])
          }
          return(member_list_3)
}
