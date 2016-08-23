long_term_prefer <- function(my_date){
          # set_time
          # statitic_date <- as.Date("2016-07-01")
          statitic_date <- my_date
          begin_date <- statitic_date-180+1-as.POSIXlt(statitic_date-180)$mday
          end_date <- statitic_date-as.POSIXlt(statitic_date)$mday
          
          sql_1 <- paste("select order_id, member_id, final_amount, payment, source, 
                         from_unixtime(createtime,'%Y-%m-%d') as create_date,
                         case 
                              when payment in ('alipay','malipaynew') then 'alipay'
                              when payment in ('wxpayjsapi','wxqrpay') then 'wxpay'
                              when payment in ('yeepayepos','yeepaymobile') then 'yeepay'
                              else NULL
                         end as payment_2
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
                         (select order_id, type_id, goods_id, product_id, bn, nums, g_price, amount from sdb_b2c_order_items)b 
                         on a.order_id = b.order_id;", sep = "")
          
          sql_3 <- paste("select goods_id, p_5 from sdb_b2c_goods")
          
          orders <- dbGetQuery(mossel, sql_1)
          orders$order_id <- as.character(orders$order_id)
          orders$member_id <- as.character(orders$member_id)
          
          order_items <- dbGetQuery(mossel, sql_2)
          order_items$order_id <- as.character(order_items$order_id)
          order_items$member_id <- as.character(order_items$member_id)
          
          goods <- dbGetQuery(mossel, sql_3)
          
          sku <- read.csv(file = "sku.csv")
          
          order_items <- merge(x = order_items, y = sku, by = "bn")
          order_items$unit_mult <- order_items$nums*order_items$unit
          order_items <- merge(x = order_items, y = goods, by = "goods_id", all.x = T)
          
          # delete outliers
          member_agg <- aggregate(unit_mult ~ order_id, data = subset(order_items, sku_type != 4), FUN = sum)
          member_agg$scale <- scale(member_agg$unit_mult, center = T, scale = T)
          member_agg <- subset(member_agg, scale < 1 & unit_mult >= 1)
          orders <- subset(orders, order_id %in% member_agg$order_id)
          order_items <- subset(order_items, order_id %in% member_agg$order_id)
          
          member_list <- data.frame(member_id = unique(orders$member_id))
          
          # source_prefer
          cast_source <- cast(orders, member_id ~ source, value = "order_id", fill = 0, fun.aggregate = length)
          cast_source$row_max <- apply(cast_source, 1, max)
          cast_source_logic <- data.frame(member_id = cast_source[,1], cast_source[,2:4] == cast_source[,5])
          cast_source_logic$num <- apply(cast_source_logic[,-1],1,sum)
          cast_source_tab <- data.frame(table(cast_source_logic$num))
          
          if(c(1) %in% cast_source_tab[,1]){
                    source_prefer_1 <- subset(cast_source_logic, num == 1)
                    source_prefer_label <- c()
                    for(i in 1:nrow(source_prefer_1)){
                              if(source_prefer_1[i,2] == T){source_prefer_label[i] <- 1}
                              if(source_prefer_1[i,3] == T){source_prefer_label[i] <- 2}
                              if(source_prefer_1[i,4] == T){source_prefer_label[i] <- 3}
                    }
                    source_prefer_1 <- cbind(source_prefer_1, source_prefer_label)
          }else{source_prefer_1 <- NULL}
          
          if(c(2) %in% cast_source_tab[,1]){
                    source_prefer_2 <- subset(cast_source_logic, num == 2)
                    source_prefer_label <- c(4)
                    source_prefer_2 <- cbind(source_prefer_2, source_prefer_label)
          }else{source_prefer_2 <- NULL}
          
          if(c(3) %in% cast_source_tab[,1]){
                    source_prefer_3 <- subset(cast_source_logic, num == 3)
                    source_prefer_label <- c(4)
                    source_prefer_3 <- cbind(source_prefer_2, source_prefer_label)
          }else{source_prefer_3 <- NULL}
          
          source_prefer <- rbind(source_prefer_1, source_prefer_2, source_prefer_3) # 1-pc; 2-wap; 3-weixin; 4-no_prefer
          
          # payment_prefer
          cast_payment <- cast(subset(orders, !payment_2 %in% NA), member_id ~ payment_2, value = "order_id", fill = 0, fun.aggregate = length)
          cast_payment$row_max <- apply(cast_payment, 1, max)
          cast_payment_logic <- data.frame(member_id = cast_payment[,1], cast_payment[,2:4] == cast_payment[,5])
          cast_payment_logic$num <- apply(cast_payment_logic[,-1],1,sum)
          cast_payment_tab <- data.frame(table(cast_payment_logic$num))
          
          if(c(1) %in% cast_payment_tab[,1]){
                    payment_prefer_1 <- subset(cast_payment_logic, num == 1)
                    payment_prefer_label <- c()
                    for(i in 1:nrow(payment_prefer_1)){
                              if(payment_prefer_1[i,2] == T){payment_prefer_label[i] <- 1}
                              if(payment_prefer_1[i,3] == T){payment_prefer_label[i] <- 2}
                              if(payment_prefer_1[i,4] == T){payment_prefer_label[i] <- 3}
                    }
                    payment_prefer_1 <- cbind(payment_prefer_1, payment_prefer_label)
          }else{payment_prefer_1 <- NULL}
          
          if(c(2) %in% cast_payment_tab[,1]){
                    payment_prefer_2 <- subset(cast_payment_logic, num == 2)
                    payment_prefer_label <- c(4)
                    payment_prefer_2 <- cbind(payment_prefer_2, payment_prefer_label)
          }else{payment_prefer_2 <- NULL}
          
          if(c(3) %in% cast_payment_tab[,1]){
                    payment_prefer_3 <- subset(cast_payment_logic, num == 3)
                    payment_prefer_label <- c(4)
                    payment_prefer_3 <- cbind(payment_prefer_3, payment_prefer_label)
          }else{payment_prefer_3 <- NULL}
          
          payment_prefer <- rbind(payment_prefer_1, payment_prefer_2, payment_prefer_3) # 1-alipay; 2-wxpay; 3-yeepay
          
          # taste_prefer
          table(order_items$p_5)
          order_items_2 <- subset(order_items, p_5 %in% c(24,27))
          
          cast_taste <- cast(subset(order_items, sku_type %in% c(1,2,5) & !p_5 %in% NA), member_id ~ p_5, value = "unit_mult", fill = 0, fun.aggregate = sum)
          cast_taste$row_max <- apply(cast_taste, 1, max)
          cast_taste_logic <- data.frame(member_id = cast_taste[,1], cast_taste[,2:9] == cast_taste[,10])
          cast_taste_logic$num <- apply(cast_taste_logic[,-1],1,sum)
          cast_taste_tab <- data.frame(table(cast_taste_logic$num))
          
          if(c(1) %in% cast_taste_tab[,1]){
                    taste_prefer_1 <- subset(cast_taste_logic, num == 1)
                    taste_prefer_label <- c()
                    for(i in 1:nrow(taste_prefer_1)){
                              if(taste_prefer_1[i,2] == T){taste_prefer_label[i] <- 23}
                              if(taste_prefer_1[i,3] == T){taste_prefer_label[i] <- 24}
                              if(taste_prefer_1[i,4] == T){taste_prefer_label[i] <- 25}
                              if(taste_prefer_1[i,5] == T){taste_prefer_label[i] <- 26}
                              if(taste_prefer_1[i,6] == T){taste_prefer_label[i] <- 27}
                              if(taste_prefer_1[i,7] == T){taste_prefer_label[i] <- 28}
                              if(taste_prefer_1[i,8] == T){taste_prefer_label[i] <- 29}
                              if(taste_prefer_1[i,9] == T){taste_prefer_label[i] <- 30}
                    }
                    taste_prefer_1 <- cbind(taste_prefer_1, taste_prefer_label)
          }else{taste_prefer_1 <- NULL}
          
          if(c(2) %in% cast_taste_tab[,1]){
                    taste_prefer_2 <- subset(cast_taste_logic, num == 2)
                    taste_prefer_label <- c(99)
                    taste_prefer_2 <- cbind(taste_prefer_2, taste_prefer_label)
          }else{taste_prefer_2 <- NULL}
          
          if(c(3) %in% cast_taste_tab[,1]){
                    taste_prefer_3 <- subset(cast_taste_logic, num == 3)
                    taste_prefer_label <- c(99)
                    taste_prefer_3 <- cbind(taste_prefer_3, taste_prefer_label)
          }else{taste_prefer_3 <- NULL}
          
          if(c(4) %in% cast_taste_tab[,1]){
                    taste_prefer_4 <- subset(cast_taste_logic, num == 4)
                    taste_prefer_label <- c(99)
                    taste_prefer_4 <- cbind(taste_prefer_4, taste_prefer_label)
          }else{taste_prefer_4 <- NULL}
          
          if(c(5) %in% cast_taste_tab[,1]){
                    taste_prefer_5 <- subset(cast_taste_logic, num == 5)
                    taste_prefer_label <- c(99)
                    taste_prefer_5 <- cbind(taste_prefer_5, taste_prefer_label)
          }else{taste_prefer_5 <- NULL}
          
          if(c(6) %in% cast_taste_tab[,1]){
                    taste_prefer_6 <- subset(cast_taste_logic, num == 6)
                    taste_prefer_label <- c(99)
                    taste_prefer_6 <- cbind(taste_prefer_6, taste_prefer_label)
          }else{taste_prefer_6 <- NULL}
          
          taste_prefer <- rbind(taste_prefer_1, taste_prefer_2, taste_prefer_3, taste_prefer_4, taste_prefer_5, taste_prefer_6)
          
          # hui_zong
          member_list_2 <- merge(member_list, source_prefer[,c("member_id","source_prefer_label")], by = "member_id", all.x = T)
          member_list_3 <- merge(member_list_2, payment_prefer[,c("member_id","payment_prefer_label")], by = "member_id", all.x = T)
          member_list_4 <- merge(member_list_3, taste_prefer[,c("member_id","taste_prefer_label")], by = "member_id", all.x = T)
          names(member_list_4) <- c("member_id","source_prefer","payment_prefer","taste_prefer")
          
          member_list_4$etl_date <- Sys.Date()
          
          for(i in 1:ncol(member_list_4)){
                    member_list_4[,i] <- as.character(member_list_4[,i])
          }
          return(member_list_4)
}
