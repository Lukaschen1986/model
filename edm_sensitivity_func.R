edm_sensitivity <- function(my_date){
          # set_time
          # statistic_date <- as.Date("2016-08-01")
          statistic_date <- my_date
          begin_date <- statistic_date-90+1-as.POSIXlt(statistic_date-90)$mday
          end_date <- statistic_date-as.POSIXlt(statistic_date)$mday
          
          sql_1 <- paste("select order_id, member_id, final_amount, payment, source, 
                         from_unixtime(createtime,'%Y-%m-%d') as create_date,
                         from_unixtime(createtime,'%H:%S') as create_hour,
                         case 
                         when payment in ('alipay','malipaynew') then 'alipay'
                         when payment in ('wxpayjsapi','wxqrpay') then 'wxpay'
                         when payment in ('yeepayepos','yeepaymobile') then 'yeepay'
                         else NULL
                         end as payment_2
                         from sdb_b2c_orders 
                         where pay_status = '1' 
                         and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
                         "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "'", sep = "")
          
          sql_2 <- paste("select a.member_id, a.final_amount, a.cost_freight, a.pmt_order, b.*, a.create_date, a.create_hour
                         from
                         (select order_id, member_id, final_amount, cost_freight, pmt_order, 
                         from_unixtime(createtime,'%Y-%m-%d') as create_date,
                         from_unixtime(createtime,'%H:%S') as create_hour
                         from sdb_b2c_orders
                         where pay_status = '1'
                         and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
                         "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "')a
                         inner join
                         (select order_id, type_id, goods_id, product_id, bn, nums, g_price, amount 
                         from sdb_b2c_order_items)b 
                         on a.order_id = b.order_id", sep = "")
          
          sql_3 <- paste("select goods_id, p_5 from sdb_b2c_goods")
          
          orders <- dbGetQuery(mossel, sql_1)
          orders$order_id <- as.character(orders$order_id)
          orders$member_id <- as.character(orders$member_id)
          
          order_items <- dbGetQuery(mossel, sql_2)
          order_items$order_id <- as.character(order_items$order_id)
          order_items$member_id <- as.character(order_items$member_id)
          
          goods <- dbGetQuery(mossel, sql_3)
          
          sku <- read.csv(file = "sku.csv")
          edm <- read.csv(file = "edm.csv")
          edm$send_time <- as.POSIXlt(edm$send_time)
          edm$is_promote <- 1
          edm <- separate(data = edm, col = send_time, into = c("create_date","send_hour"), sep = " ", remove = F)
          
          order_items <- merge(x = order_items, y = sku[,c("bn","sku_type","unit")], by = "bn", all.x = T)
          order_items$unit_mult <- order_items$nums*order_items$unit
          order_items <- merge(x = order_items, y = goods, by = "goods_id", all.x = T)
          order_items <- unite(data = order_items, col = create_time, create_date, create_hour, sep = " ", remove = F)
          order_items$create_time <- as.POSIXlt(order_items$create_time)
          order_items <- merge(order_items, edm, by = "create_date", all.x = T)
          order_items$is_promote[order_items$is_promote %in% NA] <- 0
          
          # delete outliers
          member_agg <- aggregate(unit_mult ~ order_id, data = subset(order_items, sku_type != 4), FUN = sum)
          member_agg$scale <- scale(member_agg$unit_mult, center = T, scale = T)
          member_agg <- subset(member_agg, scale < 1 & unit_mult >= 1)
          orders <- subset(orders, order_id %in% member_agg$order_id)
          order_items <- subset(order_items, order_id %in% member_agg$order_id)
          
          member_list <- data.frame(member_id = unique(orders$member_id))
          
          # edm_sensitivity
          df <- subset(order_items, is_promote == 1 & edm_type %in% c(1,4) & sku_type %in% c(1,2,5) & create_time > send_time | is_promote == 1 & edm_type %in% c(2,4) & sku_type == 6 & create_time > send_time | is_promote == 1 & edm_type %in% c(3,4) & sku_type == 3 & create_time > send_time)
          
          df$time_mines <- as.numeric(df$create_time-df$send_time)
          df$time_mines_discrete[df$time_mines >= 5 & df$time_mines <= 30] <- "t1"
          df$time_mines_discrete[df$time_mines >= 31 & df$time_mines <= 60] <- "t2"
          df$time_mines_discrete[df$time_mines >= 61 & df$time_mines <= 240] <- "t3"
          df$time_mines_discrete[df$time_mines >= 241 & df$time_mines <= 480] <- "t4"
          df$time_mines_discrete[df$time_mines >= 481 & max(df$time_mines)] <- "t5"
          
          cast_discrete <- cast(subset(df, sku_type != 4), member_id ~ time_mines_discrete, value = "order_id", fill = 0, fun.aggregate = length)
          cast_discrete$'NA' <- NULL
          cast_discrete$sensitivity_score <- cast_discrete$t1*0.4 + cast_discrete$t2*0.3 + cast_discrete$t3*0.2 + cast_discrete$t4*0.07 + cast_discrete$t5*0.03
          
          member_list <- merge(member_list, cast_discrete[,c("member_id","sensitivity_score")], by = "member_id", all.x = T)
          member_list$sensitivity_score[member_list$sensitivity_score %in% NA] <- 0
          member_list$etl_date <- statistic_date
          
          for(i in 1:ncol(member_list)){
                    member_list[,i] <- as.character(member_list[,i])
          }
          return(member_list)
}
