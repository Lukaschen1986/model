scale_func <- function(x){(x-min(x))/(max(x)-min(x))}

member_value_history <- function(mydate, w1, w2, w3, w4){
          # set_time
          statistic_date <- mydate
          # statistic_date <- as.Date("2015-11-01")
          begin_date <- statistic_date-90+1-as.POSIXlt(statistic_date-90)$mday
          end_date <- statistic_date-as.POSIXlt(statistic_date)$mday
          
          # load_data
          sql_1 <- paste("select order_id, member_id, final_amount, cost_freight, pmt_order, 
                         from_unixtime(createtime,'%Y-%m-%d') as create_date 
                         from sdb_b2c_orders 
                         where pay_status = '1' 
                         and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
                         "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "';", sep = "")
          
          sql_1.2 <- paste("select order_id, member_id, final_amount, cost_freight, pmt_order, 
                           from_unixtime(createtime,'%Y-%m-%d') as create_date 
                           from sdb_b2c_archive_orders 
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
          
          sql_2.2 <- paste("select a.member_id, a.final_amount, a.cost_freight, a.pmt_order, b.*, a.create_date
                           from
                           (select order_id, member_id, final_amount, cost_freight, pmt_order, 
                           from_unixtime(createtime,'%Y-%m-%d') as create_date
                           from sdb_b2c_archive_orders 
                           where pay_status = '1'
                           and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
                           "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "')a
                           inner join
                           (select order_id, type_id, goods_id, product_id, bn, nums, g_price, amount from sdb_b2c_archive_order_items)b 
                           on a.order_id = b.order_id;", sep = "")
          
          bao_sql <- paste("select t1.goods_id, t1.name, sum(t1.nums) as sum_nums
                           from 
                           (select a.*, b.member_id, b.create_date
                           from
                           (select * from sdb_b2c_order_items) a
                           inner join
                           (select order_id, member_id, from_unixtime(createtime,'%Y-%m-%d') as create_date
                           from sdb_b2c_orders
                           where pay_status = '1'
                           and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
                           "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "') b 
                           on a.order_id = b.order_id
                           where type_id <> '4')t1 
                           group by t1.goods_id
                           order by sum_nums desc", sep = "")
          
          orders <- dbGetQuery(mossel, sql_1)
          orders$order_id <- as.character(orders$order_id)
          orders$member_id <- as.character(orders$member_id)
          orders$create_month <- as.Date(orders$create_date) + 1 - as.POSIXlt(orders$create_date)$mday
          
          orders.2 <- dbGetQuery(mossel, sql_1.2)
          orders.2$order_id <- as.character(orders.2$order_id)
          orders.2$member_id <- as.character(orders.2$member_id)
          orders.2$create_month <- as.Date(orders.2$create_date) + 1 - as.POSIXlt(orders.2$create_date)$mday
          orders <- rbind(orders, orders.2)
          
          order_items <- dbGetQuery(mossel, sql_2)
          order_items$order_id <- as.character(order_items$order_id)
          order_items$member_id <- as.character(order_items$member_id)
          order_items$create_month <- as.Date(order_items$create_date) + 1 - as.POSIXlt(order_items$create_date)$mday
          
          order_items.2 <- dbGetQuery(mossel, sql_2.2)
          order_items.2$order_id <- as.character(order_items.2$order_id)
          order_items.2$member_id <- as.character(order_items.2$member_id)
          order_items.2$create_month <- as.Date(order_items.2$create_date) + 1 - as.POSIXlt(order_items.2$create_date)$mday
          order_items <- rbind(order_items, order_items.2)
          
          sku <- read.csv(file = "sku.csv")
          
          order_items <- merge(x = order_items, y = sku[,c("bn","sku_type","unit")], by = "bn", all.x = T)
          order_items$unit_mult <- order_items$nums*order_items$unit
          
          baokuan <- dbGetQuery(mossel, bao_sql)
          
          # delete_outliers
          member_agg <- aggregate(unit_mult ~ order_id, data = subset(order_items, sku_type != 4), FUN = sum)
          member_agg$scale <- scale(member_agg$unit_mult, center = T, scale = T)
          member_agg <- subset(member_agg, scale < 1 & unit_mult >= 1)
          orders <- subset(orders, order_id %in% member_agg$order_id)
          order_items <- subset(order_items, order_id %in% member_agg$order_id)
          
          # RFMS
          ## F
          agg_f <- aggregate(order_id ~ member_id, data = orders, FUN = length)
          agg_f$frequency <- agg_f$order_id
          agg_f$frequency_log <- log(agg_f$frequency)
          agg_f_2 <- agg_f[,c("member_id","frequency_log")]
          
          ## M
          agg_m <- aggregate(amount ~ member_id, data = subset(order_items, sku_type != 4), FUN = sum)
          agg_m$monetary_log <- log(agg_m$amount)
          agg_m_2 <- agg_m[,c("member_id","monetary_log")]   
          
          ## unit_mult
          agg_unit_mult <- aggregate(unit_mult ~ member_id, data = subset(order_items, sku_type != 4), FUN = sum)
          agg_unit_mult$unit_log <- log(agg_unit_mult$unit_mult)
          agg_unit_mult_2 <- agg_unit_mult[,c("member_id","unit_log")]
          
          ## baokuan
          agg_bao <- aggregate(unit_mult ~ member_id, data = subset(order_items, goods_id %in% baokuan[1:10,1]), FUN = sum)
          agg_bao$bao_log <- log(agg_bao$unit_mult)
          agg_bao_2 <- agg_bao[,c("member_id","bao_log")]
          
          df_merge <- data.frame(member_id = agg_f_2[,1],
                                 frequency = agg_f_2[,2], 
                                 monetary = agg_m_2[,2], 
                                 unit = agg_unit_mult_2[,2])
          df_merge <- merge(df_merge, agg_bao_2, by = "member_id", all.x = T)
          df_merge$bao_log[df_merge$bao_log %in% NA] <- 0
          
          # data_scale
          df_merge_scale <- data.frame(member_id = df_merge$member_id,
                                       frequency = scale_func(df_merge$frequency),
                                       monetary = scale_func(df_merge$monetary),
                                       unit = scale_func(df_merge$unit),
                                       unit_bao = scale_func(df_merge$bao_log))
          
          # df_merge_scale$score <- df_merge_scale$frequency*0.3 + df_merge_scale$monetary*0.4 + df_merge_scale$unit*0.2 + df_merge_scale$unit_bao*0.1
          df_merge_scale$score <- df_merge_scale$frequency*w1 + df_merge_scale$monetary*w2 + df_merge_scale$unit*w3 + df_merge_scale$unit_bao*w4
          
          df_origin <- data.frame(member_id = agg_f$member_id,
                                  frequency = agg_f$frequency,
                                  monetary = agg_m$amount,
                                  unit = agg_unit_mult$unit_mult,
                                  unit_bao = exp(df_merge$bao_log))
          df_origin <- merge(df_origin, df_merge_scale[,c("member_id","score")], by = "member_id", all.x = T)
          
          dist <- (max(df_origin$score)-min(df_origin$score))/3
          c1 <- min(df_origin$score)+dist
          c2 <- min(df_origin$score)+dist*2
          
          df_origin$label[df_origin$score >= c2] <- 1
          df_origin$label[df_origin$score >= c1 & df_origin$score < c2] <- 2
          df_origin$label[df_origin$score < c1] <- 3
          # table(df_origin$label)
          
          return(df_origin)
}
