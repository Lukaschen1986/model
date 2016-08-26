library(RMySQL)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(VIM)
library(mice)
library(ROSE)

rm(list = ls()); gc()
options(stringsAsFactors = F)

# set env
mossel <- dbConnect(MySQL(), user = "root", password = "chen1986", host = "127.0.0.1", db = "mossel")

# set time
# statitic_date <- Sys.Date()
statitic_date <- as.Date("2016-06-15")
begin_date <- statitic_date - 30
end_date <- statitic_date - 1

sql_1 <- paste("select order_id, member_id, final_amount, from_unixtime(createtime,'%Y-%m-%d') as create_date 
               from sdb_b2c_orders 
               where pay_status = 1 
               and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
               "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "';", sep = "")

sql_2 <- paste("select a.member_id, b.*, a.create_date
               from
               (select order_id, member_id, final_amount, from_unixtime(createtime,'%Y-%m-%d') as create_date
               from sdb_b2c_orders 
               where pay_status = 1
               and from_unixtime(createtime,'%Y-%m-%d') >= '", begin_date, 
               "' and from_unixtime(createtime,'%Y-%m-%d') <= '", end_date, "')a
               inner join
               (select order_id, type_id, product_id, bn, nums, g_price, amount from sdb_b2c_order_items)b 
               on a.order_id = b.order_id;", sep = "")

orders <- dbGetQuery(mossel, sql_1)
orders$order_id <- as.character(orders$order_id)
orders$member_id <- as.character(orders$member_id)

order_items <- dbGetQuery(mossel, sql_2)
order_items$order_id <- as.character(order_items$order_id)
order_items$member_id <- as.character(order_items$member_id)

sku <- read.csv(file = "sku.csv")

if(sum(!unique(order_items$bn) %in% sku$bn) >= 1){
          sku_new <- data.frame(unique(order_items[which(!order_items$bn %in% sku$bn),c("bn","product_id")]), 
                                sku_type = NA, 
                                unit = NA,
                                etl_date = as.character(Sys.Date()))
          sku_new <- rbind(sku_new, sku)
          write.csv(sku_new, file = "sku.csv", row.names = F)
}

