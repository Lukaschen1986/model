# func
data_divide <- function(df, divide_1 = 0.8){
          df <- df[sample(1:nrow(df), nrow(df)), ]
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          test <- df[-idx,]
          res <- list(train = train, test = test)
          return(res)
}

bagging_func <- function(train, iter){
          vctr_idnt <- function(x){x/sqrt(sum(x^2))}
          # iter = 5
          idx <- c(); y_train <- list(); X_train <- list(); w_hat <- c()
          for(i in 1:iter){
                    # i = 5
                    idx <- rbind(idx, sample(1:nrow(train), nrow(train), replace = T))
                    y_train[[i]] <- train[idx[i,],c("is_loss")]
                    X_train[[i]] <- as.matrix(cbind(b0 = c(1), 
                                                    scale(train[idx[i,], c("order_sum",
                                                                           "amount_sum",
                                                                           "bn_sum",
                                                                           "nums_sum",
                                                                           "unitmult_sum",
                                                                           "sensitivity_score",
                                                                           "unitmult_bao_sum",
                                                                           "cart_sum",
                                                                           "point",
                                                                           "experience",
                                                                           "netage")]), 
                                                    category_dumm_1 = train$category_dumm_1[idx[i,]],
                                                    category_dumm_2 = train$category_dumm_2[idx[i,]],
                                                    category_dumm_3 = train$category_dumm_3[idx[i,]],
                                                    categroup_dumm_1 = train$categroup_dumm_1[idx[i,]],
                                                    categroup_dumm_2 = train$categroup_dumm_2[idx[i,]],
                                                    categroup_dumm_3 = train$categroup_dumm_3[idx[i,]],
                                                    categroup_dumm_4 = train$categroup_dumm_4[idx[i,]],
                                                    categroup_dumm_5 = train$categroup_dumm_5[idx[i,]],
                                                    categroup_dumm_6 = train$categroup_dumm_6[idx[i,]],
                                                    lv_id_dumm_1 = train$lv_id_dumm_1[idx[i,]],
                                                    lv_id_dumm_2 = train$lv_id_dumm_2[idx[i,]],
                                                    lv_id_dumm_3 = train$lv_id_dumm_3[idx[i,]],
                                                    lv_id_dumm_4 = train$lv_id_dumm_4[idx[i,]]))
                    
                    n <- 50000; p <- ncol(X_train[[i]])
                    w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
                    e_in <- c(1); yita <- 0.001 # 0.01, 0.03, 0.1, 0.3, 1, 3, 10
                    
                    for(k in 2:nrow(w)){
                              w[k,] <- w[k-1,] - yita*t(vctr_idnt(t(1/(1+exp(y_train[[i]]*X_train[[i]]%*%w[k-1,])))%*%(-y_train[[i]]*X_train[[i]])/nrow(X_train[[i]])))
                              e_in[k] <- sum(log(1+exp(-y_train[[i]]*X_train[[i]]%*%w[k,])))/nrow(X_train[[i]])
                              if(round(e_in[k],7) == round(e_in[k-1],7)){break}
                    }
                    plot(e_in, type = "b")
                    w_hat <- rbind(w_hat, w[which.min(e_in),])
          }
          
          p_hat_test <- c(); y_pred_test <- c()
          for(i in 1:iter){
                    p_hat_test <- cbind(p_hat_test, 1/(1+exp(-X_test%*%w_hat[i,])))
          }
          p_hat_test_final <- apply(p_hat_test,1,mean)
          y_pred_test <- ifelse(p_hat_test_final >= (0.5+mean(p_hat_test_final))/2, 1, -1)
          # y_pred_test <- ifelse(p_hat_test_final >= 0.5, 1, -1)
          # y_pred_test <- ifelse(p_hat_test_final >= median(p_hat_test_final), 1, -1)
          accu_test <- sum(y_pred_test == y_test)/length(y_test)
          
          res <- list(p_hat_test_final = p_hat_test_final, y_pred_test = y_pred_test, w_hat = w_hat, accu_test = accu_test)
          return(res)
}

loss_warning_func <- function(mydate){
          # set_time
          statistic_date <- mydate
          # statistic_date <- as.Date("2016-08-01")
          begin_date <- statistic_date-180+1-as.POSIXlt(statistic_date-180)$mday
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
          
          member_sql <- paste("select member_id, member_lv_id, point, experience, sex, 
                              from_unixtime(regtime,'%Y-%m-%d') as regtime
                              from sdb_b2c_members where regtime <> '0'")
          
          cart_sql <- paste("select member_id, quantity, from_unixtime(time,'%Y-%m-%d') as cart_time from sdb_b2c_cart_objects")
          
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
          
          members <- dbGetQuery(mossel, member_sql)
          
          cart <- dbGetQuery(mossel, cart_sql)
          cart$cart_month <- as.Date(cart$cart_time) + 1 - as.POSIXlt(cart$cart_time)$mday
          
          baokuan <- dbGetQuery(mossel, bao_sql)
          
          # delete_outliers
          member_agg <- aggregate(unit_mult ~ order_id, data = subset(order_items, sku_type != 4), FUN = sum)
          member_agg$scale <- scale(member_agg$unit_mult, center = T, scale = T)
          member_agg <- subset(member_agg, scale < 1 & unit_mult >= 1)
          orders <- subset(orders, order_id %in% member_agg$order_id)
          order_items <- subset(order_items, order_id %in% member_agg$order_id)
          
          # member_id分组：活跃+流失
          diff_date <- statistic_date-90+1-as.POSIXlt(statistic_date-90)$mday
          orders_left <- subset(orders, create_date >= begin_date & create_date < diff_date)
          orders_right <- subset(orders, create_date >= diff_date & create_date <= end_date)
          
          order_items_left <- subset(order_items, create_date >= begin_date & create_date < diff_date)
          order_items_right <- subset(order_items, create_date >= diff_date & create_date <= end_date)
          
          member_id_left <- unique(order_items_left$member_id)
          member_id_right <- unique(order_items_right$member_id)
          
          member_id_active <- intersect(x = member_id_left, y = member_id_right)
          member_id_loss <- setdiff(x = member_id_left, y = member_id_right)
          
          cart_left <- subset(cart, cart_time >= begin_date & cart_time < diff_date)
          cart_right <- subset(cart, cart_time >= diff_date & cart_time < end_date)
          
          # merge
          order_agg <- aggregate(order_id ~ member_id, data = orders_left, FUN = length)
          names(order_agg) <- c("member_id","order_sum")
          
          amount_agg <- aggregate(final_amount ~ member_id, data = order_items_left, FUN = sum)
          names(amount_agg) <- c("member_id","amount_sum")
          
          bn_agg <- aggregate(bn ~ member_id, data = order_items_left, FUN = length)
          names(bn_agg) <- c("member_id","bn_sum")
          
          nums_agg <- aggregate(nums ~ member_id, data = order_items_left, FUN = sum)
          names(nums_agg) <- c("member_id","nums_sum")
          
          unitmult_agg <- aggregate(unit_mult ~ member_id, data = order_items_left, FUN = sum)
          names(unitmult_agg) <- c("member_id","unitmult_sum")
          
          unitmult_bao_agg <- aggregate(unit_mult ~ member_id, data = subset(order_items_left, goods_id %in% baokuan[1:10,1]), FUN = sum)
          names(unitmult_bao_agg) <- c("member_id","unitmult_bao_sum")
          
          cart_agg <- aggregate(quantity ~ member_id, data = cart_left, sum)
          names(cart_agg) <- c("member_id","cart_sum")
          
          ## short_term_prefer
          source("short_term_prefer_func_v2.R")
          res_short <- short_term_prefer(my_date = diff_date)
          res_short$categroup_prefer[res_short$categroup_prefer %in% NA] <- 99
          
          res_short$category_dumm_1 <- c(0)
          res_short$category_dumm_2 <- c(0)
          res_short$category_dumm_3 <- c(0)
          res_short$category_dumm_3[res_short$category_prefer == 2] <- 1
          res_short$category_dumm_2[res_short$category_prefer == 3] <- 1
          res_short$category_dumm_1[res_short$category_prefer == 4] <- 1
          
          res_short$categroup_dumm_1 <- c(0)
          res_short$categroup_dumm_2 <- c(0)
          res_short$categroup_dumm_3 <- c(0)
          res_short$categroup_dumm_4 <- c(0)
          res_short$categroup_dumm_5 <- c(0)
          res_short$categroup_dumm_6 <- c(0)
          res_short$categroup_dumm_6[res_short$categroup_prefer == 2] <- 1
          res_short$categroup_dumm_5[res_short$categroup_prefer == 5] <- 1
          res_short$categroup_dumm_4[res_short$categroup_prefer == 7] <- 1
          res_short$categroup_dumm_3[res_short$categroup_prefer == 8] <- 1
          res_short$categroup_dumm_2[res_short$categroup_prefer == 9] <- 1
          res_short$categroup_dumm_1[res_short$categroup_prefer == 0] <- 1
          
          ## edm_sensitivity
          source("edm_sensitivity_func.R")
          res_edm <- edm_sensitivity(my_date = diff_date)
          res_edm$sensitivity_score <- as.numeric(res_edm$sensitivity_score)
          
          df_merge <- data.frame(member_id = order_agg$member_id,
                                 order_sum = order_agg$order_sum,
                                 amount_sum = amount_agg$amount_sum,
                                 bn_sum = bn_agg$bn_sum,
                                 nums_sum = nums_agg$nums_sum,
                                 unitmult_sum = unitmult_agg$unitmult_sum)
          
          df_merge <- merge(df_merge, res_short[,-c(2:5)], by = "member_id", all.x = T)
          df_merge <- merge(df_merge, res_edm[,c(1:2)], by = "member_id", all.x = T)
          df_merge <- merge(df_merge, unitmult_bao_agg, by = "member_id", all.x = T)
          df_merge <- merge(df_merge, cart_agg, by = "member_id", all.x = T)
          df_merge$unitmult_bao_sum[df_merge$unitmult_bao_sum %in% NA] <- 0
          df_merge$cart_sum[df_merge$cart_sum %in% NA] <- 0
          
          df_merge <- merge(df_merge, members, by = "member_id", all.x = T)
          df_merge$diff_date <- diff_date
          df_merge$netage <- as.numeric(df_merge$diff_date - as.Date(df_merge$regtime))
          
          # colSums(is.na(df_merge))
          df_complete <- df_merge[complete.cases(df_merge),]
          df_complete$lv_id_dumm_1 <- c(0)
          df_complete$lv_id_dumm_2 <- c(0)
          df_complete$lv_id_dumm_3 <- c(0)
          df_complete$lv_id_dumm_4 <- c(0)
          df_complete$lv_id_dumm_4[df_complete$member_lv_id == 2] <- 1
          df_complete$lv_id_dumm_3[df_complete$member_lv_id == 3] <- 1
          df_complete$lv_id_dumm_2[df_complete$member_lv_id == 4] <- 1
          df_complete$lv_id_dumm_1[df_complete$member_lv_id == 5] <- 1
          
          df_complete <- df_complete[,c("member_id","order_sum","amount_sum","bn_sum","nums_sum","unitmult_sum","sensitivity_score","unitmult_bao_sum","cart_sum","point","experience","netage","category_dumm_1","category_dumm_2","category_dumm_3","categroup_dumm_1","categroup_dumm_2","categroup_dumm_3","categroup_dumm_4","categroup_dumm_5","categroup_dumm_6","lv_id_dumm_1","lv_id_dumm_2","lv_id_dumm_3","lv_id_dumm_4")]
          
          # data_divide
          df_active <- subset(df_complete, member_id %in% member_id_active)
          df_active$is_loss <- c(-1)
          
          df_loss <- subset(df_complete, member_id %in% member_id_loss)
          df_loss$is_loss <- c(1)
          
          df_new <- rbind(df_active, df_loss)
          random_idx <- sample(1:nrow(df_new), nrow(df_new))
          df_new <- df_new[random_idx,]
          
          chisq_test <- c()
          for(i in 1:100){
                    data <- data_divide(df = df_new, divide_1 = 0.8)
                    train <- data$train
                    test <- data$test
                    chisq_test[i] <- chisq.test(rbind(table(train$is_loss), table(test$is_loss)))$p.value
                    if(chisq_test[i] >= 0.8){break}
          }
          # chisq_test
          # rbind(table(train$is_loss), table(test$is_loss))
          
          # train_and_test
          y_test <- test$is_loss
          X_test <- as.matrix(cbind(b0 = c(1), 
                                    scale(test[,c("order_sum",
                                                  "amount_sum",
                                                  "bn_sum",
                                                  "nums_sum",
                                                  "unitmult_sum",
                                                  "sensitivity_score",
                                                  "unitmult_bao_sum",
                                                  "cart_sum",
                                                  "point",
                                                  "experience",
                                                  "netage")]), 
                                    category_dumm_1 = test$category_dumm_1,
                                    category_dumm_2 = test$category_dumm_2,
                                    category_dumm_3 = test$category_dumm_3,
                                    categroup_dumm_1 = test$categroup_dumm_1,
                                    categroup_dumm_2 = test$categroup_dumm_2,
                                    categroup_dumm_3 = test$categroup_dumm_3,
                                    categroup_dumm_4 = test$categroup_dumm_4,
                                    categroup_dumm_5 = test$categroup_dumm_5,
                                    categroup_dumm_6 = test$categroup_dumm_6,
                                    lv_id_dumm_1 = test$lv_id_dumm_1,
                                    lv_id_dumm_2 = test$lv_id_dumm_2,
                                    lv_id_dumm_3 = test$lv_id_dumm_3,
                                    lv_id_dumm_4 = test$lv_id_dumm_4))
          
          res_bagging <- bagging_func(train = train, iter = 10)
          res_bagging$accu_test # 0.8288191
          # table(y_test, res_bagging$y_pred_test)
          
          # pred <- prediction(predictions = res_bagging$p_hat_test_final, labels = y_test) 
          # perf <- performance(pred, "tpr","fpr") 
          # plot(perf)
          # performance(pred, measure = c("auc")) # 计算下曲线面积
          
          # blending
          train_2 <- train[,-1]
          train_2$is_loss <- factor(train_2$is_loss)
          train_2 <- cbind(scale(train_2[,1:11]), train_2[,12:25])
          
          test_2 <- test[,-1]
          test_2$is_loss <- factor(test_2$is_loss)
          test_2 <- cbind(scale(test_2[,1:11]), test_2[,12:25])
          ## rf
          set.seed(111)
          fit.rf <- randomForest(is_loss ~ ., data = train_2, 
                                 mtry = round(sqrt(ncol(train_2)-1)), 
                                 importance = T, 
                                 proximity = T, 
                                 ntree = 300)
          pred.rf <- predict(fit.rf, newdata = test_2)
          accu_rf <- sum(pred.rf == test_2$is_loss)/nrow(test_2) # 0.8569881
          # table(test_2$is_loss, pred.rf)
          # importance(fit.rf) #  自变量重要性
          # varImpPlot(fit.rf)
          
          ## nnet
          set.seed(222)
          fit.nnet <- nnet(is_loss ~ ., data = train_2, size = 10, decay = 0.01, maxit = 1000, inout = F, trace = F)
          pred.nnet <- predict(fit.nnet, newdata = test_2, type = "class")
          accu_nnet <- sum(pred.nnet == test_2$is_loss)/nrow(test_2) # 0.8450704
          # table(test_2$is_loss, pred.nnet)
          
          df_pred <- data.frame(pred_lr = res_bagging$y_pred_test,
                                pred_rf = as.numeric(as.character(pred.rf)),
                                pred_nnet = as.numeric(as.character(pred.nnet)))
          df_pred$pred_blending <- sign(apply(df_pred, 1, sum))
          accu_blending <- sum(df_pred$pred_blending == test_2$is_loss)/nrow(test_2) # 0.8504875
          
          # predict
          order_agg_2 <- aggregate(order_id ~ member_id, data = orders_right, FUN = length)
          names(order_agg_2) <- c("member_id","order_sum")
          
          amount_agg_2 <- aggregate(final_amount ~ member_id, data = order_items_right, FUN = sum)
          names(amount_agg_2) <- c("member_id","amount_sum")
          
          bn_agg_2 <- aggregate(bn ~ member_id, data = order_items_right, FUN = length)
          names(bn_agg_2) <- c("member_id","bn_sum")
          
          nums_agg_2 <- aggregate(nums ~ member_id, data = order_items_right, FUN = sum)
          names(nums_agg_2) <- c("member_id","nums_sum")
          
          unitmult_agg_2 <- aggregate(unit_mult ~ member_id, data = order_items_right, FUN = sum)
          names(unitmult_agg_2) <- c("member_id","unitmult_sum")
          
          unitmult_bao_agg_2 <- aggregate(unit_mult ~ member_id, data = subset(order_items_right, goods_id %in% baokuan[1:10,1]), FUN = sum)
          names(unitmult_bao_agg_2) <- c("member_id","unitmult_bao_sum")
          
          cart_agg_2 <- aggregate(quantity ~ member_id, data = cart_right, sum)
          names(cart_agg_2) <- c("member_id","cart_sum")
          
          res_short_2 <- short_term_prefer(my_date = statistic_date)
          res_short_2$categroup_prefer[res_short_2$categroup_prefer %in% NA] <- 99
          
          res_short_2$category_dumm_1 <- c(0)
          res_short_2$category_dumm_2 <- c(0)
          res_short_2$category_dumm_3 <- c(0)
          res_short_2$category_dumm_3[res_short_2$category_prefer == 2] <- 1
          res_short_2$category_dumm_2[res_short_2$category_prefer == 3] <- 1
          res_short_2$category_dumm_1[res_short_2$category_prefer == 4] <- 1
          
          res_short_2$categroup_dumm_1 <- c(0)
          res_short_2$categroup_dumm_2 <- c(0)
          res_short_2$categroup_dumm_3 <- c(0)
          res_short_2$categroup_dumm_4 <- c(0)
          res_short_2$categroup_dumm_5 <- c(0)
          res_short_2$categroup_dumm_6 <- c(0)
          res_short_2$categroup_dumm_6[res_short_2$categroup_prefer == 2] <- 1
          res_short_2$categroup_dumm_5[res_short_2$categroup_prefer == 5] <- 1
          res_short_2$categroup_dumm_4[res_short_2$categroup_prefer == 7] <- 1
          res_short_2$categroup_dumm_3[res_short_2$categroup_prefer == 8] <- 1
          res_short_2$categroup_dumm_2[res_short_2$categroup_prefer == 9] <- 1
          res_short_2$categroup_dumm_1[res_short_2$categroup_prefer == 0] <- 1
          
          res_edm_2 <- edm_sensitivity(my_date = statistic_date)
          res_edm_2$sensitivity_score <- as.numeric(res_edm_2$sensitivity_score)
          
          df_merge_2 <- data.frame(member_id = order_agg_2$member_id,
                                   order_sum = order_agg_2$order_sum,
                                   amount_sum = amount_agg_2$amount_sum,
                                   bn_sum = bn_agg_2$bn_sum,
                                   nums_sum = nums_agg_2$nums_sum,
                                   unitmult_sum = unitmult_agg_2$unitmult_sum)
          
          df_merge_2 <- merge(df_merge_2, res_short_2[,-c(2:5)], by = "member_id", all.x = T)
          df_merge_2 <- merge(df_merge_2, res_edm_2[,c(1:2)], by = "member_id", all.x = T)
          df_merge_2 <- merge(df_merge_2, unitmult_bao_agg_2, by = "member_id", all.x = T)
          df_merge_2 <- merge(df_merge_2, cart_agg_2, by = "member_id", all.x = T)
          df_merge_2$unitmult_bao_sum[df_merge_2$unitmult_bao_sum %in% NA] <- 0
          df_merge_2$cart_sum[df_merge_2$cart_sum %in% NA] <- 0
          
          df_merge_2 <- merge(df_merge_2, members, by = "member_id", all.x = T)
          df_merge_2$statistic_date <- statistic_date
          df_merge_2$netage <- as.numeric(df_merge_2$statistic_date - as.Date(df_merge_2$regtime))
          
          # colSums(is.na(df_merge_2))
          df_complete_2 <- df_merge_2[complete.cases(df_merge_2),]
          df_complete_2$lv_id_dumm_1 <- c(0)
          df_complete_2$lv_id_dumm_2 <- c(0)
          df_complete_2$lv_id_dumm_3 <- c(0)
          df_complete_2$lv_id_dumm_4 <- c(0)
          df_complete_2$lv_id_dumm_4[df_complete_2$member_lv_id == 2] <- 1
          df_complete_2$lv_id_dumm_3[df_complete_2$member_lv_id == 3] <- 1
          df_complete_2$lv_id_dumm_2[df_complete_2$member_lv_id == 4] <- 1
          df_complete_2$lv_id_dumm_1[df_complete_2$member_lv_id == 5] <- 1
          
          df_complete_2 <- df_complete_2[,c("member_id","order_sum","amount_sum","bn_sum","nums_sum","unitmult_sum","sensitivity_score","unitmult_bao_sum","cart_sum","point","experience","netage","category_dumm_1","category_dumm_2","category_dumm_3","categroup_dumm_1","categroup_dumm_2","categroup_dumm_3","categroup_dumm_4","categroup_dumm_5","categroup_dumm_6","lv_id_dumm_1","lv_id_dumm_2","lv_id_dumm_3","lv_id_dumm_4")]
          
          X_data <- as.matrix(cbind(b0 = c(1), 
                                    scale(df_complete_2[,c("order_sum",
                                                           "amount_sum",
                                                           "bn_sum",
                                                           "nums_sum",
                                                           "unitmult_sum",
                                                           "sensitivity_score",
                                                           "unitmult_bao_sum",
                                                           "cart_sum",
                                                           "point",
                                                           "experience",
                                                           "netage")]), 
                                    category_dumm_1 = df_complete_2$category_dumm_1,
                                    category_dumm_2 = df_complete_2$category_dumm_2,
                                    category_dumm_3 = df_complete_2$category_dumm_3,
                                    categroup_dumm_1 = df_complete_2$categroup_dumm_1,
                                    categroup_dumm_2 = df_complete_2$categroup_dumm_2,
                                    categroup_dumm_3 = df_complete_2$categroup_dumm_3,
                                    categroup_dumm_4 = df_complete_2$categroup_dumm_4,
                                    categroup_dumm_5 = df_complete_2$categroup_dumm_5,
                                    categroup_dumm_6 = df_complete_2$categroup_dumm_6,
                                    lv_id_dumm_1 = df_complete_2$lv_id_dumm_1,
                                    lv_id_dumm_2 = df_complete_2$lv_id_dumm_2,
                                    lv_id_dumm_3 = df_complete_2$lv_id_dumm_3,
                                    lv_id_dumm_4 = df_complete_2$lv_id_dumm_4))
          
          p_hat <- c()
          for(i in 1:10){
                    p_hat <- cbind(p_hat, 1/(1+exp(-X_data%*%res_bagging$w_hat[i,])))
          }
          p_hat <- apply(p_hat, 1, mean)
          y_pred <- ifelse(p_hat >= (0.5+mean(p_hat))/2, 1, -1)
          
          rf_pred <- predict(fit.rf, newdata = X_data)
          nnet_pred <- predict(fit.nnet, newdata = X_data, type = "class")
          
          df_pred_2 <- data.frame(pred_lr = y_pred,
                                  pred_rf = as.numeric(as.character(rf_pred)),
                                  pred_nnet = as.numeric(as.character(nnet_pred)))
          df_pred_2$pred_blending <- sign(apply(df_pred_2, 1, sum))
          
          df_pred_2 <- cbind(member_id = df_complete_2[,1], df_pred_2, p_hat)

          res_final <- list(w_bagging = res_bagging$w_hat,
                            w_rf = fit.rf, 
                            w_nnet = fit.nnet, 
                            accu_bagging = res_bagging$accu_test,
                            accu_rf = accu_rf,
                            accu_nnet = accu_nnet,
                            accu_blending = accu_blending,
                            member_list = df_pred_2)
          return(res_final)
}




