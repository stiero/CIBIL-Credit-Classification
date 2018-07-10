#setwd("~/test data")

source("conversions.R")
source("train_features.R")
source("test_features.R")

source("type_conversions.R")
library(MLmetrics)

data_new <- rbind(data_new, data_new_test)

data_new <- data_new %>% select(-Bad_label, Bad_label)

source("type_conversions.R")


train <- data_new[data_new$train == 1,]
test <- data_new_test[data_new_test$train != 1,]

train$train <- NULL
test$train <- NULL

testlabels <- test$Bad_label
testlabels <- as.numeric(testlabels)

cols <- paste(names(train), collapse="+")

scaling = 1
if(scaling){
  source("scaling.R")
}


##########################################################

# library(xgboost)
# 
# 
# trainmatrix <- model.matrix(~ dt_opened+entry_time+feature_1+feature_2+
#                               feature_3+feature_4+feature_7+feature_8+feature_9+
#                               feature_10+feature_11+feature_12+feature_13+feature_14+
#                               feature_15+feature_16+feature_17+feature_18+feature_19+
#                               feature_20+feature_21+feature_22+feature_23+feature_24+
#                               feature_25+feature_26+feature_27+feature_28+feature_29+
#                               feature_30+feature_31+feature_32+feature_33+feature_34+
#                               feature_35+feature_36+feature_37+feature_38+feature_39+
#                               feature_40+feature_41+feature_42+feature_44+feature_46+
#                               feature_47+feature_48+feature_50+feature_51+feature_52+
#                               feature_53+feature_54+feature_55+feature_56+feature_57+
#                               feature_58+feature_59+feature_60+feature_61+feature_62+
#                               feature_63+feature_64+feature_65+feature_66+feature_67+
#                               feature_68+feature_69+feature_70+feature_71+feature_72+
#                               feature_73+feature_75+feature_76+feature_77+feature_78+
#                               feature_79+
#                               Bad_label+mean_paymenthistory_avg_dpd_0_20_bucket.x+
#                               last_365day_enquiries+last_90day_enquiries+
#                               mean_diff_lastpayment_opened_dt+mean_diff_open_enquiry_dt+
#                               num_of_times_overdue+payment_history_mean_length+
#                               Ratio_currbalance_creditlimit+
#                               mean_paymenthistory_avg_dpd_0_20_bucket.y+
#                               total_enquiries+total_secured+perc_secured+
#                               utilisation_trend,
#                             data = train, options(na.action = "na.fail"))
# 
# 
# trainmatrix <- trainmatrix[,-c(1)]
# 
# labels <- train$Bad_label
# 
# xgb_model <- xgboost(data = trainmatrix,
#                      label = labels,
#                      nrounds = 10,
#                      verbose = 2,
#                      max_depth = 5,
#                      eta = 0.15,
#                      gamma = 0,
#                      booster = "gbtree",
#                      min_child_weight = 0.5,
#                      colsample_by_tree = 0.5,
#                      subsample = 1
# )
# 
# 
# test$Bad_label <- NA
# 
# testmatrix <- model.matrix(~ dt_opened+entry_time+feature_1+feature_2+
#                               feature_3+feature_4+feature_7+feature_8+feature_9+
#                               feature_10+feature_11+feature_12+feature_13+feature_14+
#                               feature_15+feature_16+feature_17+feature_18+feature_19+
#                               feature_20+feature_21+feature_22+feature_23+feature_24+
#                               feature_25+feature_26+feature_27+feature_28+feature_29+
#                               feature_30+feature_31+feature_32+feature_33+feature_34+
#                               feature_35+feature_36+feature_37+feature_38+feature_39+
#                               feature_40+feature_41+feature_42+feature_44+
#                              feature_46+feature_47+feature_48+feature_50+
#                               feature_51+feature_52+feature_53+feature_54+feature_55+
#                               feature_56+feature_57+feature_58+feature_59+feature_60+
#                               feature_61+feature_62+feature_63+feature_64+feature_65+
#                               feature_66+feature_67+feature_68+feature_69+feature_70+
#                               feature_71+feature_72+feature_73+feature_75+feature_76+
#                               feature_77+feature_78+feature_79+
#                               mean_paymenthistory_avg_dpd_0_20_bucket.x+
#                               last_365day_enquiries+last_90day_enquiries
#                            +mean_diff_lastpayment_opened_dt+
#                               mean_diff_open_enquiry_dt+num_of_times_overdue+
#                               payment_history_mean_length+Ratio_currbalance_creditlimit+
#                               mean_paymenthistory_avg_dpd_0_20_bucket.y+total_enquiries+
#                               total_secured+perc_secured+utilisation_trend,
#                            data = test, options(na.action = "na.fail"))
# 
# 
# 
# preds <- predict(xgb_model, testmatrix)
# 
# importance_matrix <- xgb.importance(model = xgb_model)
# 
# #################################
# 
# library(fastAdaboost)
# 
# adbst.model <- adaboost(Bad_label ~ dt_opened+entry_time+feature_1+feature_2+
#                           feature_3+feature_4+feature_7+feature_8+feature_9+
#                           feature_10+feature_11+feature_12+feature_13+feature_14+
#                           feature_15+feature_16+feature_17+feature_18+feature_19+
#                           feature_20+feature_21+feature_22+feature_23+feature_24+
#                           feature_25+feature_26+feature_27+feature_28+feature_29+
#                           feature_30+feature_31+feature_32+feature_33+feature_34+
#                           feature_35+feature_36+feature_37+feature_38+feature_39+
#                           feature_40+feature_41+feature_42+feature_44+feature_46+feature_47+feature_48+feature_50+
#                           feature_51+feature_52+feature_53+feature_54+feature_55+
#                           feature_56+feature_57+feature_58+feature_59+feature_60+
#                           feature_61+feature_62+feature_63+feature_64+feature_65+
#                           feature_66+feature_67+feature_68+feature_69+feature_70+
#                           feature_71+feature_72+feature_73+feature_75+feature_76+
#                           feature_77+feature_78+feature_79+
#                           mean_paymenthistory_avg_dpd_0_20_bucket.x+
#                           last_365day_enquiries+last_90day_enquiries
#                         +mean_diff_lastpayment_opened_dt+
#                           mean_diff_open_enquiry_dt+num_of_times_overdue+
#                           payment_history_mean_length+Ratio_currbalance_creditlimit+
#                           mean_paymenthistory_avg_dpd_0_20_bucket.y+total_enquiries+
#                           total_secured+perc_secured+utilisation_trend,
#                         data = train, nIter = 2)
# 
# 
# pred_adabst <- predict(adbst.model, test)

###################################

library(adabag)

ctrl <- rpart.control(max_depth=10, minsplit=50)

adabst.model <- boosting(Bad_label ~ dt_opened+entry_time+feature_1+
                           feature_2+feature_3+feature_4+feature_7+feature_8+feature_9+
                           feature_10+feature_11+feature_12+feature_13+feature_14+
                           feature_15+feature_16+feature_17+feature_18+feature_19+
                           feature_23+feature_25+feature_26+feature_27+feature_28+
                           feature_29+feature_30+feature_31+feature_32+feature_33+
                           feature_34+feature_35+feature_36+feature_37+feature_39+
                           feature_40+feature_41+feature_42+feature_46+
                           feature_48+feature_50+feature_51+feature_52+feature_53+
                           feature_54+feature_55+feature_56+feature_57+feature_58+
                           feature_59+feature_60+feature_61+feature_62+
                           feature_64+feature_65+feature_67+feature_68+
                           feature_69+feature_70+feature_71+feature_72+feature_73+
                           feature_75+feature_76+feature_78+feature_79+
                           mean_paymenthistory_avg_dpd_0_20_bucket.x+
                           last_365day_enquiries+last_90day_enquiries+
                           mean_diff_lastpayment_opened_dt+mean_diff_open_enquiry_dt+
                           num_of_times_overdue+payment_history_mean_length+
                           Ratio_currbalance_creditlimit+
                           mean_paymenthistory_avg_dpd_0_20_bucket.y+total_enquiries+
                           total_secured+perc_secured+utilisation_trend+avg_diff_payment+
                           median_high_credit+median_credit_limit+median_cash_limit+
                           mean_rateofinterest+median_actualpaymentamount+median_enq_amtt,
                        data = train, boos = TRUE, mfinal = 100,
                        control = ctrl)

pred_adabst <- predict(adabst.model, newdata = test)
preds <- as.numeric(pred_adabst$class)


Gini(preds, testlabels)


######################### 

library(randomForest)

train_rf <- train %>% select(-feature_15, -feature_16, -feature_17, -feature_20,
                             -feature_21, -feature_22, -feature_24, -feature_28,
                             -feature_29, -feature_30, -feature_38, -feature_44,
                             -feature_47, -feature_63, -feature_65, -feature_66,
                             -feature_70, -feature_75, -feature_77, -feature_53,
                             -feature_48)

cols_rf <- paste(names(train_rf), collapse="+")


train_rf$feature_1 <- as.factor(train_rf$feature_1)

# test_rf <- test %>% select(-feature_15, -feature_16, -feature_17, -feature_20,
#                             -feature_21, -feature_22, -feature_24, -feature_28,
#                             -feature_29, -feature_30, -feature_38, -feature_44,
#                             -feature_47, -feature_63, -feature_65, -feature_66,
#                             -feature_70, -feature_75, -feature_77, -feature_53,
#                             -feature_48)
# 
# test_rf$feature_1 <- as.factor(test$feature_1)
# 
# test_rf <- rbind(train_rf[1,], test_rf)
# test_rf <- test_rf[-1,]


rf.model <- randomForest(Bad_label ~ dt_opened+entry_time+feature_1+feature_2+feature_3+feature_4+
                           feature_7+feature_8+feature_9+feature_10+feature_11+feature_12+
                           feature_13+feature_14+feature_18+feature_19+feature_23+
                           feature_25+feature_26+feature_27+feature_31+feature_32+
                           feature_33+feature_34+feature_35+feature_36+feature_37+
                           feature_39+feature_40+feature_41+feature_42+feature_46+
                           feature_50+feature_51+feature_52+
                           feature_54+feature_55+feature_56+feature_57+feature_58+
                           feature_59+feature_60+feature_61+feature_62+feature_64+
                           feature_67+feature_68+feature_69+feature_71+feature_72+
                           feature_73+feature_76+feature_78+feature_79+
                           mean_paymenthistory_avg_dpd_0_20_bucket.x+
                           last_365day_enquiries+last_90day_enquiries+
                           mean_diff_lastpayment_opened_dt+mean_diff_open_enquiry_dt+
                           num_of_times_overdue+payment_history_mean_length+
                           Ratio_currbalance_creditlimit+
                           mean_paymenthistory_avg_dpd_0_20_bucket.y+total_enquiries+
                           total_secured+perc_secured+utilisation_trend+avg_diff_payment+
                           median_high_credit+median_credit_limit+median_cash_limit+
                           mean_rateofinterest+median_actualpaymentamount+median_enq_amtt,
                         ntree=1000, mtry = 20, importance=TRUE, data = train_rf)

pred_rf <- predict(rf.model, newdata = test_rf)

preds <- as.numeric(pred_rf)


Gini(preds, testlabels)

rf.model$importance
rf.model$confusion
