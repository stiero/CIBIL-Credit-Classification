library(dplyr)
library(lubridate)
#library(tidyr)

#new <- account %>% group_by(customer_no) %>% summarise(which.max(table(acct_type)))

#new <- account %>% group_by(customer_no) %>% count(acct_type) %>% arrange(desc(n))

#new1 <- account %>% group_by(customer_no) %>% count(acct_type) %>% 
#  arrange(desc(n)) %>% slice(1) %>% select(-n)

#data <- left_join(data, new1, by="customer_no")

account_new <- account

######################## 2
account_new$total_diff_lastpaymt_opened_dt <- difftime(account_new$dt_opened, account_new$last_paymt_dt,
                                                       units = "days")
########################### 1

account_new$paymenthistory_avg_dpd_0_29_bucket <- gsub("(.{3})", "\\1,", account_new$paymenthistory1)

account_new$paymenthistory_avg_dpd_0_29_bucket <- substring(account_new$paymenthistory_avg_dpd_0_29_bucket, 5)

account_new$paymenthistory_avg_dpd_0_29_bucket <- substr(account_new$paymenthistory_avg_dpd_0_29_bucket, 1, nchar(account_new$paymenthistory_avg_dpd_0_29_bucket) - 5)


#account_new$paymenthistory_avg_dpd_0_29_bucket <- lapply(account_new$paymenthistory_avg_dpd_0_29_bucket,
#                                                         FUN = function(x) { c(x) })

account_new$paymenthistory_avg_dpd_0_29_bucket <- sapply(account_new$paymenthistory_avg_dpd_0_29_bucket,
                                                         FUN = function(x) unlist(strsplit(x, split = ",")))

account_new$sum_paymenthistory_avg_dpd_0_29_bucket <- lapply(account_new$paymenthistory_avg_dpd_0_29_bucket,
                                                         FUN = function(x) sum(as.numeric(as.character(x[x < 30])), na.rm=TRUE))

account_new$sum_paymenthistory_avg_dpd_0_29_bucket <- unlist(unname(account_new$sum_paymenthistory_avg_dpd_0_29_bucket))

account_new$sum_paymenthistory_avg_dpd_0_29_bucket[is.nan(account_new$sum_paymenthistory_avg_dpd_0_29_bucket)] <-
  NA

sum_paymenthistory_avg_dpd_0_29_bucket <- account_new %>% select(customer_no, sum_paymenthistory_avg_dpd_0_29_bucket) %>%
  group_by(customer_no) %>% summarise("mean_paymenthistory_avg_dpd_0_20_bucket" = mean(sum_paymenthistory_avg_dpd_0_29_bucket))
############################## 4

utilisation_trend <- account_new %>% select(customer_no, creditlimit, cur_balance_amt) %>% 
  group_by(customer_no) %>% summarise("utilisation_trend" = sum(cur_balance_amt) / sum(creditlimit)) 
#%>% select(-c(customer_no, creditlimit, cur_balance_amt))


#JOIN THIS to data
##############################  5, 11

#Some dates are way into the future. Count for last 365 days will be very few in this case

last_365day_enquiries <- enquiry %>% select(customer_no, enquiry_dt) %>% group_by(customer_no) %>% 
  mutate("last_enquiry_date" = max(enquiry_dt), "difference" = difftime(last_enquiry_date, enquiry_dt, units = c("days")))


last_365day_enquiries <- last_365day_enquiries %>% filter(difference <= 365 && difference > 0) %>% 
  group_by(customer_no) %>% summarise(last_365day_enquiries = n())

last_90day_enquiries <- enquiry %>% select(customer_no, enquiry_dt) %>% group_by(customer_no) %>% 
  mutate("last_enquiry_date" = max(enquiry_dt), "difference" = difftime(last_enquiry_date, enquiry_dt, units = c("days")))


last_90day_enquiries <- last_90day_enquiries %>% filter(difference <= 90 && difference > 0) %>%
  group_by(customer_no) %>% summarise(last_90day_enquiries = n())

############################### 6

ratio_currbalance_creditlimit <- account %>% select(customer_no, cur_balance_amt, creditlimit) %>%
  group_by(customer_no) %>% summarise("Ratio_currbalance_creditlimit" = sum(cur_balance_amt) / sum(creditlimit))

############################### 7

mean_diff_lastpayment_opened_dt <- account %>% select(customer_no, last_paymt_dt, dt_opened) %>%
  mutate("mean_diff_lastpayment_opened_dt" = abs(difftime(last_paymt_dt, dt_opened, units=c("days")))) %>%
  group_by(customer_no) %>% summarise("mean_diff_lastpayment_opened_dt" = mean(mean_diff_lastpayment_opened_dt))

######################### 8
mean_diff_open_enquiry_dt <- enquiry %>% select(customer_no, dt_opened, enquiry_dt) %>%
  mutate("mean_diff_open_enquiry_dt" = abs(difftime(dt_opened, enquiry_dt, units = c("days")))) %>%
  group_by(customer_no) %>% summarise("mean_diff_open_enquiry_dt" = mean(mean_diff_open_enquiry_dt))

################################### 9

payment_history_mean_length <- account %>% select(customer_no, paymenthistory1) %>% 
  mutate("length" = nchar(as.character(paymenthistory1)) - 6) %>% group_by(customer_no) %>%
  summarise("payment_history_mean_length" = mean(length))

################################## 10

max_freq_enquiry <- enquiry %>% select(customer_no, enq_purpose) %>% group_by(customer_no) %>%
  count(enq_purpose) %>% arrange(desc(n)) %>% group_by(customer_no) %>% slice(1) %>% select(-n)

#################################### 12 temp

secured_loan_types = c(1:4, 7, 11, 13:15, 17, 31:34, 42, 51:59)


total_enquiries <- enquiry %>% select(customer_no, enq_purpose) %>% group_by(customer_no) %>%
  summarise("total_enquiries" = n())

perc_unsecured_others <- enquiry %>% select(customer_no, enq_purpose) %>% 
  filter(enq_purpose %in% secured_loan_types) %>% group_by(customer_no) %>% summarise("total_secured" = n())

temp <- left_join(total_enquiries, perc_unsecured_others, by="customer_no")

temp$total_secured[is.na(temp$total_secured)] <- 0

temp$perc_secured <- temp$total_secured / temp$total_enquiries
  

############################ Avg amount overdue 

avg_overdue <- account %>% select(customer_no, amt_past_due) %>% group_by(customer_no) %>% 
  summarise("avg_overdue" = mean(amt_past_due, na.rm=TRUE))

avg_overdue$avg_overdue[is.nan(avg_overdue$avg_overdue)] <- 0
  
###################### No of times overdue

num_of_times_overdue <- account %>% select(customer_no, amt_past_due) %>% group_by(customer_no) %>%
  summarise("num_of_times_overdue" = n())
  
######################  

diff_payment <- account %>% select(customer_no, paymt_str_dt, paymt_end_dt) %>%
  mutate("diff_payment" = as.numeric(abs(difftime(paymt_end_dt, paymt_str_dt, 
                                              units=c("days"))))) %>%
  select(-paymt_str_dt, -paymt_end_dt) %>% group_by(customer_no) %>%
  summarise("avg_diff_payment" = mean(diff_payment)) 

####################

median_high_credit <- account %>% select(customer_no, high_credit_amt) %>%
  group_by(customer_no) %>% summarise("median_high_credit" = 
                                        median(high_credit_amt, na.rm=TRUE))


median_credit_limit <- account %>% select(customer_no, creditlimit) %>%
  group_by(customer_no) %>% summarise("median_credit_limit" = 
                                        median(creditlimit, na.rm=TRUE))


median_cash_limit <- account %>% select(customer_no, cashlimit) %>%
  group_by(customer_no) %>% summarise("median_cash_limit" = 
                                        median(cashlimit, na.rm=TRUE))


mean_rateofinterest <- account %>% select(customer_no, rateofinterest) %>%
  group_by(customer_no) %>% summarise("mean_rateofinterest" = 
                                        mean(rateofinterest, na.rm=TRUE))


median_actualpaymentamount <- account %>% select(customer_no, actualpaymentamount) %>% 
  group_by(customer_no) %>% summarise("median_actualpaymentamount" = 
                                        median(actualpaymentamount, na.rm=TRUE))

median_enq_amt <- enquiry %>% select(customer_no, enq_amt) %>% 
  group_by(customer_no) %>% summarise("median_enq_amtt" = 
                                        median(enq_amt, na.rm=TRUE))









data_new <- left_join(data, sum_paymenthistory_avg_dpd_0_29_bucket, by=c("customer_no"))

data_new <- left_join(data_new, last_365day_enquiries, by=c("customer_no"))
data_new$last_365day_enquiries[is.na(data_new$last_365day_enquiries)] <- 0

data_new <- left_join(data_new, last_90day_enquiries, by=c("customer_no"))
data_new$last_90day_enquiries[is.na(data_new$last_90day_enquiries)] <- 0


#data_new <- left_join(data_new, max_freq_enquiry, by=c("customer_no"))

data_new <- left_join(data_new, mean_diff_lastpayment_opened_dt, by=c("customer_no"))

data_new <- left_join(data_new, mean_diff_open_enquiry_dt, by=c("customer_no"))

data_new <- left_join(data_new, num_of_times_overdue, by=c("customer_no"))

data_new <- left_join(data_new, payment_history_mean_length, by=c("customer_no"))

data_new <- left_join(data_new, ratio_currbalance_creditlimit, by=c("customer_no"))

data_new <- left_join(data_new, sum_paymenthistory_avg_dpd_0_29_bucket, by=("customer_no"))

data_new <- left_join(data_new, temp, by=("customer_no"))

data_new <- left_join(data_new, utilisation_trend, by=c("customer_no"))

data_new <- left_join(data_new, diff_payment, by="customer_no")

data_new <- left_join(data_new, median_high_credit, by="customer_no")

data_new <- left_join(data_new, median_credit_limit, by="customer_no")

data_new <- left_join(data_new, median_cash_limit, by="customer_no")

data_new <- left_join(data_new, mean_rateofinterest, by="customer_no")

data_new <- left_join(data_new, median_actualpaymentamount, by="customer_no")

data_new <- left_join(data_new, median_enq_amt, by="customer_no")

############### Fixing NAs ##################

#data_new$customer_no <- NULL
#data_new$feature_49 <- NULL #Mostly NAs
#data_new$feature_14[is.na(data_new$feature_14)] <- names(which.max(table(data_new$feature_14)))

#data_new$feature_3[is.na(data_new$feature_3)] <- median(data_new$feature_3, na.rm = TRUE)

#data_new$feature_74 <- NULL #Mostly NAs

#data_new$Ratio_currbalance_creditlimit[is.na(data_new$Ratio_currbalance_creditlimit)] <- 
#  median(data_new$Ratio_currbalance_creditlimit, na.rm = TRUE)

#data_new$mean_diff_lastpayment_opened_dt <- median(as.numeric(data_new$mean_diff_lastpayment_opened_dt))

#data_new$utilisation_trend <- median(data_new$utilisation_trend, na.rm=TRUE)

#data_new$mean_paymenthistory_avg_dpd_0_20_bucket.y <- NULL

#data_new$enq_purpose[is.na(data_new$enq_purpose)] <- names(which.max(table(data_new$enq_purpose)))

#data_new$mean_diff_open_enquiry_dt[is.na(data_new$mean_diff_open_enquiry_dt)] <- 
#  median(as.numeric(data_new$mean_diff_open_enquiry_dt), na.rm=TRUE)


#data_new$feature_2[is.na(data_new$feature_2)] <- median(data_new$feature_2, na.rm=TRUE)

#data_new$feature_8 <- as.character(data_new$feature_8)
#data_new$feature_8[data_new$feature_8 == ""] <- "None"
#data_new$feature_8 <- as.factor(data_new$feature_8)

#data_new$feature_9 <- as.character(data_new$feature_9)
#data_new$feature_9[data_new$feature_9 == ""] <- "None"
#data_new$feature_9 <- as.factor(data_new$feature_9)

#data_new$feature_10 <- as.character(data_new$feature_10)
#data_new$feature_10[data_new$feature_10 == ""] <- "None"
#data_new$feature_10 <- as.factor(data_new$feature_10)

#data_new$feature_9 <- as.character(data_new$feature_9)
#data_new$feature_9[data_new$feature_9 == ""] <- "None"
#data_new$feature_9 <- as.factor(data_new$feature_9)

#data_new$feature_13 <- as.character(data_new$feature_13)
#data_new$feature_13[data_new$feature_13 == ""] <- "None"
#data_new$feature_13 <- as.factor(data_new$feature_13)

#aa <- data_new[!is.na(data_new$entry_time),]

#data_new$feature_6 <- NULL #Uninformative


#write.csv(data_new, "combined.csv")

##############################################

#data_new <- data_new[!is.na(data_new$entry_time),]










