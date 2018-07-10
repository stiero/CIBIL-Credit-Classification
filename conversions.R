#setwd("~/test data")

library(lubridate)

data <- read.csv("raw_data_70_new.csv", stringsAsFactors = FALSE)
data_test <- read.csv("raw_data_30_new.csv", stringsAsFactors = FALSE)
account <- read.csv("raw_account_70_new.csv", stringsAsFactors = FALSE)
account_test <- read.csv("raw_account_30_new.csv", stringsAsFactors = FALSE)
enquiry <- read.csv("raw_enquiry_70_new.csv", stringsAsFactors = FALSE)
enquiry_test <- read.csv("raw_enquiry_30_new.csv", stringsAsFactors = FALSE)

data$train <- 1
data_test$train <- 0
account$train <- 1
account_test$train <- 0
enquiry$train <- 1
enquiry_test$train <- 0

##############################################




##################################################

data$Bad_label <- as.factor(data$Bad_label)

# Dates - factors to ymd

data$dt_opened <- as.Date(ymd(data$dt_opened))
data$entry_time <- as.Date(ymd(data$entry_time))
data$feature_2 <- as.Date(ymd(data$feature_2))
#data$feature_21 <- as.Date(ymd(data$feature_21))
#data$feature_53 <- as.Date(ymd(data$feature_53))
data$feature_54 <- as.Date(ymd(data$feature_54))

account$dt_opened <- as.Date(ymd(account$dt_opened))
account$upload_dt <- as.Date(ymd(account$upload_dt))
account$opened_dt <- as.Date(ymd(account$opened_dt))
account$last_paymt_dt <- as.Date(ymd(account$last_paymt_dt))
account$closed_dt <- as.Date(ymd(account$closed_dt))
account$reporting_dt <- as.Date(ymd(account$reporting_dt))
account$paymt_str_dt <- as.Date(ymd(account$paymt_str_dt))
account$paymt_end_dt <- as.Date(ymd(account$paymt_end_dt))

enquiry$dt_opened <- as.Date(ymd(enquiry$dt_opened))
enquiry$upload_dt <- as.Date(ymd(enquiry$upload_dt))
enquiry$enquiry_dt <- as.Date(ymd(enquiry$enquiry_dt))

# Int and Num to factors

account$customer_no <- as.factor(account$customer_no)
account$acct_type <- as.factor(account$acct_type)
account$owner_indic <- as.factor(account$owner_indic)
account$paymentfrequency <- as.factor(account$paymentfrequency)

data$customer_no <- as.factor(data$customer_no)
data$feature_4 <- as.factor(data$feature_4)
data$feature_6 <- as.factor(data$feature_6)
data$feature_14 <- as.factor(data$feature_14)
data$feature_19 <- as.factor(data$feature_19)
data$feature_25 <- as.factor(data$feature_25)
data$feature_26 <- as.factor(data$feature_26)
data$feature_29 <- as.factor(data$feature_29)
data$feature_30 <- as.factor(data$feature_30)
data$feature_31 <- as.factor(data$feature_31)
data$feature_34 <- as.factor(data$feature_34)
data$feature_39 <- as.factor(data$feature_39)
data$feature_40 <- as.factor(data$feature_40)
data$feature_41 <- as.factor(data$feature_41)
data$feature_42 <- as.factor(data$feature_42)
data$feature_44 <- as.factor(data$feature_44)
data$feature_49 <- as.factor(data$feature_49)
data$feature_52 <- as.factor(data$feature_52)
data$feature_55 <- as.factor(data$feature_55)
data$feature_56 <- as.factor(data$feature_56)
data$feature_64 <- as.factor(data$feature_64)
data$feature_65 <- as.factor(data$feature_65)
data$feature_66 <- as.factor(data$feature_66)
data$feature_67 <- as.factor(data$feature_67)
data$feature_68 <- as.factor(data$feature_68)
data$feature_71 <- as.factor(data$feature_71)
data$feature_74 <- as.factor(data$feature_74)
data$feature_76 <- as.factor(data$feature_76)
data$feature_78 <- as.factor(data$feature_78)

enquiry$customer_no <- as.factor(enquiry$customer_no)
enquiry$enq_purpose <- as.factor(enquiry$enq_purpose)



# Factor to Int or Num
account$rateofinterest <- as.numeric(account$rateofinterest)


##################################

data$feature_2[is.na(data$feature_2)] <- median(data$feature_2, na.rm=TRUE)
account$opened_dt[is.na(account$opened_dt)] <- median(account$opened_dt, na.rm=TRUE)
account$last_paymt_dt[is.na(account$last_paymt_dt)] <- median(account$last_paymt_dt, na.rm=TRUE)
account$closed_dt[is.na(account$closed_dt)] <- median(account$closed_dt, na.rm=TRUE)
account$paymt_str_dt[is.na(account$paymt_str_dt)] <- median(account$paymt_str_dt, na.rm=TRUE)
account$paymt_end_dt[is.na(account$paymt_end_dt)] <- median(account$paymt_end_dt, na.rm=TRUE)
enquiry$enquiry_dt[is.na(enquiry$enquiry_dt)] <- median(enquiry$enquiry_dt, na.rm=TRUE)
enquiry$upload_dt[is.na(enquiry$upload_dt)] <- median(enquiry$upload_dt, na.rm=TRUE)
account$rateofinterest[is.na(account$rateofinterest)] <- mean(account$rateofinterest, na.rm=TRUE)

data$feature_3[is.na(data$feature_3)] <- median(data$feature_3, na.rm = TRUE)
data$feature_14 <- as.character(data$feature_14)
data$feature_14[is.na(data$feature_14)] <- "none"
data$feature_14 <- as.factor(data$feature_14)
data$feature_49 <- NULL
data$feature_74 <- NULL
data <- data[!is.na(data$feature_4),]

account$high_credit_amt[is.na(account$high_credit_amt)] <- median(account$high_credit_amt, na.rm = TRUE)
account$creditlimit[is.na(account$creditlimit)] <- median(account$creditlimit, na.rm=TRUE)
account$amt_past_due[is.na(account$amt_past_due)] <- median(account$amt_past_due, na.rm=TRUE)
account$cashlimit[is.na(account$cashlimit)] <- median(account$cashlimit, na.rm=TRUE)
account$paymentfrequency <- as.character(account$paymentfrequency)
account$paymentfrequency[is.na(account$paymentfrequency)] <- "none"
account$paymentfrequency <- as.factor(account$paymentfrequency)
account$actualpaymentamount[is.na(account$actualpaymentamount)] <- 
  median(account$actualpaymentamount, na.rm=TRUE)

enquiry$enq_purpose <- as.character(enquiry$enq_purpose)
enquiry$enq_purpose[is.na(enquiry$enq_purpose)] <- "none"
enquiry$enq_purpose <- as.factor(enquiry$enq_purpose)
enquiry$enq_amt[is.na(enquiry$enq_amt)] <- median(enquiry$enq_amt, na.rm=TRUE)




#################################### Test

data_test$Bad_label <- as.factor(data_test$Bad_label)

# Dates - factors to ymd

data_test$dt_opened <- as.Date(ymd(data_test$dt_opened))
data_test$entry_time <- as.Date(ymd(data_test$entry_time))
data_test$feature_2 <- as.Date(ymd(data_test$feature_2))
#data_test$feature_21 <- as.Date(ymd(data_test$feature_21))
#data_test$feature_53 <- as.Date(ymd(data_test$feature_53))
data_test$feature_54 <- as.Date(ymd(data_test$feature_54))

account_test$dt_opened <- as.Date(ymd(account_test$dt_opened))
account_test$upload_dt <- as.Date(ymd(account_test$upload_dt))
account_test$opened_dt <- as.Date(ymd(account_test$opened_dt))
account_test$last_paymt_dt <- as.Date(ymd(account_test$last_paymt_dt))
account_test$closed_dt <- as.Date(ymd(account_test$closed_dt))
account_test$reporting_dt <- as.Date(ymd(account_test$reporting_dt))
account_test$paymt_str_dt <- as.Date(ymd(account_test$paymt_str_dt))
account_test$paymt_end_dt <- as.Date(ymd(account_test$paymt_end_dt))

enquiry_test$dt_opened <- as.Date(ymd(enquiry_test$dt_opened))
enquiry_test$upload_dt <- as.Date(ymd(enquiry_test$upload_dt))
enquiry_test$enquiry_dt <- as.Date(ymd(enquiry_test$enquiry_dt))

# Int and Num to factors

account_test$customer_no <- as.factor(account_test$customer_no)
account_test$acct_type <- as.factor(account_test$acct_type)
account_test$owner_indic <- as.factor(account_test$owner_indic)
account_test$paymentfrequency <- as.factor(account_test$paymentfrequency)

data_test$customer_no <- as.factor(data_test$customer_no)
data_test$feature_4 <- as.factor(data_test$feature_4)
data_test$feature_6 <- as.factor(data_test$feature_6)
data_test$feature_14 <- as.factor(data_test$feature_14)
data_test$feature_19 <- as.factor(data_test$feature_19)
data_test$feature_25 <- as.factor(data_test$feature_25)
data_test$feature_26 <- as.factor(data_test$feature_26)
data_test$feature_29 <- as.factor(data_test$feature_29)
data_test$feature_30 <- as.factor(data_test$feature_30)
data_test$feature_31 <- as.factor(data_test$feature_31)
data_test$feature_34 <- as.factor(data_test$feature_34)
data_test$feature_39 <- as.factor(data_test$feature_39)
data_test$feature_40 <- as.factor(data_test$feature_40)
data_test$feature_41 <- as.factor(data_test$feature_41)
data_test$feature_42 <- as.factor(data_test$feature_42)
data_test$feature_44 <- as.factor(data_test$feature_44)
data_test$feature_49 <- as.factor(data_test$feature_49)
data_test$feature_52 <- as.factor(data_test$feature_52)
data_test$feature_55 <- as.factor(data_test$feature_55)
data_test$feature_56 <- as.factor(data_test$feature_56)
data_test$feature_64 <- as.factor(data_test$feature_64)
data_test$feature_65 <- as.factor(data_test$feature_65)
data_test$feature_66 <- as.factor(data_test$feature_66)
data_test$feature_67 <- as.factor(data_test$feature_67)
data_test$feature_68 <- as.factor(data_test$feature_68)
data_test$feature_71 <- as.factor(data_test$feature_71)
data_test$feature_74 <- as.factor(data_test$feature_74)
data_test$feature_76 <- as.factor(data_test$feature_76)
data_test$feature_78 <- as.factor(data_test$feature_78)

enquiry_test$customer_no <- as.factor(enquiry_test$customer_no)
enquiry_test$enq_purpose <- as.factor(enquiry_test$enq_purpose)



# Factor to Int or Num
account_test$rateofinterest <- as.numeric(account_test$rateofinterest)









#####################################



data_test$feature_3[is.na(data_test$feature_3)] <- median(data_test$feature_3, na.rm = TRUE)
data_test$feature_14 <- as.character(data_test$feature_14)
data_test$feature_14[is.na(data_test$feature_14)] <- "none"
data_test$feature_14 <- as.factor(data_test$feature_14)
data_test$feature_49 <- NULL
data_test$feature_74 <- NULL
data_test <- data_test[!is.na(data_test$feature_4),]



account_test$high_credit_amt[is.na(account_test$high_credit_amt)] <- median(account_test$high_credit_amt, na.rm = TRUE)
account_test$creditlimit[is.na(account_test$creditlimit)] <- median(account_test$creditlimit, na.rm=TRUE)
account_test$amt_past_due[is.na(account_test$amt_past_due)] <- median(account_test$amt_past_due, na.rm=TRUE)
account_test$cashlimit[is.na(account_test$cashlimit)] <- median(account_test$cashlimit, na.rm=TRUE)
account_test$paymentfrequency <- is.character(account_test$paymentfrequency)
account_test$paymentfrequency[is.na(account_test$paymentfrequency)] <- "none"
account_test$paymentfrequency <- is.factor(account_test$paymentfrequency)
account_test$actualpaymentamount[is.na(account_test$actualpaymentamount)] <- 
  median(account_test$actualpaymentamount, na.rm=TRUE)


enquiry_test$enq_purpose <- as.character(enquiry_test$enq_purpose)
enquiry_test$enq_purpose[is.na(enquiry_test$enq_purpose)] <- "none"
enquiry_test$enq_purpose <- as.factor(enquiry_test$enq_purpose)
enquiry_test$enq_amt[is.na(enquiry_test$enq_amt)] <- median(enquiry_test$enq_amt, na.rm=TRUE)


data_test$feature_2[is.na(data_test$feature_2)] <- median(data_test$feature_2, na.rm=TRUE)
account_test$opened_dt[is.na(account_test$opened_dt)] <- median(account_test$opened_dt, na.rm=TRUE)
account_test$last_paymt_dt[is.na(account_test$last_paymt_dt)] <- median(account_test$last_paymt_dt, na.rm=TRUE)
account_test$closed_dt[is.na(account_test$closed_dt)] <- median(account_test$closed_dt, na.rm=TRUE)
account_test$paymt_str_dt[is.na(account_test$paymt_str_dt)] <- median(account_test$paymt_str_dt, na.rm=TRUE)
account_test$paymt_end_dt[is.na(account_test$paymt_end_dt)] <- median(account_test$paymt_end_dt, na.rm=TRUE)
enquiry_test$enquiry_dt[is.na(enquiry_test$enquiry_dt)] <- median(enquiry_test$enquiry_dt, na.rm=TRUE)
enquiry_test$upload_dt[is.na(enquiry_test$upload_dt)] <- median(enquiry_test$upload_dt, na.rm=TRUE)
account_test$rateofinterest[is.na(account_test$rateofinterest)] <- mean(account_test$rateofinterest, na.rm=TRUE)












####################################
#data <- rbind(data, data_test)
#enquiry <- rbind(enquiry, enquiry_test)
#account <- rbind(account, account_test)

#data_test <- NULL
#account_test <- NULL
#enquiry_test <- NULL

account$paymentfrequency <- as.character(account$paymentfrequency)
account$paymentfrequency[is.na(account$paymentfrequency)] <- "none"
account$paymentfrequency <- as.factor(account$paymentfrequency)



