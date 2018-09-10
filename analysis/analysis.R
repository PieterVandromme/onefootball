# OneFootball coding challenge main script to predict user churn for May 2018
# by Pieter Vandromme

# this file do not include any exploratory analysis and models evaluation
# see assignment.Rmd for the detailed analysis and comments

if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if (!require(lubridate)) install.packages("lubridate")
library(lubridate)

if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)

if (!require(abind)) install.packages("abind")
library(abind)

if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

if (!require(caret)) install.packages("caret")
library(caret)

if (!require(party)) install.packages("party")
library(party)

if (!require(randomForest)) install.packages("randomForest")
library(randomForest)

if (!require(e1071)) install.packages("e1071")
library(e1071)


# Load data
userAct <- read.csv(file = "./data/user_activity.csv", header = TRUE)
userAct <- as.tibble(userAct)


# data preprocessing
userAct$predictions_made[is.na(userAct$predictions_made)] = 0
userAct$month_at <- ymd(userAct$month_at)
userAct$first_install_date <- ymd(userAct$first_install_date)
userAct$last_session_date <- ymd(userAct$last_session_date)
userAct$has_favourite_team <- as.numeric(userAct$has_favourite_team)

# dataframe of metadata per users
userMeta <- userAct %>% select(user_id, first_install_date, country, has_favourite_team, operating_system)
userMeta <- unique(userMeta)

# remove from userAct
userAct <- userAct %>% select(-first_install_date, -country, -has_favourite_team, -operating_system)


# activity per user per month
userActPerMonth <- userAct %>% mutate(month = month(month_at,label = TRUE, abbr = TRUE)) %>%
  select(user_id, month, session_count) %>%
  spread(month, session_count)
userActPerMonth <- merge(x = userActPerMonth, y = userMeta[ , c("user_id","first_install_date")], by = "user_id")
userActPerMonth <- as.tibble(userActPerMonth)


# churn per month
userFebChurn <- userActPerMonth %>% filter(first_install_date < "2018-02-01") %>%
  transmute(user_id, churn = is.na(Feb))
userMarChurn <- userActPerMonth %>% filter(first_install_date < "2018-03-01") %>%
  transmute(user_id, churn = is.na(Mar))
userAprChurn <- userActPerMonth %>% filter(first_install_date < "2018-04-01") %>%
  transmute(user_id, churn = is.na(Mar))

# build a data.frame to use for analysis

temp.df <- userAct %>% filter(user_id %in% userFebChurn$user_id & month_at == "2018-01-01") %>%
  select(-month_at, -last_session_date)
userFebChurnData <- left_join(userFebChurn, userMeta, by = "user_id")
userFebChurnData <- left_join(userFebChurnData, unique(temp.df), by = "user_id")
userFebChurnData[is.na(userFebChurnData)] <- 0
userFebChurnData <- userFebChurnData %>% mutate(user_seniority = ymd("2018-02-01") - first_install_date)

temp.df <- userAct %>% filter(user_id %in% userMarChurn$user_id & month_at == "2018-02-01") %>%
  select(-month_at, -last_session_date)
userMarChurnData <- left_join(userMarChurn, userMeta, by = "user_id")
userMarChurnData <- left_join(userMarChurnData, unique(temp.df), by = "user_id")
userMarChurnData[is.na(userMarChurnData)] <- 0
userMarChurnData <- userMarChurnData %>% mutate(user_seniority = ymd("2018-03-01") - first_install_date)

temp.df <- userAct %>% filter(user_id %in% userAprChurn$user_id & month_at == "2018-03-01") %>%
  select(-month_at, -last_session_date)
userAprChurnData <- left_join(userAprChurn, userMeta, by = "user_id")
userAprChurnData <- left_join(userAprChurnData, unique(temp.df), by = "user_id")
userAprChurnData[is.na(userAprChurnData)] <- 0
userAprChurnData <- userAprChurnData %>% mutate(user_seniority = ymd("2018-04-01") - first_install_date)

# bind the months together
userMonthlyChurn <- bind_rows(userFebChurnData,userMarChurnData,userAprChurnData)
userMonthlyChurn$user_seniority <- as.numeric(userMonthlyChurn$user_seniority)

# clear up a bit our environment of dataframe we no longer need
rm(temp.df, userFebChurn, userFebChurnData, userMarChurn, userMarChurnData, userAprChurn, userAprChurnData)

# preparing data for model prediction
# select only the variables we need
churn.data <- userMonthlyChurn %>%
  select(-user_id, -first_install_date)

# see assignment.Rmd for model evaluation

# logistic on all data
logModel <- glm(churn ~ ., family = binomial(link = "logit"), data = churn.data)

# estimation of May Churn
userApr <- userActPerMonth %>% filter(first_install_date < "2018-05-01") %>% select(user_id)

temp.df <- userAct %>% filter(user_id %in% userApr$user_id & month_at == "2018-04-01") %>%
  select(-month_at, -last_session_date)
userAprData <- left_join(userApr, userMeta, by = "user_id")
userAprData <- left_join(userAprData, unique(temp.df), by = "user_id")
userAprData[is.na(userAprData)] <- 0
userAprData <- userAprData %>% mutate(user_seniority = ymd("2018-05-01") - first_install_date)
userAprData$user_seniority <- as.numeric(userAprData$user_seniority)

# predict using the logistic model
logPredMay <- predict(logModel, newdata = userAprData)
logPredMayTF <- logPredMay > 0.5

# gather user_id and prediction for May in a single dataframe
userClass <- data.frame(userMeta$user_id,logPredMayTF)
names(userClass) <- c("user_id","churn_may_estimate")

# save it under user_classified.csv
write.csv(userClass,"./data/user_classified.csv")

  