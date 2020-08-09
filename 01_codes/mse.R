# mse at ols and rf

setwd("~/bertarifa/2008/B_models")
minta <- readRDS("minta_2008.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

mse <- matrix(0,(2*9+1), 5)

kezdo <- 2

mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"

mse[1,1] <- "év"
mse[1,2] <- "adatbázis"
mse[1,3] <- "RF"
mse[1,4] <- "regr"
mse[1,5] <- "elemszam"

mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


minta <- readRDS("minta_2009.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)
  
  
minta <- readRDS("minta_2010.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


minta <- readRDS("minta_2011.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


minta <- readRDS("minta_2012.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


minta <- readRDS("minta_2013.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


minta <- readRDS("minta_2014.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


minta <- readRDS("minta_2015.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


minta <- readRDS("minta_2016.RData")

minta$train <- subset(minta$train, minta$train$nem==1)
minta$test <- subset(minta$test, minta$test$nem==1)

kezdo <- kezdo+2
mse[kezdo:(kezdo+1), 1] <- mean(minta$train$ev)
mse[kezdo, 2] <- "train"
mse[kezdo+1, 2] <- "test"
mse[kezdo, 3] <- sum((minta$train$pred_B_RF_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))
mse[kezdo, 4] <- sum((minta$train$pred_B_regr_subgr_ordered-minta$train$lnker)^2/
                       nrow(minta$train))

mse[kezdo+1, 3] <- sum((minta$test$pred_B_RF_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo+1, 4] <- sum((minta$test$pred_B_regr_subgr_ordered-minta$test$lnker)^2/
                         nrow(minta$test))
mse[kezdo, 5] <- nrow(minta$train)
mse[kezdo+1, 5] <- nrow(minta$test)


saveRDS(mse, "mse_csak_ferfiak.RData")
write.csv(mse, "mse_csak_ferfiak.csv")