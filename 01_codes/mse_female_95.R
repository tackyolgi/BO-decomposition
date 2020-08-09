# mse at ols and rf

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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


minta <- readRDS("minta_2009_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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
  
  
minta <- readRDS("minta_2010_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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


minta <- readRDS("minta_2011_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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


minta <- readRDS("minta_2012_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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


minta <- readRDS("minta_2013_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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


minta <- readRDS("minta_2014_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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


minta <- readRDS("minta_2015_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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


minta <- readRDS("minta_2016_95.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

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

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results")

saveRDS(mse, "mse_csak_nok_95.RData")
write.csv(mse, "mse_csak_nok_95.csv")