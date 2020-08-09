# differenciák összehasonlítása csoportonként

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

# korcsoport szerinti megoszlás
feor_1_alapjan <- matrix(0, 10*5*9, 6)
feor_1_alapjan[1,2] <-"category"
feor_1_alapjan[1,3] <-"RF"
feor_1_alapjan[1,4] <-"category"
feor_1_alapjan[1,5] <-"regr"
feor_1_alapjan[1,6] <-"elemszam"

kezdo <-2
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))


minta <- readRDS("minta_2009.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))

minta <- readRDS("minta_2010.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))



minta <- readRDS("minta_2011.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))


minta <- readRDS("minta_2012.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))


minta <- readRDS("minta_2013.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))


minta <- readRDS("minta_2014.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))


minta <- readRDS("minta_2015.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))


minta <- readRDS("minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+9
feor_1_alapjan[kezdo:(kezdo+(2*9+1)-2),1] <- mean(minta$train$ev)
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                           list(minta$train$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$train$feor_1))

kezdo <- kezdo+9
feor_1_alapjan[kezdo:(kezdo+8),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                           list(minta$test$feor_1), mean))
feor_1_alapjan[kezdo:(kezdo+8),6] <- as.matrix(table(minta$test$feor_1))

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results/ws effects")
write.csv(feor_1_alapjan, "feor_1_alapjan_csak_nok.csv")