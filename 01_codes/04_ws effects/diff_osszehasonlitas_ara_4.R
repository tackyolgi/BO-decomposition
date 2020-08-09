# differenciák összehasonlítása csoportonként

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

# korcsoport szerinti megoszlás
ara_ordered_alapjan <- matrix(0, 10*5*2, 6)
ara_ordered_alapjan[1,2] <-"category"
ara_ordered_alapjan[1,3] <-"RF"
ara_ordered_alapjan[1,4] <-"category"
ara_ordered_alapjan[1,5] <-"regr"
ara_ordered_alapjan[1,6] <-"elemszam"

kezdo <-2
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))


minta <- readRDS("minta_2009.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))


minta <- readRDS("minta_2010.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))



minta <- readRDS("minta_2011.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))



minta <- readRDS("minta_2012.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))



minta <- readRDS("minta_2013.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))



minta <- readRDS("minta_2014.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))



minta <- readRDS("minta_2015.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))



minta <- readRDS("minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$ara_ordered))

kezdo <- kezdo+4
ara_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$ara_ordered), mean))
ara_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$ara_ordered))


setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results/ws effects")
write.csv(ara_ordered_alapjan, "ara_ordered_alapjan_csak_nok.csv")