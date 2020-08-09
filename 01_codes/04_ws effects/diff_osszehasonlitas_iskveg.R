# differenciák összehasonlítása csoportonként

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


# korcsoport szerinti megoszlás
iskveg4_alapjan <- matrix(0, 10*4*2, 6)
iskveg4_alapjan[1,2] <-"category"
iskveg4_alapjan[1,3] <-"RF"
iskveg4_alapjan[1,6] <- "elemszam"
iskveg4_alapjan[1,4] <- "category"
iskveg4_alapjan[1,5] <-"OLS"

kezdo <-2
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                 list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$iskveg4_ordered), mean))


minta <- readRDS("minta_2009.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))

minta <- readRDS("minta_2010.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))

minta <- readRDS("minta_2011.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))

minta <- readRDS("minta_2012.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))


minta <- readRDS("minta_2013.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))


minta <- readRDS("minta_2014.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))

minta <- readRDS("minta_2015.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))

minta <- readRDS("minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
iskveg4_alapjan[kezdo:(kezdo+(2*4+1)-2),1] <- mean(minta$train$ev)
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$iskveg4_ordered), mean))


kezdo <- kezdo+4
iskveg4_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))
iskveg4_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$iskveg4_ordered))
iskveg4_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$iskveg4_ordered), mean))

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results/ws effects")
write.csv(iskveg4_alapjan, "iskveg4_alapjan_csak_nok.csv")