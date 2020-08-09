# differenciák összehasonlítása csoportonként

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

# korcsoport szerinti megoszlás
kra_ordered_alapjan <- matrix(0, 10*5*2, 6)
kra_ordered_alapjan[1,2] <-"category"
kra_ordered_alapjan[1,3] <-"RF"
kra_ordered_alapjan[1,4] <-"category"
kra_ordered_alapjan[1,5] <-"regr"
kra_ordered_alapjan[1,6] <- "elemszam"

kezdo <-2
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))


minta <- readRDS("minta_2009.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))


minta <- readRDS("minta_2010.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))


minta <- readRDS("minta_2011.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))



minta <- readRDS("minta_2012.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))



minta <- readRDS("minta_2013.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))



minta <- readRDS("minta_2014.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))



minta <- readRDS("minta_2015.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))



minta <- readRDS("minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+(2*4+3)-2),1] <- mean(minta$train$ev)
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                                list(minta$train$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$kra_ordered))

kezdo <- kezdo+4
kra_ordered_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                                list(minta$test$kra_ordered), mean))
kra_ordered_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$kra_ordered))


setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results/ws effects")
write.csv(kra_ordered_alapjan, "kra_ordered_alapjan_csak_nok.csv")
