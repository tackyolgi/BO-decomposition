# differenciák összehasonlítása csoportonként

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

# korcsoport szerinti megoszlás
kor_csop_alapjan <- matrix(0, 10*5*2, 6)
kor_csop_alapjan[1,2] <-"category"
kor_csop_alapjan[1,3] <-"RF"
kor_csop_alapjan[1,4] <-"category"
kor_csop_alapjan[1,5] <-"regr"
kor_csop_alapjan[1,6] <-"elemszam"

kezdo <-2
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))

minta <- readRDS("minta_2009.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))

minta <- readRDS("minta_2010.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))


minta <- readRDS("minta_2011.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))


minta <- readRDS("minta_2012.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))


minta <- readRDS("minta_2013.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))


minta <- readRDS("minta_2014.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))


minta <- readRDS("minta_2015.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))


minta <- readRDS("minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

kezdo <-kezdo+5
kor_csop_alapjan[kezdo:(kezdo+(2*5+1)-2),1] <- mean(minta$train$ev)
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                             list(minta$train$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$kor_csop))

kezdo <- kezdo+5
kor_csop_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$kor_csop), mean))
kor_csop_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$kor_csop))

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results/ws effects")
write.csv(kor_csop_alapjan, "kor_csop_alapjan_csak_nok.csv")