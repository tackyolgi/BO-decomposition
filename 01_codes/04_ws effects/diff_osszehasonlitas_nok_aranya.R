# differenciák összehasonlítása csoportonként

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

# korcsoport szerinti megoszlás
nok_aranya_alapjan <- matrix(0, 10*10*2, 6)
nok_aranya_alapjan[1,2] <-"category"
nok_aranya_alapjan[1,3] <-"RF"
nok_aranya_alapjan[1,4] <-"category"
nok_aranya_alapjan[1,5] <-"regr"
nok_aranya_alapjan[1,6] <- "elemszam"

nok_aranya_kvant <- matrix(0, 10*10*2, 3)
nok_aranya_kvant[1,2] <-"category"
nok_aranya_kvant[1,3] <-"kvantilisek"


minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                           minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                           minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                           minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                          minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                          minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                          minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5


kezdo <-2
percent <- seq(0.2,1,0.2)
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))


minta <- readRDS("minta_2009.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5




kezdo <-kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))

minta <- readRDS("minta_2010.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5




kezdo <-kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))


minta <- readRDS("minta_2011.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5



kezdo <-kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))

minta <- readRDS("minta_2012.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5




kezdo <-kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))



minta <- readRDS("minta_2013.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5



kezdo <-kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))


minta <- readRDS("minta_2014.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5


kezdo <-kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))

minta <- readRDS("minta_2015.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5


kezdo <-kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))


minta <- readRDS("minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$nok_aranya_csop[minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.20)] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.20) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.4)] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.4) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.6)] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.6) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 0.8)] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=quantile(minta$train$nok_aranya, 0.8) &
                              minta$train$nok_aranya<quantile(minta$train$nok_aranya, 1)] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.2)] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.2) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.4)] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.4) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.6)] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.6) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 0.8)] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=quantile(minta$test$nok_aranya, 0.8) &
                             minta$test$nok_aranya<quantile(minta$test$nok_aranya, 1)] <- 5



kezdo <- kezdo+5
nok_aranya_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$train$nok_aranya, percent))



nok_aranya_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                               list(minta$train$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$train$nok_aranya_csop))

kezdo <- kezdo+5

nok_aranya_kvant[kezdo:(kezdo+4), 2] <- percent
nok_aranya_kvant[kezdo:(kezdo+4), 3] <- as.matrix(quantile(minta$test$nok_aranya, percent))

nok_aranya_alapjan[kezdo:(kezdo+4),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                               list(minta$test$nok_aranya_csop), mean))
nok_aranya_alapjan[kezdo:(kezdo+4),6] <- as.matrix(table(minta$test$nok_aranya_csop))


setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results/ws effects")
write.csv(nok_aranya_alapjan, "nok_aranya_alapjan_csak_nok.csv")
write.csv(nok_aranya_kvant, "nok_aranya_kvant_csak_nok.csv")