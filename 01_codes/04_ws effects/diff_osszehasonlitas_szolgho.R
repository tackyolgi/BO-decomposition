# differenciák összehasonlítása csoportonként

setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data")
minta <- readRDS("minta_2008.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

# korcsoport szerinti megoszlás
szolgho_alapjan <- matrix(0, 10*10*2, 6)
szolgho_alapjan[1,2] <-"category"
szolgho_alapjan[1,3] <-"RF"
szolgho_alapjan[1,4] <- "elemszam"
szolgho_alapjan[1,5] <- "category"
szolgho_alapjan[1,6] <-"regr"


szolgho_kvant <- matrix(0, 10*10*2, 3)
szolgho_kvant[1,2] <-"category"
szolgho_kvant[1,3] <-"kvantilisek"


minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                         minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                         minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                         minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                         minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                         minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                         minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4


kezdo <-2
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                 list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                             list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


minta <- readRDS("minta_2009.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4



kezdo <-kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


minta <- readRDS("minta_2010.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4



kezdo <-kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


minta <- readRDS("minta_2011.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4



kezdo <-kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


minta <- readRDS("minta_2012.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4



kezdo <-kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))



minta <- readRDS("minta_2013.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4


kezdo <-kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


minta <- readRDS("minta_2014.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4


kezdo <-kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


minta <- readRDS("minta_2015.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4


kezdo <-kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


minta <- readRDS("minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)
minta$train$szolgho_csop[minta$train$szolgho<quantile(minta$train$szolgho, 0.25)] <- 1
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.25) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.5)] <- 2
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.5) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 0.75)] <- 3
minta$train$szolgho_csop[minta$train$szolgho>=quantile(minta$train$szolgho, 0.75) &
                           minta$train$szolgho<quantile(minta$train$szolgho, 1)] <- 4

minta$test$szolgho_csop[minta$test$szolgho<quantile(minta$test$szolgho, 0.25)] <- 1
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.25) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.5)] <- 2
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.5) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 0.75)] <- 3
minta$test$szolgho_csop[minta$test$szolgho>=quantile(minta$test$szolgho, 0.75) &
                          minta$test$szolgho<quantile(minta$test$szolgho, 1)] <- 4



kezdo <- kezdo+4
percent <- seq(0.25,1,0.25)
szolgho_kvant[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$train$szolgho, percent))



szolgho_alapjan[kezdo:(kezdo+(2*10+1)-2),1] <- mean(minta$train$ev)
szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$train$pred_diff_B_regr_ordered, 
                                                            list(minta$train$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$train$szolgho_csop))

kezdo <- kezdo+4

szolgho_kvant[kezdo:(kezdo+3), 2] <- percent
szolgho_kvant[kezdo:(kezdo+3), 3] <- as.matrix(quantile(minta$test$szolgho, percent))

szolgho_alapjan[kezdo:(kezdo+3),2:3] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                            list(minta$test$szolgho_csop), mean))
szolgho_alapjan[kezdo:(kezdo+3),4:5] <- as.matrix(aggregate(minta$test$pred_diff_B_regr_ordered, 
                                                            list(minta$test$szolgho_csop), mean))

szolgho_alapjan[kezdo:(kezdo+3),6] <- as.matrix(table(minta$test$szolgho_csop))


setwd("C:/Users/tacky/OneDrive - Corvinus University of Budapest/phd/research/technical note/data/results/ws effects")
write.csv(szolgho_alapjan, "szolgho_alapjan_csak_nok.csv")
write.csv(szolgho_kvant, "szolgho_kvant_csak_nok.csv")