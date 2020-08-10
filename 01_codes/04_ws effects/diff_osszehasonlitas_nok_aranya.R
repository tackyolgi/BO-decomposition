
minta <- readRDS("./data/minta_2008.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

categories <- 5

#  quantiles of femaleness
pivot_nok_aranya <- matrix(0, categories*3*2, 5)
pivot_nok_aranya[, 1] <- c(rep(2008, categories), 
                          rep(2012, categories), 
                          rep(2016, categories),
                          rep(2008, categories), 
                          rep(2012, categories), 
                          rep(2016, categories))
pivot_nok_aranya[, 2] <- c(rep("training", categories*3),
                          rep("test", categories*3))


minta$train$nok_aranya_csop[minta$train$nok_aranya<20] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=20 &
                           minta$train$nok_aranya<40] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=40 &
                           minta$train$nok_aranya<60] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=60 &
                           minta$train$nok_aranya<80] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=80 &
                              minta$train$nok_aranya< 100] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<20] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=20 &
                              minta$test$nok_aranya<40] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=40 &
                              minta$test$nok_aranya<60] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=60 &
                              minta$test$nok_aranya<80] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=80 &
                              minta$test$nok_aranya< 100] <- 5


pivot_nok_aranya[1:categories,3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                            list(minta$train$nok_aranya_csop), mean))
pivot_nok_aranya[1:categories,5] <- as.matrix(table(minta$train$nok_aranya_csop))

pivot_nok_aranya[(categories*3+1):(categories*3+categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                          list(minta$train$nok_aranya_csop), mean))
pivot_nok_aranya[(categories*3+1):(categories*3+categories),5] <- as.matrix(table(minta$train$nok_aranya_csop))


minta <- readRDS("./data/minta_2012.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

minta$train$nok_aranya_csop[minta$train$nok_aranya<20] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=20 &
                              minta$train$nok_aranya<40] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=40 &
                              minta$train$nok_aranya<60] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=60 &
                              minta$train$nok_aranya<80] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=80 &
                              minta$train$nok_aranya< 100] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<20] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=20 &
                             minta$test$nok_aranya<40] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=40 &
                             minta$test$nok_aranya<60] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=60 &
                             minta$test$nok_aranya<80] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=80 &
                             minta$test$nok_aranya< 100] <- 5


pivot_nok_aranya[(categories+1):(2*categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                          list(minta$train$nok_aranya_csop), mean))
pivot_nok_aranya[(categories+1):(2*categories),5] <- as.matrix(table(minta$train$nok_aranya_csop))

pivot_nok_aranya[(categories*4+1):(categories*4+categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                                        list(minta$train$nok_aranya_csop), mean))
pivot_nok_aranya[(categories*4+1):(categories*4+categories),5] <- as.matrix(table(minta$train$nok_aranya_csop))



minta <- readRDS("./data/minta_2016.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)


minta$train$nok_aranya_csop[minta$train$nok_aranya<20] <- 1
minta$train$nok_aranya_csop[minta$train$nok_aranya>=20 &
                              minta$train$nok_aranya<40] <- 2
minta$train$nok_aranya_csop[minta$train$nok_aranya>=40 &
                              minta$train$nok_aranya<60] <- 3
minta$train$nok_aranya_csop[minta$train$nok_aranya>=60 &
                              minta$train$nok_aranya<80] <- 4
minta$train$nok_aranya_csop[minta$train$nok_aranya>=80 &
                              minta$train$nok_aranya< 100] <- 5

minta$test$nok_aranya_csop[minta$test$nok_aranya<20] <- 1
minta$test$nok_aranya_csop[minta$test$nok_aranya>=20 &
                             minta$test$nok_aranya<40] <- 2
minta$test$nok_aranya_csop[minta$test$nok_aranya>=40 &
                             minta$test$nok_aranya<60] <- 3
minta$test$nok_aranya_csop[minta$test$nok_aranya>=60 &
                             minta$test$nok_aranya<80] <- 4
minta$test$nok_aranya_csop[minta$test$nok_aranya>=80 &
                             minta$test$nok_aranya< 100] <- 5

pivot_nok_aranya[(2*categories+1):(3*categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                           list(minta$train$nok_aranya_csop), mean))
pivot_nok_aranya[(2*categories+1):(3*categories),5] <- as.matrix(table(minta$train$nok_aranya_csop))

pivot_nok_aranya[(categories*5+1):(categories*5+categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                                        list(minta$train$nok_aranya_csop), mean))
pivot_nok_aranya[(categories*5+1):(categories*5+categories),5] <- as.matrix(table(minta$train$nok_aranya_csop))


pivot_nok_aranya[, 4] <- as.numeric(pivot_nok_aranya[, 4])

pivot_nok_aranya <- as.data.frame(pivot_nok_aranya)

names(pivot_nok_aranya) <-c("year", "dataset", "category", "RF", "no of obs.")

write.csv(pivot_nok_aranya, "./results/ws effects/pivot_nok_aranya.csv")



