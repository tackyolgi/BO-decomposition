# education categorization

minta <- readRDS("./data/minta_2008.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

categories <- 9

pivot_feor <- matrix(0, categories*3*2, 5)
pivot_feor[, 1] <- c(rep(2008, categories), 
                        rep(2012, categories), 
                        rep(2016, categories),
                        rep(2008, categories), 
                        rep(2012, categories), 
                        rep(2016, categories))
pivot_feor[, 2] <- c(rep("training", categories*3),
                        rep("test", categories*3))

pivot_feor[1:categories,3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                             list(minta$train$feor_1), mean))
pivot_feor[1:categories,5] <- as.matrix(table(minta$train$feor_1))

pivot_feor[(categories*3+1):(categories*3+categories),3:4] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                             list(minta$test$feor_1), mean))
pivot_feor[(categories*3+1):(categories*3+categories),5] <- as.matrix(table(minta$test$feor_1))


minta <- readRDS("./data/minta_2012.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

pivot_feor[(categories+1):(2*categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                       list(minta$train$feor_1), mean))
pivot_feor[(categories+1):(2*categories),5] <- as.matrix(table(minta$train$feor_1))

pivot_feor[(categories*4+1):(categories*4+categories),3:4] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                                     list(minta$test$feor_1), mean))
pivot_feor[(categories*4+1):(categories*4+categories),5] <- as.matrix(table(minta$test$feor_1))



minta <- readRDS("./data/minta_2016.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

pivot_feor[(2*categories+1):(3*categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                        list(minta$train$feor_1), mean))
pivot_feor[(2*categories+1):(3*categories),5] <- as.matrix(table(minta$train$feor_1))

pivot_feor[(categories*5+1):(categories*5+categories),3:4] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                                     list(minta$test$feor_1), mean))
pivot_feor[(categories*5+1):(categories*5+categories),5] <- as.matrix(table(minta$test$feor_1))


pivot_feor[, 4] <- as.numeric(pivot_feor[, 4])

pivot_feor <- as.data.frame(pivot_feor)

names(pivot_feor) <-c("year", "dataset", "category", "RF", "no of obs.")

write.csv(pivot_feor, "./results/ws effects/pivot_feor.csv")