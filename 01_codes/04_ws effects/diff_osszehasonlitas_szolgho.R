
minta <- readRDS("./data/minta_2008.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

categories <- 4
percent <- seq(0.25, 1, 0.25)

# tenure quintiles 
pivot_szolgho <- matrix(0, categories*3*2, 6)

pivot_szolgho[, 1] <- c(rep(2008, categories), 
                        rep(2012, categories), 
                        rep(2016, categories),
                        rep(2008, categories), 
                        rep(2012, categories), 
                        rep(2016, categories))
pivot_szolgho[, 2] <- c(rep("training", categories*3),
                        rep("test", categories*3))

pivot_szolgho[1:categories,3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                    list(minta$train$szolgho_csop), mean))
pivot_szolgho[1:categories,5] <- as.matrix(table(minta$train$szolgho_csop))

pivot_szolgho[1:categories,6] <- as.matrix(quantile(minta$train$szolgho, percent))

pivot_szolgho[(categories*3+1):(categories*3+categories),3:4] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                                  list(minta$test$szolgho_csop), mean))
pivot_szolgho[(categories*3+1):(categories*3+categories),5] <- as.matrix(table(minta$test$szolgho_csop))

pivot_szolgho[(categories*3+1):(categories*3+categories),6] <- as.matrix(quantile(minta$test$szolgho, percent))

minta <- readRDS("./data/minta_2012.RData")

minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

pivot_szolgho[(categories+1):(2*categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                     list(minta$train$szolgho_csop), mean))
pivot_szolgho[(categories+1):(2*categories),5] <- as.matrix(table(minta$train$szolgho_csop))

pivot_szolgho[(categories+1):(2*categories),6] <- as.matrix(quantile(minta$train$szolgho, percent))

pivot_szolgho[(categories*4+1):(categories*4+categories),3:4] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                                  list(minta$test$szolgho_csop), mean))
pivot_szolgho[(categories*4+1):(categories*4+categories),5] <- as.matrix(table(minta$test$szolgho_csop))

pivot_szolgho[(categories*4+1):(categories*4+categories),6] <- as.matrix(quantile(minta$test$szolgho, percent))


minta <- readRDS("./data/minta_2016.RData")
minta$train <- subset(minta$train, minta$train$nem==0)
minta$test <- subset(minta$test, minta$test$nem==0)

pivot_szolgho[(2*categories+1):(3*categories),3:4] <- as.matrix(aggregate(minta$train$pred_diff_B_RF_ordered, 
                                                                       list(minta$train$szolgho_csop), mean))
pivot_szolgho[(2*categories+1):(3*categories),5] <- as.matrix(table(minta$train$szolgho_csop))

pivot_szolgho[(2*categories+1):(3*categories),6] <- as.matrix(quantile(minta$train$szolgho, percent))

pivot_szolgho[(categories*5+1):(categories*5+categories),3:4] <- as.matrix(aggregate(minta$test$pred_diff_B_RF_ordered, 
                                                                                  list(minta$test$szolgho_csop), mean))
pivot_szolgho[(categories*5+1):(categories*5+categories),5] <- as.matrix(table(minta$test$szolgho_csop))

pivot_szolgho[(categories*5+1):(categories*5+categories),6] <- as.matrix(quantile(minta$test$szolgho, percent))



pivot_szolgho[, 4] <- as.numeric(pivot_szolgho[, 4])

pivot_szolgho <- as.data.frame(pivot_szolgho)

names(pivot_szolgho) <-c("year", "dataset", "category", "RF", "no of obs.", "quantiles")

write.csv(pivot_szolgho, "./results/ws effects/pivot_szolgho.csv")

