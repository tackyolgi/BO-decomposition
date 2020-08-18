# mse at ols and rf
library(oaxaca)

mse <- matrix("NA", 8*9, 6)

# 2008
kezdo <- 1

minta <- readRDS("./data/minta_2008.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2008.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
                       nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
                         nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
                       nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
                         nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"

# 2009
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2009.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2009.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"

# 2010
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2010.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2010.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"

# 2011
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2011.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2011.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"


# 2012
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2012.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2012.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"


# 2013
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2013.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2013.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"

# 2014
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2014.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2014.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"


# 2015
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2015.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2015.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"


# 2016
kezdo <- kezdo+4

minta <- readRDS("./data/minta_2016.RData")
B_regr_ordered <- readRDS("./models/B_regr_ordered_2016.RData")

# predictions of ols for male with male model and for female with female model
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==0))
minta$train$pred_B_regr_subgr_ordered_male[minta$train$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$train, minta$train$nem==1))

minta$test$pred_B_regr_subgr_ordered[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.A, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==0] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==0))
minta$test$pred_B_regr_subgr_ordered_male[minta$test$nem==1] <- predict(B_regr_ordered$reg$reg.B, subset(minta$test, minta$test$nem==1))

# diff at ols
minta$train$pred_diff_B_regr_ordered[minta$train$nem==1] <- subset(minta$train$pred_B_regr_subgr_ordered- 
                                                                     minta$train$pred_B_regr_subgr_ordered_male,
                                                                   minta$train$nem==1)
minta$train$pred_diff_B_regr_ordered[minta$train$nem==0] <- subset(minta$train$pred_B_regr_subgr_ordered_male
                                                                   -minta$train$pred_B_regr_subgr_ordered,
                                                                   minta$train$nem==0)

minta$test$pred_diff_B_regr_ordered[minta$test$nem==1] <- subset(minta$test$pred_B_regr_subgr_ordered
                                                                 - minta$test$pred_B_regr_subgr_ordered_male,
                                                                 minta$test$nem==1)
minta$test$pred_diff_B_regr_ordered[minta$test$nem==0] <- subset(minta$test$pred_B_regr_subgr_ordered_male
                                                                 -minta$test$pred_B_regr_subgr_ordered,
                                                                 minta$test$nem==0)



# males
train_male <- subset(minta$train, minta$train$nem==1)
test_male <- subset(minta$test, minta$test$nem==1)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"


mse[kezdo:(kezdo+3), 1] <- mean(train_male$ev)
mse[kezdo, 4] <- sum((train_male$pred_B_RF_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+1, 4] <- sum((test_male$pred_B_RF_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo+2, 4] <- sum((train_male$pred_B_regr_subgr_ordered-train_male$lnker)^2)/
  nrow(train_male)
mse[kezdo+3, 4] <- sum((test_male$pred_B_regr_subgr_ordered-test_male$lnker)^2)/
  nrow(test_male)

mse[kezdo, 5] <- nrow(train_male)
mse[kezdo+1, 5] <- nrow(test_male)

mse[kezdo:(kezdo+3), 6] <- "Males"

# females
kezdo <- 4+kezdo


train_female <- subset(minta$train, minta$train$nem==0)
test_female <- subset(minta$test, minta$test$nem==0)

mse[kezdo, 2] <- "Training sample"
mse[kezdo+1, 2] <- "Test sample"

mse[kezdo+2, 2] <- "Training sample"
mse[kezdo+3, 2] <- "Test sample"

mse[kezdo, 3] <- "RF"
mse[kezdo+1, 3] <- "RF"

mse[kezdo+2, 3] <- "OLS"
mse[kezdo+3, 3] <- "OLS"





mse[kezdo:(kezdo+3), 1] <- mean(train_female$ev)
mse[kezdo, 4] <- sum((train_female$pred_B_RF_subgr_ordered-train_female$lnker)^2/
                       nrow(train_female))
mse[kezdo+1, 4] <- sum((test_female$pred_B_RF_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo+2, 4] <- sum((train_female$pred_B_regr_subgr_ordered-train_female$lnker)^2/
                         nrow(train_female))
mse[kezdo+3, 4] <- sum((test_female$pred_B_regr_subgr_ordered-test_female$lnker)^2/
                         nrow(test_female))

mse[kezdo, 5] <- nrow(train_female)
mse[kezdo+1, 5] <- nrow(test_female)

mse[kezdo:(kezdo+3), 6] <- "Females"


mse <- as.data.frame(mse)
names(mse) <- c("Year", "Database", "Method", "MSE", "No of obs",
                "Gender")

saveRDS(mse, "./results/mse.RData")
write.csv(mse, "./results/mse.csv")