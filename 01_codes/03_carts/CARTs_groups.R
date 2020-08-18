library(DescTools)
library(tidyverse)
library(modeest)

# L group
# Sector: Manufacturing, Region: transdanubian, 
# property: foreign majority (1,2),
# education: primary and lower secondary (1-2)

# S group:
# Region: central, Sector: not-manufacturing, firm size: 1-2
# education: higher secondary and tertiary (3-4), 
# no foreign property (4)
# Size 1,2

source("./01_codes/03_carts/get_L_and_S_groups.R")
source("./01_codes/03_carts/get_characteristics.R")

total_matrix <- matrix(NA, 16, 11)
total_matrix[,1] <- c("WS effect", "n",
               "Age", "Tenure", "Education", "Occupation", "Foreign", "State",
               "Firm size", "Settlement", "Region", "Industry", "Collective agr",
               "Coll agr in industry", "Coll agr some firms", "Any collective agr")

S_group_matrix <- matrix(NA, 16, 11)
S_group_matrix[,1] <- c("WS effect", "n",
               "Age", "Tenure", "Education", "Occupation", "Foreign", "State",
               "Firm size", "Settlement", "Region", "Industry", "Collective agr",
               "Coll agr in industry", "Coll agr some firms", "Any collective agr")

S_group_matrix <- matrix(NA, 16, 11)
S_group_matrix[,1] <- c("WS effect", "n",
               "Age", "Tenure", "Education", "Occupation", "Foreign", "State",
               "Firm size", "Settlement", "Region", "Industry", "Collective agr",
               "Coll agr in industry", "Coll agr some firms", "Any collective agr")


minta <- readRDS("./data/minta_2008.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_S <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 1:2] <- pivot_total[, 1:2]
S_group_matrix[, 1:2] <- pivot_S[, 1:2]
S_group_matrix[, 1:2] <- pivot_S[, 1:2]


minta <- readRDS("./data/minta_2009.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 3] <- pivot_total[, 2]
L_group_matrix[, 3] <- pivot_L[, 2]
S_group_matrix[, 3] <- pivot_S[, 2]

minta <- readRDS("./data/minta_2010.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 4] <- pivot_total[, 2]
L_group_matrix[, 4] <- pivot_L[, 2]
S_group_matrix[, 4] <- pivot_S[, 2]

minta <- readRDS("./data/minta_2011.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 5] <- pivot_total[, 2]
L_group_matrix[, 5] <- pivot_L[, 2]
S_group_matrix[, 5] <- pivot_S[, 2]

minta <- readRDS("./data/minta_2012.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 6] <- pivot_total[, 2]
L_group_matrix[, 6] <- pivot_L[, 2]
S_group_matrix[, 6] <- pivot_S[, 2]

minta <- readRDS("./data/minta_2013.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 7] <- pivot_total[, 2]
L_group_matrix[, 7] <- pivot_L[, 2]
S_group_matrix[, 7] <- pivot_S[, 2]


minta <- readRDS("./data/minta_2014.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 8] <- pivot_total[, 2]
L_group_matrix[, 8] <- pivot_L[, 2]
S_group_matrix[, 8] <- pivot_S[, 2]

minta <- readRDS("./data/minta_2015.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 9] <- pivot_total[, 2]
L_group_matrix[, 9] <- pivot_L[, 2]
S_group_matrix[, 9] <- pivot_S[, 2]

minta <- readRDS("./data/minta_2016.RData")

only_women <- subset(minta$train, minta$train$nem == "0")
pivot_total <- get_characteristics(only_women)

groups <- get_L_and_S_groups(only_women)
pivot_L <- get_characteristics(groups$L_group)
pivot_S <- get_characteristics(groups$S_group)

total_matrix[, 10:11] <- pivot_total[, 2:3]
L_group_matrix[, 10:11] <- pivot_L[, 2:3]
S_group_matrix[, 10:11] <- pivot_S[, 2:3]

total_matrix <- as.data.frame(total_matrix)
L_group_matrix <- as.data.frame(L_group_matrix)
S_group_matrix <- as.data.frame(S_group_matrix)

names(total_matrix) <- c("variables", seq(2008,2016), "aggregate")
names(L_group_matrix) <- c("variables", seq(2008,2016), "aggregate")
names(S_group_matrix) <- c("variables", seq(2008,2016), "aggregate")

write.csv(total_matrix, "./results/CARTs/total_matrix.csv")
write.csv(L_group_matrix, "./results/CARTs/L_group_matrix.csv")
write.csv(S_group_matrix, "./results/CARTs/S_group_matrix.csv")