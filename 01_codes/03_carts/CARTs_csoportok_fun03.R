kimutatas <- function(level_data) {
  
  pivot <- matrix(" ",18,3)
  pivot[,1] <- c("pred_diff_B_RF_ordered", "n",
                 "kor", "szolgho", "iskveg", "feor_2", "feor_2", "ara", "kra",
                 "letszam", "ttip", "kshregio", "ag1", "kol", "kag", "ksz", "ev")
  
  pivot[1, 2] <- (mean(level_data$pred_diff_B_RF_ordered))
  pivot[2, 2] <- nrow(level_data)
  
  pivot[3, 2] <- mean(level_data$kor)
  pivot[3, 3] <- "avg"
  
  pivot[4, 2] <- mean(level_data$szolgho)
  pivot[4, 3] <- "avg"
  
  pivot[5, 2] <- mean(as.numeric(level_data$iskveg9))
  pivot[5, 3] <- "avg"
  
  pivot[6, 2] <- mean(as.numeric(level_data$feor_2))
  pivot[6, 3] <- "avg"
  
  pivot[7, 2] <- mfv(as.numeric(level_data$feor_2))
  pivot[7, 3] <- "mode"
  
  pivot[8, 2] <- mean(as.numeric(level_data$ara_ordered))
  pivot[8, 3] <- "avg"
  
  pivot[9, 2] <- mean(as.numeric(level_data$kra_ordered))
  pivot[9, 3] <- "avg"
  
  pivot[10, 2] <- mean(as.numeric(level_data$letszam_bv1))
  pivot[10, 3] <- "avg"
  
  pivot[11, 2] <- mfv(level_data$ttip)
  pivot[11, 3] <- "mode"
  
  pivot[12, 2] <- mfv(level_data$kshreg)
  pivot[12, 3] <- "mode"
  
  pivot[13, 2] <- mfv(level_data$ag1)
  pivot[13, 3] <- "mode"
  
  pivot[14, 2] <- mean(as.numeric(level_data$kol)-1)
  pivot[14, 3] <- "avg"
  
  pivot[15, 2] <- mean(as.numeric(level_data$kag)-1)
  pivot[15, 3] <- "avg"
  
  pivot[16, 2] <- mean(as.numeric(level_data$ksz)-1)
  pivot[16, 3] <- "avg"
  
  pivot[17, 2] <- mean(as.numeric(level_data$ev))
  
  pivot
}