get_characteristics <- function(level_data) {
  
  pivot <- matrix(" ",16,3)
  pivot[,1] <- c("WS effect", "n",
                 "Age", "Tenure", "Education", "Occupation", "Foreign", "State",
                 "Firm size", "Settlement", "Region", "Industry", "Collective agr",
                 "Coll agr in industry", "Coll agr some firms", "Any collective agr")
  
  pivot[1, 2] <- round((mean(level_data$pred_diff_B_RF_ordered)),4)
  pivot[2, 2] <- nrow(level_data)
  
  pivot[3, 2] <- round(mean(level_data$kor),2)
  pivot[3, 3] <- "avg"
  
  pivot[4, 2] <- round(mean(level_data$szolgho),2)
  pivot[4, 3] <- "avg"
  
  pivot[5, 2] <- round(mean(as.numeric(level_data$iskveg9)),2)
  pivot[5, 3] <- "avg"
  
  pivot[6, 2] <- mfv(as.numeric(level_data$feor_2))
  pivot[6, 3] <- "mode"
  
  pivot[7, 2] <- round(mean(as.numeric(level_data$kra_ordered)),2)
  pivot[7, 3] <- "avg"
  
  pivot[8, 2] <- round(mean(as.numeric(level_data$ara_ordered)),2)
  pivot[8, 3] <- "avg"
  
  pivot[9, 2] <- round(mean(as.numeric(level_data$letszam_bv1)),2)
  pivot[9, 3] <- "avg"
  
  pivot[10, 2] <- mfv(level_data$ttip)
  pivot[10, 3] <- "mode"
  
  pivot[11, 2] <- mfv(level_data$kshreg)
  pivot[11, 3] <- "mode"
  
  pivot[12, 2] <- mfv(level_data$ag1)
  pivot[12, 3] <- "mode"
  
  pivot[13, 2] <- round(mean(as.numeric(level_data$kol)-1),2)
  pivot[13, 3] <- "avg"
  
  pivot[14, 2] <- round(mean(as.numeric(level_data$kag)-1),2)
  pivot[14, 3] <- "avg"
  
  pivot[15, 2] <- round(mean(as.numeric(level_data$ksz)-1),2)
  pivot[15, 3] <- "avg"
  
  pivot[16, 2] <- round(mean(as.numeric(level_data$kollszerz)-1),2)
  pivot[16, 3] <- "avg"
  
  pivot
}