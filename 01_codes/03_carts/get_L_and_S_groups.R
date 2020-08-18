get_L_and_S_groups <- function(df) {
  # L group
  # Sector: Manufacturing, Region: transdanubian, 
  # property: foreign majority (1,2),
  # education: primary and lower secondary (1-2)
  
  # S group:
  # Region: central, Sector: not-manufacturing, firm size: 1-2
  # education: higher secondary and tertiary (3-4), 
  # no foreign property (4)
  # Size 1,2
  
  level_data_H1 <- subset(df, df$nem =="0" & 
                            df$exp_kib_class == 2 &
                            df$kshreg_3 == 2 &
                            (df$kra_ordered == 1 | df$kra_ordered ==2) &
                            (df$iskveg4_ordered == 1 | df$iskveg4_ordered ==2)
  )
  
  level_data_L3 <- subset(df, df$nem == "0" & 
                            df$exp_kib_class != 2 &
                            df$kshreg_3 == 1 &
                            df$kra_ordered == 4 &
                            (df$vallmeret == 1 | df$vallmeret==2) &
                            (df$iskveg4_ordered == 3 | df$iskveg4_ordered ==4)
  )
  
  list(L_group=level_data_H1, S_group=level_data_L3)
  
}
