TOD_optimization <- function(dataframe_avg,max_periods,min_dur,max_dur,percentile,cycle_length_rounded){
library(dplyr)
df <- dataframe_avg
df$time <- as.integer(df$time)
df$time <- df$time/60
maximum_periods <- max_periods-1
min_time_period <- min_dur
max_time_period <- max_dur
period1_hr_range <- c(5,6,7,8,9)
period1_mts_range <- c(0,1)
period_2_hrs <- seq(min_time_period, max_time_period, by=0.5)
period_3_hrs <- seq(min_time_period, max_time_period, by=0.5)
period_4_hrs <- seq(min_time_period, max_time_period, by=0.5)
period_5_hrs <- seq(min_time_period, max_time_period, by=0.5)
period_6_hrs <- seq(min_time_period, max_time_period, by=0.5)
period_7_hrs <- seq(min_time_period, max_time_period, by=0.5)
period_8_hrs <- seq(min_time_period, max_time_period, by=0.5)
period_9_hrs <- seq(min_time_period, max_time_period, by=0.5)
df_error_3_periods <- NULL
df_proposed_cycle <- NULL
df_subset_period <- NULL
if(maximum_periods == 3){
df_error_3_periods <- data.frame(matrix(ncol = maximum_periods*4+4, nrow = 0))
colnames(df_error_3_periods) <- c("period1_start","period1_end", "period1_error","period1_proposed","period2_start","period2_end", "period2_error","period2_proposed","period3_start","period3_end", "period3_error","period3_proposed","periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
for (hr in period1_hr_range){
  for (mts in period1_mts_range){
period_1_lower <- 1
period_1_upper <- 60*hr+mts*30
df_subset_period_1 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_1_lower & df$time < period_1_upper])
colnames(df_subset_period_1) <- c("Nominal_Cycle_Length")
proposed_cycle_period_1 <- as.integer(quantile(df_subset_period_1$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
df_proposed_cycle_1 <- as.data.frame(rep(proposed_cycle_period_1,times = nrow(df_subset_period_1)))
colnames(df_proposed_cycle_1) <- c("proposed")
df_subset_period_1$diff <- abs(df_subset_period_1$Nominal_Cycle_Length - df_proposed_cycle_1$proposed)
diff_avg_period1 <- mean(df_subset_period_1$diff)
df_error_tmp_period1 <- data.frame(period_1_lower,period_1_upper,diff_avg_period1,proposed_cycle_period_1)
colnames(df_error_tmp_period1) <- c("period1_start","period1_end", "period1_error","period1_proposed")
for (period2 in period_2_hrs){
  period_2_lower <- period_1_upper
  period_2_upper <- period_2_lower+period2*60
  if(period_2_upper <= 1440){
    df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
    colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
    proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
    df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
    colnames(df_proposed_cycle_2) <- c("proposed")
    df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
    diff_avg_period2 <- mean(df_subset_period_2$diff)
    df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2)
    colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed")
    for (period3 in period_3_hrs){
      period_3_lower <- period_2_upper
      period_3_upper <- period_3_lower+period3*60
      if(period_3_upper == 1440){
        df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
        colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
        proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
        df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
        colnames(df_proposed_cycle_3) <- c("proposed")
        df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
        diff_avg_period3 <- mean(df_subset_period_3$diff)
        df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
        colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
        df_error_tmp_periodfinal <- data.frame(999,999,999,999)
        colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
        df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
        df_error_3_periods <- rbind(df_error_3_periods, df_error_tmp)
       } else if(period_3_upper < 1440){
          period_final_lower <- period_3_upper
          period_final_upper <- 1440
          df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
          colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
          colnames(df_proposed_cycle_3) <- c("proposed")
          df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
          diff_avg_period3 <- mean(df_subset_period_3$diff)
          df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
          colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
          df_subset_period_final <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_final_lower & df$time < period_final_upper])
          colnames(df_subset_period_final) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_final <- as.integer(mean(df_subset_period_final$Nominal_Cycle_Length)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_final <- as.data.frame(rep(proposed_cycle_period_final,times = nrow(df_subset_period_final)))
          colnames(df_proposed_cycle_final) <- c("proposed")
          df_subset_period_final$diff <- abs(df_subset_period_final$Nominal_Cycle_Length - df_proposed_cycle_final$proposed)
          diff_avg_periodfinal <- mean(df_subset_period_final$diff)
          df_error_tmp_periodfinal <- data.frame(period_final_lower,period_final_upper,diff_avg_periodfinal,proposed_cycle_period_final)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
          df_error_3_periods <- rbind(df_error_3_periods, df_error_tmp)
      } else {
        period_3_upper <- 1440
        df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
        colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
        proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
        df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
        colnames(df_proposed_cycle_3) <- c("proposed")
        df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
        diff_avg_period3 <- mean(df_subset_period_3$diff)
        df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
        colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
        df_error_tmp_periodfinal <- data.frame(999,999,999,999)
        colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
        df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
        df_error_3_periods <- rbind(df_error_3_periods, df_error_tmp)
      }
    }
  } else{
    period_2_upper <- 1440
    df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
    colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
    proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
    df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
    colnames(df_proposed_cycle_2) <- c("proposed")
    df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
    diff_avg_period2 <- mean(df_subset_period_2$diff)
    df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2,999,999,999,999)
    colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed","period3_start","period3_end", "period3_error","period3_proposed")
    df_error_tmp_periodfinal <- data.frame(999,999,999,999)
    colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
    df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_periodfinal)
    df_error_3_periods <- rbind(df_error_3_periods, df_error_tmp)
  }
}
}
}
df_error_3_periods$overall_error <- 
((df_error_3_periods$period1_end-df_error_3_periods$period1_start)*df_error_3_periods$period1_error+
(df_error_3_periods$period2_end-df_error_3_periods$period2_start)*df_error_3_periods$period2_error+
(df_error_3_periods$period3_end-df_error_3_periods$period3_start)*df_error_3_periods$period3_error+
(df_error_3_periods$periodfinal_end-df_error_3_periods$periodfinal_start)*df_error_3_periods$periodfinal_error)/1440
df_error_3_periods_final <- df_error_3_periods[!is.na(df_error_3_periods$overall_error),]
df_error_3_periods_final[df_error_3_periods_final==999] <- 0
df_error_3_periods_final <- df_error_3_periods[df_error_3_periods_final$overall_error == min(df_error_3_periods_final$overall_error),]
df_error_3_periods_final <- df_error_3_periods_final[1,]
df_error_3_periods_final2 <- as.data.frame(t(as.matrix(df_error_3_periods_final)))
df_error_3_periods_final2$rn <- row.names(df_error_3_periods_final2)
colnames(df_error_3_periods_final2) <- c("value","rn")
df_final <- sqldf::sqldf("select a.*
                           ,b.period1_proposed as proposed1
                           ,c.period2_proposed as proposed2
                           ,d.period3_proposed as proposed3
                           ,h.periodfinal_proposed as proposedfinal
                           from df as a
                           left join df_error_3_periods_final as b
                           on a.time = b.period1_start
                           left join df_error_3_periods_final as c
                           on a.time = c.period2_start
                           left join df_error_3_periods_final as d
                           on a.time = d.period3_start
                           left join df_error_3_periods_final as h
                           on a.time = h.periodfinal_start", drv = "SQLite")
df_final$proposed <-  rowSums(df_final[,c("proposed1", "proposed2"
                                          ,"proposed3",
                                          "proposedfinal")], na.rm=TRUE)
df_final$proposed1 <- NULL
df_final$proposed2 <- NULL
df_final$proposed3 <- NULL
df_final$proposedfinal <- NULL
df_final$proposed[df_final$proposed == 0] <- NA
df_final <- df_final %>% tidyr::fill(proposed, .direction = c("down"))
}
if(maximum_periods == 4){
  df_error_4_periods <- data.frame(matrix(ncol = maximum_periods*4+4, nrow = 0))
  colnames(df_error_4_periods) <- c("period1_start","period1_end", "period1_error","period1_proposed",
                                    "period2_start","period2_end", "period2_error","period2_proposed",
                                    "period3_start","period3_end", "period3_error","period3_proposed",
                                    "period4_start","period4_end", "period4_error","period4_proposed",
                                    "periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
  for (hr in period1_hr_range){
    for (mts in period1_mts_range){
      period_1_lower <- 1
      period_1_upper <- 60*hr+mts*30
      df_subset_period_1 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_1_lower & df$time < period_1_upper])
      colnames(df_subset_period_1) <- c("Nominal_Cycle_Length")
      proposed_cycle_period_1 <- as.integer(quantile(df_subset_period_1$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
      df_proposed_cycle_1 <- as.data.frame(rep(proposed_cycle_period_1,times = nrow(df_subset_period_1)))
      colnames(df_proposed_cycle_1) <- c("proposed")
      df_subset_period_1$diff <- abs(df_subset_period_1$Nominal_Cycle_Length - df_proposed_cycle_1$proposed)
      diff_avg_period1 <- mean(df_subset_period_1$diff)
      df_error_tmp_period1 <- data.frame(period_1_lower,period_1_upper,diff_avg_period1,proposed_cycle_period_1)
      colnames(df_error_tmp_period1) <- c("period1_start","period1_end", "period1_error","period1_proposed")
      for (period2 in period_2_hrs){
        period_2_lower <- period_1_upper
        period_2_upper <- period_2_lower+period2*60
        if(period_2_upper <= 1440){
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed")
          for (period3 in period_3_hrs){
            period_3_lower <- period_2_upper
            period_3_upper <- period_3_lower+period3*60
            if(period_3_upper <= 1440){
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
          for (period4 in period_4_hrs){
            period_4_lower <- period_3_upper
            period_4_upper <- period_4_lower+period4*60
            if(period_4_upper == 1440){
              df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
              colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
              colnames(df_proposed_cycle_4) <- c("proposed")
              df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
              diff_avg_period4 <- mean(df_subset_period_4$diff)
              df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
              colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
              df_error_4_periods <- rbind(df_error_4_periods, df_error_tmp)
            } else if(period_4_upper < 1440){
              period_final_lower <- period_4_upper
              period_final_upper <- 1440
              df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
              colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
              colnames(df_proposed_cycle_4) <- c("proposed")
              df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
              diff_avg_period4 <- mean(df_subset_period_4$diff)
              df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
              colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
              df_subset_period_final <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_final_lower & df$time < period_final_upper])
              colnames(df_subset_period_final) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_final <- as.integer(mean(df_subset_period_final$Nominal_Cycle_Length)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_final <- as.data.frame(rep(proposed_cycle_period_final,times = nrow(df_subset_period_final)))
              colnames(df_proposed_cycle_final) <- c("proposed")
              df_subset_period_final$diff <- abs(df_subset_period_final$Nominal_Cycle_Length - df_proposed_cycle_final$proposed)
              diff_avg_periodfinal <- mean(df_subset_period_final$diff)
              df_error_tmp_periodfinal <- data.frame(period_final_lower,period_final_upper,diff_avg_periodfinal,proposed_cycle_period_final)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
              df_error_4_periods <- rbind(df_error_4_periods, df_error_tmp)
            } else {
              period_4_upper <- 1440
              df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
              colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
              colnames(df_proposed_cycle_4) <- c("proposed")
              df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
              diff_avg_period4 <- mean(df_subset_period_4$diff)
              df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
              colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
              df_error_4_periods <- rbind(df_error_4_periods, df_error_tmp)
            }
          }
             }  else if (period_3_upper > 1440){
          period_3_upper <- 1440
          df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
          colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
          colnames(df_proposed_cycle_3) <- c("proposed")
          df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
          diff_avg_period3 <- mean(df_subset_period_3$diff)
          df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3,999,999,999,999)
          colnames(df_error_tmp_period3) <- c("period3_start","period3_end","period3_error","period3_proposed","period4_start","period4_end", "period4_error","period4_proposed")
          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
          df_error_4_periods <- rbind(df_error_4_periods, df_error_tmp)
             }
      }
        } else if (period_2_upper > 1440){
          period_2_upper <- 1440
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2,999,999,999,999,999,999,999,999)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed",
                                              "period3_start","period3_end", "period3_error","period3_proposed",
                                              "period4_start","period4_end", "period4_error","period4_proposed")
          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_periodfinal)
          df_error_4_periods <- rbind(df_error_4_periods, df_error_tmp)
    }
      }
    }
  }
  df_error_4_periods$overall_error <- 
    ((df_error_4_periods$period1_end-df_error_4_periods$period1_start)*df_error_4_periods$period1_error+
       (df_error_4_periods$period2_end-df_error_4_periods$period2_start)*df_error_4_periods$period2_error+
       (df_error_4_periods$period3_end-df_error_4_periods$period3_start)*df_error_4_periods$period3_error+
       (df_error_4_periods$period4_end-df_error_4_periods$period4_start)*df_error_4_periods$period4_error+
       (df_error_4_periods$periodfinal_end-df_error_4_periods$periodfinal_start)*df_error_4_periods$periodfinal_error)/1440
  df_error_4_periods_final <- df_error_4_periods[!is.na(df_error_4_periods$overall_error),]
  df_error_4_periods_final[df_error_4_periods_final==999] <- 0
  df_error_4_periods_final <- df_error_4_periods[df_error_4_periods_final$overall_error == min(df_error_4_periods_final$overall_error),]
  df_error_4_periods_final <- df_error_4_periods_final[1,]
  df_error_4_periods_final2 <- as.data.frame(t(as.matrix(df_error_4_periods_final)))
  df_error_4_periods_final2$rn <- row.names(df_error_4_periods_final2)
  colnames(df_error_4_periods_final2) <- c("value","rn")
  df_final <- sqldf::sqldf("select a.*
                           ,b.period1_proposed as proposed1
                           ,c.period2_proposed as proposed2
                           ,d.period3_proposed as proposed3
                           ,e.period4_proposed as proposed4
                           ,f.periodfinal_proposed as proposedfinal
                           from df as a
                           left join df_error_4_periods_final as b
                           on a.time = b.period1_start
                           left join df_error_4_periods_final as c
                           on a.time = c.period2_start
                           left join df_error_4_periods_final as d
                           on a.time = d.period3_start
                           left join df_error_4_periods_final as e
                           on a.time = e.period4_start
                           left join df_error_4_periods_final as f
                           on a.time = f.periodfinal_start", drv = "SQLite")
  df_final$proposed <-  rowSums(df_final[,c("proposed1", "proposed2"
                                            ,"proposed3", "proposed4"
                                            ,"proposedfinal")], na.rm=TRUE)
  df_final$proposed1 <- NULL
  df_final$proposed2 <- NULL
  df_final$proposed3 <- NULL
  df_final$proposed4 <- NULL
  df_final$proposedfinal <- NULL
  df_final$proposed[df_final$proposed == 0] <- NA
  df_final <- df_final %>% tidyr::fill(proposed, .direction = c("down"))
}
if(maximum_periods == 5){
  df_error_5_periods <- data.frame(matrix(ncol = maximum_periods*4+4, nrow = 0))
  colnames(df_error_5_periods) <- c("period1_start","period1_end", "period1_error","period1_proposed",
                                    "period2_start","period2_end", "period2_error","period2_proposed",
                                    "period3_start","period3_end", "period3_error","period3_proposed",
                                    "period4_start","period4_end", "period4_error","period4_proposed",
                                    "period5_start","period5_end", "period5_error","period5_proposed",
                                    "periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
  for (hr in period1_hr_range){
    for (mts in period1_mts_range){
      period_1_lower <- 1
      period_1_upper <- 60*hr+mts*30
      df_subset_period_1 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_1_lower & df$time < period_1_upper])
      colnames(df_subset_period_1) <- c("Nominal_Cycle_Length")
      proposed_cycle_period_1 <- as.integer(quantile(df_subset_period_1$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
      df_proposed_cycle_1 <- as.data.frame(rep(proposed_cycle_period_1,times = nrow(df_subset_period_1)))
      colnames(df_proposed_cycle_1) <- c("proposed")
      df_subset_period_1$diff <- abs(df_subset_period_1$Nominal_Cycle_Length - df_proposed_cycle_1$proposed)
      diff_avg_period1 <- mean(df_subset_period_1$diff)
      df_error_tmp_period1 <- data.frame(period_1_lower,period_1_upper,diff_avg_period1,proposed_cycle_period_1)
      colnames(df_error_tmp_period1) <- c("period1_start","period1_end", "period1_error","period1_proposed")
      for (period2 in period_2_hrs){
        period_2_lower <- period_1_upper
        period_2_upper <- period_2_lower+period2*60
        if(period_2_upper <= 1440){
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed")
          for (period3 in period_3_hrs){
            period_3_lower <- period_2_upper
            period_3_upper <- period_3_lower+period3*60
            if(period_3_upper <= 1440){
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
              for (period4 in period_4_hrs){
                period_4_lower <- period_3_upper
                period_4_upper <- period_4_lower+period4*60
                if(period_4_upper <= 1440){
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
              for (period5 in period_5_hrs){
                period_5_lower <- period_4_upper
                period_5_upper <- period_5_lower+period5*60
                if(period_5_upper == 1440){
                  df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                  colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                  colnames(df_proposed_cycle_5) <- c("proposed")
                  df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                  diff_avg_period5 <- mean(df_subset_period_5$diff)
                  df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5)
                  colnames(df_error_tmp_period5) <- c("period5_start","period5_end", "period5_error","period5_proposed")
                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_periodfinal)
                  df_error_5_periods <- rbind(df_error_5_periods, df_error_tmp)
                } else if(period_5_upper < 1440){
                  period_final_lower <- period_5_upper
                  period_final_upper <- 1440
                  df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                  colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                  colnames(df_proposed_cycle_5) <- c("proposed")
                  df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                  diff_avg_period5 <- mean(df_subset_period_5$diff)
                  df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5)
                  colnames(df_error_tmp_period5) <- c("period5_start","period5_end", "period5_error","period5_proposed")
                  df_subset_period_final <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_final_lower & df$time < period_final_upper])
                  colnames(df_subset_period_final) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_final <- as.integer(mean(df_subset_period_final$Nominal_Cycle_Length)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_final <- as.data.frame(rep(proposed_cycle_period_final,times = nrow(df_subset_period_final)))
                  colnames(df_proposed_cycle_final) <- c("proposed")
                  df_subset_period_final$diff <- abs(df_subset_period_final$Nominal_Cycle_Length - df_proposed_cycle_final$proposed)
                  diff_avg_periodfinal <- mean(df_subset_period_final$diff)
                  df_error_tmp_periodfinal <- data.frame(period_final_lower,period_final_upper,diff_avg_periodfinal,proposed_cycle_period_final)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_periodfinal)
                  df_error_5_periods <- rbind(df_error_5_periods, df_error_tmp)
                } else {
                  period_5_upper <- 1440
                  df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                  colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                  colnames(df_proposed_cycle_5) <- c("proposed")
                  df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                  diff_avg_period5 <- mean(df_subset_period_5$diff)
                  df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5)
                  colnames(df_error_tmp_period5) <- c("period5_start","period5_end", "period5_error","period5_proposed")
                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_periodfinal)
                  df_error_5_periods <- rbind(df_error_5_periods, df_error_tmp)
                }
              }
                }  else if (period_4_upper > 1440){
                  period_4_upper <- 1440
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4,999,999,999,999)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end","period4_error","period4_proposed","period5_start","period5_end", "period5_error","period5_proposed")
                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
                  df_error_5_periods <- rbind(df_error_5_periods, df_error_tmp)
                }
              }
            }  else if (period_3_upper > 1440){
              period_3_upper <- 1440
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3,999,999,999,999,999,999,999,999)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end","period3_error","period3_proposed",
                                                 "period4_start","period4_end", "period4_error","period4_proposed",
                                                 "period5_start","period5_end", "period5_error","period5_proposed")
              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
              df_error_5_periods <- rbind(df_error_5_periods, df_error_tmp)
            }
          }
        } else if (period_2_upper > 1440){
          period_2_upper <- 1440
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2,999,999,999,999,999,999,999,999,999,999,999,999)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed",
                                              "period3_start","period3_end", "period3_error","period3_proposed",
                                              "period4_start","period4_end", "period4_error","period4_proposed",
                                              "period5_start","period5_end", "period5_error","period5_proposed")
          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_periodfinal)
          df_error_5_periods <- rbind(df_error_5_periods, df_error_tmp)
        }
      }
    }
  }
  df_error_5_periods$overall_error <- 
    ((df_error_5_periods$period1_end-df_error_5_periods$period1_start)*df_error_5_periods$period1_error+
       (df_error_5_periods$period2_end-df_error_5_periods$period2_start)*df_error_5_periods$period2_error+
       (df_error_5_periods$period3_end-df_error_5_periods$period3_start)*df_error_5_periods$period3_error+
       (df_error_5_periods$period4_end-df_error_5_periods$period4_start)*df_error_5_periods$period4_error+
       (df_error_5_periods$period5_end-df_error_5_periods$period5_start)*df_error_5_periods$period5_error+
       (df_error_5_periods$periodfinal_end-df_error_5_periods$periodfinal_start)*df_error_5_periods$periodfinal_error)/1440
  df_error_5_periods_final <- df_error_5_periods[!is.na(df_error_5_periods$overall_error),]
  df_error_5_periods_final[df_error_5_periods_final==999] <- 0
  df_error_5_periods_final <- df_error_5_periods[df_error_5_periods_final$overall_error == min(df_error_5_periods_final$overall_error),]
  df_error_5_periods_final <- df_error_5_periods_final[1,]
  df_error_5_periods_final2 <- as.data.frame(t(as.matrix(df_error_5_periods_final)))
  df_error_5_periods_final2$rn <- row.names(df_error_5_periods_final2)
  colnames(df_error_5_periods_final2) <- c("value","rn")
  df_final <- sqldf::sqldf("select a.*
                           ,b.period1_proposed as proposed1
                           ,c.period2_proposed as proposed2
                           ,d.period3_proposed as proposed3
                           ,e.period4_proposed as proposed4
                           ,f.period5_proposed as proposed5
                           ,h.periodfinal_proposed as proposedfinal
                           from df as a
                           left join df_error_5_periods_final as b
                           on a.time = b.period1_start
                           left join df_error_5_periods_final as c
                           on a.time = c.period2_start
                           left join df_error_5_periods_final as d
                           on a.time = d.period3_start
                           left join df_error_5_periods_final as e
                           on a.time = e.period4_start
                           left join df_error_5_periods_final as f
                           on a.time = f.period5_start
                           left join df_error_5_periods_final as h
                           on a.time = h.periodfinal_start", drv = "SQLite")
  df_final$proposed <-  rowSums(df_final[,c("proposed1", "proposed2"
                                            ,"proposed3", "proposed4"
                                            ,"proposed5",
                                            "proposedfinal")], na.rm=TRUE)
  df_final$proposed1 <- NULL
  df_final$proposed2 <- NULL
  df_final$proposed3 <- NULL
  df_final$proposed4 <- NULL
  df_final$proposed5 <- NULL
  df_final$proposedfinal <- NULL
  df_final$proposed[df_final$proposed == 0] <- NA
  df_final <- df_final %>% tidyr::fill(proposed, .direction = c("down"))
}
if(maximum_periods == 6){
  df_error_6_periods <- data.frame(matrix(ncol = maximum_periods*4+4, nrow = 0))
  colnames(df_error_6_periods) <- c("period1_start","period1_end", "period1_error","period1_proposed",
                                    "period2_start","period2_end", "period2_error","period2_proposed",
                                    "period3_start","period3_end", "period3_error","period3_proposed",
                                    "period4_start","period4_end", "period4_error","period4_proposed",
                                    "period5_start","period5_end", "period5_error","period5_proposed",
                                    "period6_start","period6_end", "period6_error","period6_proposed",
                                    "periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
  for (hr in period1_hr_range){
    for (mts in period1_mts_range){
      period_1_lower <- 1
      period_1_upper <- 60*hr+mts*30
      df_subset_period_1 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_1_lower & df$time < period_1_upper])
      colnames(df_subset_period_1) <- c("Nominal_Cycle_Length")
      proposed_cycle_period_1 <- as.integer(quantile(df_subset_period_1$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
      df_proposed_cycle_1 <- as.data.frame(rep(proposed_cycle_period_1,times = nrow(df_subset_period_1)))
      colnames(df_proposed_cycle_1) <- c("proposed")
      df_subset_period_1$diff <- abs(df_subset_period_1$Nominal_Cycle_Length - df_proposed_cycle_1$proposed)
      diff_avg_period1 <- mean(df_subset_period_1$diff)
      df_error_tmp_period1 <- data.frame(period_1_lower,period_1_upper,diff_avg_period1,proposed_cycle_period_1)
      colnames(df_error_tmp_period1) <- c("period1_start","period1_end", "period1_error","period1_proposed")
      for (period2 in period_2_hrs){
        period_2_lower <- period_1_upper
        period_2_upper <- period_2_lower+period2*60
        if(period_2_upper <= 1440){
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed")
          for (period3 in period_3_hrs){
            period_3_lower <- period_2_upper
            period_3_upper <- period_3_lower+period3*60
            if(period_3_upper <= 1440){
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
              for (period4 in period_4_hrs){
                period_4_lower <- period_3_upper
                period_4_upper <- period_4_lower+period4*60
                if(period_4_upper <= 1440){
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
                  for (period5 in period_5_hrs){
                    period_5_lower <- period_4_upper
                    period_5_upper <- period_5_lower+period5*60
                    if(period_5_upper <= 1440){
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end", "period5_error","period5_proposed")
                  for (period6 in period_6_hrs){
                    period_6_lower <- period_5_upper
                    period_6_upper <- period_6_lower+period6*60
                    if(period_6_upper == 1440){
                      df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                      colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                      colnames(df_proposed_cycle_6) <- c("proposed")
                      df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                      diff_avg_period6 <- mean(df_subset_period_6$diff)
                      df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6)
                      colnames(df_error_tmp_period6) <- c("period6_start","period6_end", "period6_error","period6_proposed")
                      df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                      colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                      df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_periodfinal)
                      df_error_6_periods <- rbind(df_error_6_periods, df_error_tmp)
                    } else if(period_6_upper < 1440){
                      period_final_lower <- period_6_upper
                      period_final_upper <- 1440
                      df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                      colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                      colnames(df_proposed_cycle_6) <- c("proposed")
                      df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                      diff_avg_period6 <- mean(df_subset_period_6$diff)
                      df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6)
                      colnames(df_error_tmp_period6) <- c("period6_start","period6_end", "period6_error","period6_proposed")
                      df_subset_period_final <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_final_lower & df$time < period_final_upper])
                      colnames(df_subset_period_final) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_final <- as.integer(mean(df_subset_period_final$Nominal_Cycle_Length)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_final <- as.data.frame(rep(proposed_cycle_period_final,times = nrow(df_subset_period_final)))
                      colnames(df_proposed_cycle_final) <- c("proposed")
                      df_subset_period_final$diff <- abs(df_subset_period_final$Nominal_Cycle_Length - df_proposed_cycle_final$proposed)
                      diff_avg_periodfinal <- mean(df_subset_period_final$diff)
                      df_error_tmp_periodfinal <- data.frame(period_final_lower,period_final_upper,diff_avg_periodfinal,proposed_cycle_period_final)
                      colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                      df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_periodfinal)
                      df_error_6_periods <- rbind(df_error_6_periods, df_error_tmp)
                    } else {
                      period_6_upper <- 1440
                      df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                      colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                      colnames(df_proposed_cycle_6) <- c("proposed")
                      df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                      diff_avg_period6 <- mean(df_subset_period_6$diff)
                      df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6)
                      colnames(df_error_tmp_period6) <- c("period6_start","period6_end", "period6_error","period6_proposed")
                      df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                      colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                      df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_periodfinal)
                      df_error_6_periods <- rbind(df_error_6_periods, df_error_tmp)
                    }
                  }
                    }  else if (period_5_upper > 1440){
                      period_5_upper <- 1440
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5,999,999,999,999)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end","period5_error","period5_proposed",
                                                          "period6_start","period6_end","period6_error","period6_proposed")
                      df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                      colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                      df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_periodfinal)
                      df_error_6_periods <- rbind(df_error_6_periods, df_error_tmp)
                    }
                  }   
                }  else if (period_4_upper > 1440){
                  period_4_upper <- 1440
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4,999,999,999,999,999,999,999,999)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end","period4_error","period4_proposed",
                                                      "period5_start","period5_end","period5_error","period5_proposed",
                                                      "period6_start","period6_end","period6_error","period6_proposed")
                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
                  df_error_6_periods <- rbind(df_error_6_periods, df_error_tmp)
                }
              }
            }  else if (period_3_upper > 1440){
              period_3_upper <- 1440
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3,999,999,999,999,999,999,999,999,999,999,999,999)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end","period3_error","period3_proposed",
                                                  "period4_start","period4_end","period4_error","period4_proposed",
                                                  "period5_start","period5_end","period5_error","period5_proposed",
                                                  "period6_start","period6_end","period6_error","period6_proposed")
              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
              df_error_6_periods <- rbind(df_error_6_periods, df_error_tmp)
            }
          }
        } else if (period_2_upper > 1440){
          period_2_upper <- 1440
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed",
                                              "period3_start","period3_end", "period3_error","period3_proposed",
                                              "period4_start","period4_end", "period4_error","period4_proposed",
                                              "period5_start","period5_end", "period5_error","period5_proposed",
                                              "period6_start","period6_end", "period6_error","period6_proposed")
          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_periodfinal)
          df_error_6_periods <- rbind(df_error_6_periods, df_error_tmp)
        }
      }
    }
  }
  df_error_6_periods$overall_error <- 
    ((df_error_6_periods$period1_end-df_error_6_periods$period1_start)*df_error_6_periods$period1_error+
       (df_error_6_periods$period2_end-df_error_6_periods$period2_start)*df_error_6_periods$period2_error+
       (df_error_6_periods$period3_end-df_error_6_periods$period3_start)*df_error_6_periods$period3_error+
       (df_error_6_periods$period4_end-df_error_6_periods$period4_start)*df_error_6_periods$period4_error+
       (df_error_6_periods$period5_end-df_error_6_periods$period5_start)*df_error_6_periods$period5_error+
       (df_error_6_periods$period6_end-df_error_6_periods$period6_start)*df_error_6_periods$period6_error+
       (df_error_6_periods$periodfinal_end-df_error_6_periods$periodfinal_start)*df_error_6_periods$periodfinal_error)/1440
  df_error_6_periods_final <- df_error_6_periods[!is.na(df_error_6_periods$overall_error),]
  df_error_6_periods_final[df_error_6_periods_final==999] <- 0
  df_error_6_periods_final <- df_error_6_periods[df_error_6_periods_final$overall_error == min(df_error_6_periods_final$overall_error),]
  df_error_6_periods_final <- df_error_6_periods_final[1,]
  df_error_6_periods_final2 <- as.data.frame(t(as.matrix(df_error_6_periods_final)))
  df_error_6_periods_final2$rn <- row.names(df_error_6_periods_final2)
  colnames(df_error_6_periods_final2) <- c("value","rn")
  df_final <- sqldf::sqldf("select a.*
                           ,b.period1_proposed as proposed1
                           ,c.period2_proposed as proposed2
                           ,d.period3_proposed as proposed3
                           ,e.period4_proposed as proposed4
                           ,f.period5_proposed as proposed5
                           ,g.period6_proposed as proposed6
                           ,h.periodfinal_proposed as proposedfinal
                           from df as a
                           left join df_error_6_periods_final as b
                           on a.time = b.period1_start
                           left join df_error_6_periods_final as c
                           on a.time = c.period2_start
                           left join df_error_6_periods_final as d
                           on a.time = d.period3_start
                           left join df_error_6_periods_final as e
                           on a.time = e.period4_start
                           left join df_error_6_periods_final as f
                           on a.time = f.period5_start
                           left join df_error_6_periods_final as g
                           on a.time = g.period6_start
                           left join df_error_6_periods_final as h
                           on a.time = h.periodfinal_start", drv = "SQLite")
  df_final$proposed <-  rowSums(df_final[,c("proposed1", "proposed2"
                                       ,"proposed3", "proposed4"
                                       ,"proposed5", "proposed6"
                                       ,"proposedfinal")], na.rm=TRUE)
    df_final$proposed1 <- NULL
    df_final$proposed2 <- NULL
    df_final$proposed3 <- NULL
    df_final$proposed4 <- NULL
    df_final$proposed5 <- NULL
    df_final$proposed6 <- NULL
    df_final$proposedfinal <- NULL
    df_final$proposed[df_final$proposed == 0] <- NA
    df_final <- df_final %>% tidyr::fill(proposed, .direction = c("down"))
}
if(maximum_periods == 7){
  df_error_7_periods <- data.frame(matrix(ncol = maximum_periods*4+4, nrow = 0))
  colnames(df_error_7_periods) <- c("period1_start","period1_end", "period1_error","period1_proposed",
                                    "period2_start","period2_end", "period2_error","period2_proposed",
                                    "period3_start","period3_end", "period3_error","period3_proposed",
                                    "period4_start","period4_end", "period4_error","period4_proposed",
                                    "period5_start","period5_end", "period5_error","period5_proposed",
                                    "period6_start","period6_end", "period6_error","period6_proposed",
                                    "period7_start","period7_end", "period7_error","period7_proposed",
                                    "periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
  for (hr in period1_hr_range){
    for (mts in period1_mts_range){
      period_1_lower <- 1
      period_1_upper <- 60*hr+mts*30
      df_subset_period_1 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_1_lower & df$time < period_1_upper])
      colnames(df_subset_period_1) <- c("Nominal_Cycle_Length")
      proposed_cycle_period_1 <- as.integer(quantile(df_subset_period_1$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
      df_proposed_cycle_1 <- as.data.frame(rep(proposed_cycle_period_1,times = nrow(df_subset_period_1)))
      colnames(df_proposed_cycle_1) <- c("proposed")
      df_subset_period_1$diff <- abs(df_subset_period_1$Nominal_Cycle_Length - df_proposed_cycle_1$proposed)
      diff_avg_period1 <- mean(df_subset_period_1$diff)
      df_error_tmp_period1 <- data.frame(period_1_lower,period_1_upper,diff_avg_period1,proposed_cycle_period_1)
      colnames(df_error_tmp_period1) <- c("period1_start","period1_end", "period1_error","period1_proposed")
      for (period2 in period_2_hrs){
        period_2_lower <- period_1_upper
        period_2_upper <- period_2_lower+period2*60
        if(period_2_upper <= 1440){
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed")
          for (period3 in period_3_hrs){
            period_3_lower <- period_2_upper
            period_3_upper <- period_3_lower+period3*60
            if(period_3_upper <= 1440){
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
              for (period4 in period_4_hrs){
                period_4_lower <- period_3_upper
                period_4_upper <- period_4_lower+period4*60
                if(period_4_upper <= 1440){
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
                  for (period5 in period_5_hrs){
                    period_5_lower <- period_4_upper
                    period_5_upper <- period_5_lower+period5*60
                    if(period_5_upper <= 1440){
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end", "period5_error","period5_proposed")
                      for (period6 in period_6_hrs){
                        period_6_lower <- period_5_upper
                        period_6_upper <- period_6_lower+period6*60
                        if(period_6_upper <= 1440){
                          df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                          colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                          colnames(df_proposed_cycle_6) <- c("proposed")
                          df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                          diff_avg_period6 <- mean(df_subset_period_6$diff)
                          df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6)
                          colnames(df_error_tmp_period6) <- c("period6_start","period6_end", "period6_error","period6_proposed")
                      for (period7 in period_7_hrs){
                        period_7_lower <- period_6_upper
                        period_7_upper <- period_7_lower+period7*60
                        if(period_7_upper == 1440){
                          df_subset_period_7 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_7_lower & df$time < period_7_upper])
                          colnames(df_subset_period_7) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_7 <- as.integer(quantile(df_subset_period_7$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_7 <- as.data.frame(rep(proposed_cycle_period_7,times = nrow(df_subset_period_7)))
                          colnames(df_proposed_cycle_7) <- c("proposed")
                          df_subset_period_7$diff <- abs(df_subset_period_7$Nominal_Cycle_Length - df_proposed_cycle_7$proposed)
                          diff_avg_period7 <- mean(df_subset_period_7$diff)
                          df_error_tmp_period7 <- data.frame(period_7_lower,period_7_upper,diff_avg_period7,proposed_cycle_period_7)
                          colnames(df_error_tmp_period7) <- c("period7_start","period7_end", "period7_error","period7_proposed")
                          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_periodfinal)
                          df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
                        } else if(period_7_upper < 1440){
                          period_final_lower <- period_7_upper
                          period_final_upper <- 1440
                          df_subset_period_7 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_7_lower & df$time < period_7_upper])
                          colnames(df_subset_period_7) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_7 <- as.integer(quantile(df_subset_period_7$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_7 <- as.data.frame(rep(proposed_cycle_period_7,times = nrow(df_subset_period_7)))
                          colnames(df_proposed_cycle_7) <- c("proposed")
                          df_subset_period_7$diff <- abs(df_subset_period_7$Nominal_Cycle_Length - df_proposed_cycle_7$proposed)
                          diff_avg_period7 <- mean(df_subset_period_7$diff)
                          df_error_tmp_period7 <- data.frame(period_7_lower,period_7_upper,diff_avg_period7,proposed_cycle_period_7)
                          colnames(df_error_tmp_period7) <- c("period7_start","period7_end", "period7_error","period7_proposed")
                          df_subset_period_final <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_final_lower & df$time < period_final_upper])
                          colnames(df_subset_period_final) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_final <- as.integer(mean(df_subset_period_final$Nominal_Cycle_Length)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_final <- as.data.frame(rep(proposed_cycle_period_final,times = nrow(df_subset_period_final)))
                          colnames(df_proposed_cycle_final) <- c("proposed")
                          df_subset_period_final$diff <- abs(df_subset_period_final$Nominal_Cycle_Length - df_proposed_cycle_final$proposed)
                          diff_avg_periodfinal <- mean(df_subset_period_final$diff)
                          df_error_tmp_periodfinal <- data.frame(period_final_lower,period_final_upper,diff_avg_periodfinal,proposed_cycle_period_final)
                          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_periodfinal)
                          df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
                        } else {
                          period_7_upper <- 1440
                          df_subset_period_7 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_7_lower & df$time < period_7_upper])
                          colnames(df_subset_period_7) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_7 <- as.integer(quantile(df_subset_period_7$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_7 <- as.data.frame(rep(proposed_cycle_period_7,times = nrow(df_subset_period_7)))
                          colnames(df_proposed_cycle_7) <- c("proposed")
                          df_subset_period_7$diff <- abs(df_subset_period_7$Nominal_Cycle_Length - df_proposed_cycle_7$proposed)
                          diff_avg_period7 <- mean(df_subset_period_7$diff)
                          df_error_tmp_period7 <- data.frame(period_7_lower,period_7_upper,diff_avg_period7,proposed_cycle_period_7)
                          colnames(df_error_tmp_period7) <- c("period7_start","period7_end", "period7_error","period7_proposed")
                          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_periodfinal)
                          df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
                        }
                      }
                      }  else if (period_6_upper > 1440){
                        period_6_upper <- 1440
                        df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                        colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                        proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                        df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                        colnames(df_proposed_cycle_6) <- c("proposed")
                        df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                        diff_avg_period6 <- mean(df_subset_period_6$diff)
                        df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6,999,999,999,999)
                        colnames(df_error_tmp_period6) <- c("period6_start","period6_end","period6_error","period6_proposed",
                                                            "period7_start","period7_end","period7_error","period7_proposed")
                        df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                        colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                        df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_periodfinal)
                        df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
                      }
                        }  
                    }  else if (period_5_upper > 1440){
                      period_5_upper <- 1440
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5,999,999,999,999,999,999,999,999)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end","period5_error","period5_proposed",
                                                          "period6_start","period6_end","period6_error","period6_proposed",
                                                          "period7_start","period7_end","period7_error","period7_proposed")
                      df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                      colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                      df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_periodfinal)
                      df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
                    }
                  }   
                }  else if (period_4_upper > 1440){
                  period_4_upper <- 1440
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4,999,999,999,999,999,999,999,999,999,999,999,999)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end","period4_error","period4_proposed",
                                                      "period5_start","period5_end","period5_error","period5_proposed",
                                                      "period6_start","period6_end","period6_error","period6_proposed",
                                                      "period7_start","period7_end","period7_error","period7_proposed")
                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
                  df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
                }
              }
            }  else if (period_3_upper > 1440){
              period_3_upper <- 1440
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end","period3_error","period3_proposed",
                                                  "period4_start","period4_end","period4_error","period4_proposed",
                                                  "period5_start","period5_end","period5_error","period5_proposed",
                                                  "period6_start","period6_end","period6_error","period6_proposed",
                                                  "period7_start","period7_end","period7_error","period7_proposed")
              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
              df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
            }
          }
        } else if (period_2_upper > 1440){
          period_2_upper <- 1440
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed",
                                              "period3_start","period3_end", "period3_error","period3_proposed",
                                              "period4_start","period4_end", "period4_error","period4_proposed",
                                              "period5_start","period5_end", "period5_error","period5_proposed",
                                              "period6_start","period6_end", "period6_error","period6_proposed",
                                              "period7_start","period7_end", "period7_error","period7_proposed")
          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_periodfinal)
          df_error_7_periods <- rbind(df_error_7_periods, df_error_tmp)
        }
      }
    }
  }
  df_error_7_periods$overall_error <- 
    ((df_error_7_periods$period1_end-df_error_7_periods$period1_start)*df_error_7_periods$period1_error+
       (df_error_7_periods$period2_end-df_error_7_periods$period2_start)*df_error_7_periods$period2_error+
       (df_error_7_periods$period3_end-df_error_7_periods$period3_start)*df_error_7_periods$period3_error+
       (df_error_7_periods$period4_end-df_error_7_periods$period4_start)*df_error_7_periods$period4_error+
       (df_error_7_periods$period5_end-df_error_7_periods$period5_start)*df_error_7_periods$period5_error+
       (df_error_7_periods$period6_end-df_error_7_periods$period6_start)*df_error_7_periods$period6_error+
       (df_error_7_periods$period7_end-df_error_7_periods$period7_start)*df_error_7_periods$period7_error+
       (df_error_7_periods$periodfinal_end-df_error_7_periods$periodfinal_start)*df_error_7_periods$periodfinal_error)/1440
  df_error_7_periods_final <- df_error_7_periods[!is.na(df_error_7_periods$overall_error),]
  df_error_7_periods_final[df_error_7_periods_final==999] <- 0
  df_error_7_periods_final <- df_error_7_periods[df_error_7_periods_final$overall_error == min(df_error_7_periods_final$overall_error),]
  df_error_7_periods_final <- df_error_7_periods_final[1,]
  df_error_7_periods_final2 <- as.data.frame(t(as.matrix(df_error_7_periods_final)))
  df_error_7_periods_final2$rn <- row.names(df_error_7_periods_final2)
  colnames(df_error_7_periods_final2) <- c("value","rn")
  df_final <- sqldf::sqldf("select a.*
                           ,b.period1_proposed as proposed1
                           ,c.period2_proposed as proposed2
                           ,d.period3_proposed as proposed3
                           ,e.period4_proposed as proposed4
                           ,f.period5_proposed as proposed5
                           ,g.period6_proposed as proposed6
                           ,h.period7_proposed as proposed7
                           ,i.periodfinal_proposed as proposedfinal
                           from df as a
                           left join df_error_7_periods_final as b
                           on a.time = b.period1_start
                           left join df_error_7_periods_final as c
                           on a.time = c.period2_start
                           left join df_error_7_periods_final as d
                           on a.time = d.period3_start
                           left join df_error_7_periods_final as e
                           on a.time = e.period4_start
                           left join df_error_7_periods_final as f
                           on a.time = f.period5_start
                           left join df_error_7_periods_final as g
                           on a.time = f.period6_start
                           left join df_error_7_periods_final as h
                           on a.time = h.period7_start
                           left join df_error_7_periods_final as i
                           on a.time = i.periodfinal_start", drv = "SQLite")
  df_final$proposed <-  rowSums(df_final[,c("proposed1", "proposed2"
                                            ,"proposed3", "proposed4"
                                            ,"proposed5", "proposed6"
                                            ,"proposed7"
                                            ,"proposedfinal")], na.rm=TRUE)
  df_final$proposed1 <- NULL
  df_final$proposed2 <- NULL
  df_final$proposed3 <- NULL
  df_final$proposed4 <- NULL
  df_final$proposed5 <- NULL
  df_final$proposed6 <- NULL
  df_final$proposed7 <- NULL
  df_final$proposedfinal <- NULL
  df_final$proposed[df_final$proposed == 0] <- NA
  df_final <- df_final %>% tidyr::fill(proposed, .direction = c("down"))
}
if(maximum_periods == 8){
  df_error_8_periods <- data.frame(matrix(ncol = maximum_periods*4+4, nrow = 0))
  colnames(df_error_8_periods) <- c("period1_start","period1_end", "period1_error","period1_proposed",
                                    "period2_start","period2_end", "period2_error","period2_proposed",
                                    "period3_start","period3_end", "period3_error","period3_proposed",
                                    "period4_start","period4_end", "period4_error","period4_proposed",
                                    "period5_start","period5_end", "period5_error","period5_proposed",
                                    "period6_start","period6_end", "period6_error","period6_proposed",
                                    "period7_start","period7_end", "period7_error","period7_proposed",
                                    "period8_start","period8_end", "period8_error","period8_proposed",
                                    "periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
  for (hr in period1_hr_range){
    for (mts in period1_mts_range){
      period_1_lower <- 1
      period_1_upper <- 60*hr+mts*30
      df_subset_period_1 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_1_lower & df$time < period_1_upper])
      colnames(df_subset_period_1) <- c("Nominal_Cycle_Length")
      proposed_cycle_period_1 <- as.integer(quantile(df_subset_period_1$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
      df_proposed_cycle_1 <- as.data.frame(rep(proposed_cycle_period_1,times = nrow(df_subset_period_1)))
      colnames(df_proposed_cycle_1) <- c("proposed")
      df_subset_period_1$diff <- abs(df_subset_period_1$Nominal_Cycle_Length - df_proposed_cycle_1$proposed)
      diff_avg_period1 <- mean(df_subset_period_1$diff)
      df_error_tmp_period1 <- data.frame(period_1_lower,period_1_upper,diff_avg_period1,proposed_cycle_period_1)
      colnames(df_error_tmp_period1) <- c("period1_start","period1_end", "period1_error","period1_proposed")
      for (period2 in period_2_hrs){
        period_2_lower <- period_1_upper
        period_2_upper <- period_2_lower+period2*60
        if(period_2_upper <= 1440){
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed")
          for (period3 in period_3_hrs){
            period_3_lower <- period_2_upper
            period_3_upper <- period_3_lower+period3*60
            if(period_3_upper <= 1440){
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
              for (period4 in period_4_hrs){
                period_4_lower <- period_3_upper
                period_4_upper <- period_4_lower+period4*60
                if(period_4_upper <= 1440){
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
                  for (period5 in period_5_hrs){
                    period_5_lower <- period_4_upper
                    period_5_upper <- period_5_lower+period5*60
                    if(period_5_upper <= 1440){
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end", "period5_error","period5_proposed")
                      for (period6 in period_6_hrs){
                        period_6_lower <- period_5_upper
                        period_6_upper <- period_6_lower+period6*60
                        if(period_6_upper <= 1440){
                          df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                          colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                          colnames(df_proposed_cycle_6) <- c("proposed")
                          df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                          diff_avg_period6 <- mean(df_subset_period_6$diff)
                          df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6)
                          colnames(df_error_tmp_period6) <- c("period6_start","period6_end", "period6_error","period6_proposed")
                          for (period7 in period_7_hrs){
                            period_7_lower <- period_6_upper
                            period_7_upper <- period_7_lower+period6*60
                            if(period_7_upper <= 1440){
                              df_subset_period_7 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_7_lower & df$time < period_7_upper])
                              colnames(df_subset_period_7) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_7 <- as.integer(quantile(df_subset_period_7$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_7 <- as.data.frame(rep(proposed_cycle_period_7,times = nrow(df_subset_period_7)))
                              colnames(df_proposed_cycle_7) <- c("proposed")
                              df_subset_period_7$diff <- abs(df_subset_period_7$Nominal_Cycle_Length - df_proposed_cycle_7$proposed)
                              diff_avg_period7 <- mean(df_subset_period_7$diff)
                              df_error_tmp_period7 <- data.frame(period_7_lower,period_7_upper,diff_avg_period7,proposed_cycle_period_7)
                              colnames(df_error_tmp_period7) <- c("period7_start","period7_end", "period7_error","period7_proposed")
                          for (period8 in period_8_hrs){
                            period_8_lower <- period_7_upper
                            period_8_upper <- period_8_lower+period8*60
                            if(period_8_upper == 1440){
                              df_subset_period_8 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_8_lower & df$time < period_8_upper])
                              colnames(df_subset_period_8) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_8 <- as.integer(quantile(df_subset_period_8$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_8 <- as.data.frame(rep(proposed_cycle_period_8,times = nrow(df_subset_period_8)))
                              colnames(df_proposed_cycle_8) <- c("proposed")
                              df_subset_period_8$diff <- abs(df_subset_period_8$Nominal_Cycle_Length - df_proposed_cycle_8$proposed)
                              diff_avg_period8 <- mean(df_subset_period_8$diff)
                              df_error_tmp_period8 <- data.frame(period_8_lower,period_8_upper,diff_avg_period8,proposed_cycle_period_8)
                              colnames(df_error_tmp_period8) <- c("period8_start","period8_end", "period8_error","period8_proposed")
                              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_period8,df_error_tmp_periodfinal)
                              df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
                            } else if(period_8_upper < 1440){
                              period_final_lower <- period_8_upper
                              period_final_upper <- 1440
                              df_subset_period_8 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_8_lower & df$time < period_8_upper])
                              colnames(df_subset_period_8) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_8 <- as.integer(quantile(df_subset_period_8$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_8 <- as.data.frame(rep(proposed_cycle_period_8,times = nrow(df_subset_period_8)))
                              colnames(df_proposed_cycle_8) <- c("proposed")
                              df_subset_period_8$diff <- abs(df_subset_period_8$Nominal_Cycle_Length - df_proposed_cycle_8$proposed)
                              diff_avg_period8 <- mean(df_subset_period_8$diff)
                              df_error_tmp_period8 <- data.frame(period_8_lower,period_8_upper,diff_avg_period8,proposed_cycle_period_8)
                              colnames(df_error_tmp_period8) <- c("period8_start","period8_end", "period8_error","period8_proposed")
                              df_subset_period_final <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_final_lower & df$time < period_final_upper])
                              colnames(df_subset_period_final) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_final <- as.integer(mean(df_subset_period_final$Nominal_Cycle_Length)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_final <- as.data.frame(rep(proposed_cycle_period_final,times = nrow(df_subset_period_final)))
                              colnames(df_proposed_cycle_final) <- c("proposed")
                              df_subset_period_final$diff <- abs(df_subset_period_final$Nominal_Cycle_Length - df_proposed_cycle_final$proposed)
                              diff_avg_periodfinal <- mean(df_subset_period_final$diff)
                              df_error_tmp_periodfinal <- data.frame(period_final_lower,period_final_upper,diff_avg_periodfinal,proposed_cycle_period_final)
                              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_period8,df_error_tmp_periodfinal)
                              df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
                            } else {
                              period_8_upper <- 1440
                              df_subset_period_8 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_8_lower & df$time < period_8_upper])
                              colnames(df_subset_period_8) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_8 <- as.integer(quantile(df_subset_period_8$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_8 <- as.data.frame(rep(proposed_cycle_period_8,times = nrow(df_subset_period_8)))
                              colnames(df_proposed_cycle_8) <- c("proposed")
                              df_subset_period_8$diff <- abs(df_subset_period_8$Nominal_Cycle_Length - df_proposed_cycle_8$proposed)
                              diff_avg_period8 <- mean(df_subset_period_8$diff)
                              df_error_tmp_period8 <- data.frame(period_8_lower,period_8_upper,diff_avg_period8,proposed_cycle_period_8)
                              colnames(df_error_tmp_period8) <- c("period8_start","period8_end", "period8_error","period8_proposed")
                              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_period8,df_error_tmp_periodfinal)
                              df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
                            }
                          }
                            }  else if (period_7_upper > 1440){
                              period_7_upper <- 1440
                              df_subset_period_7 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_7_lower & df$time < period_7_upper])
                              colnames(df_subset_period_7) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_7 <- as.integer(quantile(df_subset_period_7$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_7 <- as.data.frame(rep(proposed_cycle_period_7,times = nrow(df_subset_period_7)))
                              colnames(df_proposed_cycle_7) <- c("proposed")
                              df_subset_period_7$diff <- abs(df_subset_period_7$Nominal_Cycle_Length - df_proposed_cycle_7$proposed)
                              diff_avg_period7 <- mean(df_subset_period_7$diff)
                              df_error_tmp_period7 <- data.frame(period_7_lower,period_7_upper,diff_avg_period7,proposed_cycle_period_7,999,999,999,999)
                              colnames(df_error_tmp_period7) <- c("period7_start","period7_end","period7_error","period7_proposed",
                                                                  "period8_start","period8_end","period8_error","period8_proposed")
                              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_periodfinal)
                              df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
                            }
                          }
                        }  else if (period_6_upper > 1440){
                          period_6_upper <- 1440
                          df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                          colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                          colnames(df_proposed_cycle_6) <- c("proposed")
                          df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                          diff_avg_period6 <- mean(df_subset_period_6$diff)
                          df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6,999,999,999,999,999,999,999,999)
                          colnames(df_error_tmp_period6) <- c("period6_start","period6_end","period6_error","period6_proposed",
                                                              "period7_start","period7_end","period7_error","period7_proposed",
                                                              "period8_start","period8_end","period8_error","period8_proposed")
                          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                          
                          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_periodfinal)
                          df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
                        }
                      }  
                    }  else if (period_5_upper > 1440){
                      period_5_upper <- 1440
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5,999,999,999,999,999,999,999,999,999,999,999,999)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end","period5_error","period5_proposed",
                                                          "period6_start","period6_end","period6_error","period6_proposed",
                                                          "period7_start","period7_end","period7_error","period7_proposed",
                                                          "period8_start","period8_end","period8_error","period8_proposed")
                      df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                      colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                      df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_periodfinal)
                      df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
                    }
                  }   
                }  else if (period_4_upper > 1440){
                  period_4_upper <- 1440
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end","period4_error","period4_proposed",
                                                      "period5_start","period5_end","period5_error","period5_proposed",
                                                      "period6_start","period6_end","period6_error","period6_proposed",
                                                      "period7_start","period7_end","period7_error","period7_proposed",
                                                      "period8_start","period8_end","period8_error","period8_proposed")
                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
                  df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
                }
              }
            }  else if (period_3_upper > 1440){
              period_3_upper <- 1440
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end","period3_error","period3_proposed",
                                                  "period4_start","period4_end","period4_error","period4_proposed",
                                                  "period5_start","period5_end","period5_error","period5_proposed",
                                                  "period6_start","period6_end","period6_error","period6_proposed",
                                                  "period7_start","period7_end","period7_error","period7_proposed",
                                                  "period8_start","period8_end","period8_error","period8_proposed")
              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
              df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
            }
          }
        } else if (period_2_upper > 1440){
          period_2_upper <- 1440
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed",
                                              "period3_start","period3_end", "period3_error","period3_proposed",
                                              "period4_start","period4_end", "period4_error","period4_proposed",
                                              "period5_start","period5_end", "period5_error","period5_proposed",
                                              "period6_start","period6_end", "period6_error","period6_proposed",
                                              "period7_start","period7_end", "period7_error","period7_proposed",
                                              "period8_start","period8_end", "period8_error","period8_proposed")
          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_periodfinal)
          df_error_8_periods <- rbind(df_error_8_periods, df_error_tmp)
        }
      }
    }
  }
  df_error_8_periods $overall_error <- 
    ((df_error_8_periods $period1_end-df_error_8_periods $period1_start)*df_error_8_periods $period1_error+
       (df_error_8_periods $period2_end-df_error_8_periods $period2_start)*df_error_8_periods $period2_error+
       (df_error_8_periods $period3_end-df_error_8_periods $period3_start)*df_error_8_periods $period3_error+
       (df_error_8_periods $period4_end-df_error_8_periods $period4_start)*df_error_8_periods $period4_error+
       (df_error_8_periods $period5_end-df_error_8_periods $period5_start)*df_error_8_periods $period5_error+
       (df_error_8_periods $period6_end-df_error_8_periods $period6_start)*df_error_8_periods $period6_error+
       (df_error_8_periods $period7_end-df_error_8_periods $period7_start)*df_error_8_periods $period7_error+
       (df_error_8_periods $period8_end-df_error_8_periods $period8_start)*df_error_8_periods $period8_error+
       (df_error_8_periods $periodfinal_end-df_error_8_periods $periodfinal_start)*df_error_8_periods $periodfinal_error)/1440
  df_error_8_periods_final <- df_error_8_periods[!is.na(df_error_8_periods$overall_error),]
  df_error_8_periods_final[df_error_8_periods_final==999] <- 0
  df_error_8_periods_final <- df_error_8_periods[df_error_8_periods_final$overall_error == min(df_error_8_periods_final$overall_error),]
  df_error_8_periods_final <- df_error_8_periods_final[1,]
  df_error_8_periods_final2 <- as.data.frame(t(as.matrix(df_error_8_periods_final)))
  df_error_8_periods_final2$rn <- row.names(df_error_8_periods_final2)
  colnames(df_error_8_periods_final2) <- c("value","rn")
  df_final <- sqldf::sqldf("select a.*
                           ,b.period1_proposed as proposed1
                           ,c.period2_proposed as proposed2
                           ,d.period3_proposed as proposed3
                           ,e.period4_proposed as proposed4
                           ,f.period5_proposed as proposed5
                           ,g.period6_proposed as proposed6
                           ,h.period7_proposed as proposed7
                           ,i.period8_proposed as proposed8
                           ,j.periodfinal_proposed as proposedfinal
                           from df as a
                           left join df_error_8_periods_final as b
                           on a.time = b.period1_start
                           left join df_error_8_periods_final as c
                           on a.time = c.period2_start
                           left join df_error_8_periods_final as d
                           on a.time = d.period3_start
                           left join df_error_8_periods_final as e
                           on a.time = e.period4_start
                           left join df_error_8_periods_final as f
                           on a.time = f.period5_start
                           left join df_error_8_periods_final as g
                           on a.time = f.period6_start
                           left join df_error_8_periods_final as h
                           on a.time = h.period7_start
                           left join df_error_8_periods_final as i
                           on a.time = i.period8_start
                           left join df_error_8_periods_final as j
                           on a.time = j.periodfinal_start", drv = "SQLite")
  df_final$proposed <-  rowSums(df_final[,c("proposed1", "proposed2"
                                            ,"proposed3", "proposed4"
                                            ,"proposed5", "proposed6"
                                            ,"proposed7","proposed8"
                                            ,"proposedfinal")], na.rm=TRUE)
  df_final$proposed1 <- NULL
  df_final$proposed2 <- NULL
  df_final$proposed3 <- NULL
  df_final$proposed4 <- NULL
  df_final$proposed5 <- NULL
  df_final$proposed6 <- NULL
  df_final$proposed7 <- NULL
  df_final$proposed8 <- NULL
  df_final$proposedfinal <- NULL
  df_final$proposed[df_final$proposed == 0] <- NA
  df_final <- df_final %>% tidyr::fill(proposed, .direction = c("down"))
}
if(maximum_periods == 9){
  df_error_9_periods <- data.frame(matrix(ncol = maximum_periods*4+4, nrow = 0))
  colnames(df_error_9_periods) <- c("period1_start","period1_end", "period1_error","period1_proposed",
                                    "period2_start","period2_end", "period2_error","period2_proposed",
                                    "period3_start","period3_end", "period3_error","period3_proposed",
                                    "period4_start","period4_end", "period4_error","period4_proposed",
                                    "period5_start","period5_end", "period5_error","period5_proposed",
                                    "period6_start","period6_end", "period6_error","period6_proposed",
                                    "period7_start","period7_end", "period7_error","period7_proposed",
                                    "period8_start","period8_end", "period8_error","period8_proposed",
                                    "period9_start","period9_end", "period9_error","period9_proposed",
                                    "periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
  for (hr in period1_hr_range){
    for (mts in period1_mts_range){
      period_1_lower <- 1
      period_1_upper <- 60*hr+mts*30
      df_subset_period_1 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_1_lower & df$time < period_1_upper])
      colnames(df_subset_period_1) <- c("Nominal_Cycle_Length")
      proposed_cycle_period_1 <- as.integer(quantile(df_subset_period_1$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
      df_proposed_cycle_1 <- as.data.frame(rep(proposed_cycle_period_1,times = nrow(df_subset_period_1)))
      colnames(df_proposed_cycle_1) <- c("proposed")
      df_subset_period_1$diff <- abs(df_subset_period_1$Nominal_Cycle_Length - df_proposed_cycle_1$proposed)
      diff_avg_period1 <- mean(df_subset_period_1$diff)
      df_error_tmp_period1 <- data.frame(period_1_lower,period_1_upper,diff_avg_period1,proposed_cycle_period_1)
      colnames(df_error_tmp_period1) <- c("period1_start","period1_end", "period1_error","period1_proposed")
      for (period2 in period_2_hrs){
        period_2_lower <- period_1_upper
        period_2_upper <- period_2_lower+period2*60
        if(period_2_upper <= 1440){
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed")
          for (period3 in period_3_hrs){
            period_3_lower <- period_2_upper
            period_3_upper <- period_3_lower+period3*60
            if(period_3_upper <= 1440){
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end", "period3_error","period3_proposed")
              for (period4 in period_4_hrs){
                period_4_lower <- period_3_upper
                period_4_upper <- period_4_lower+period4*60
                if(period_4_upper <= 1440){
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end", "period4_error","period4_proposed")
                  for (period5 in period_5_hrs){
                    period_5_lower <- period_4_upper
                    period_5_upper <- period_5_lower+period5*60
                    if(period_5_upper <= 1440){
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end", "period5_error","period5_proposed")
                      for (period6 in period_6_hrs){
                        period_6_lower <- period_5_upper
                        period_6_upper <- period_6_lower+period6*60
                        if(period_6_upper <= 1440){
                          df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                          colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                          colnames(df_proposed_cycle_6) <- c("proposed")
                          df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                          diff_avg_period6 <- mean(df_subset_period_6$diff)
                          df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6)
                          colnames(df_error_tmp_period6) <- c("period6_start","period6_end", "period6_error","period6_proposed")
                          for (period7 in period_7_hrs){
                            period_7_lower <- period_6_upper
                            period_7_upper <- period_7_lower+period6*60
                            if(period_7_upper <= 1440){
                              df_subset_period_7 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_7_lower & df$time < period_7_upper])
                              colnames(df_subset_period_7) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_7 <- as.integer(quantile(df_subset_period_7$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_7 <- as.data.frame(rep(proposed_cycle_period_7,times = nrow(df_subset_period_7)))
                              colnames(df_proposed_cycle_7) <- c("proposed")
                              df_subset_period_7$diff <- abs(df_subset_period_7$Nominal_Cycle_Length - df_proposed_cycle_7$proposed)
                              diff_avg_period7 <- mean(df_subset_period_7$diff)
                              df_error_tmp_period7 <- data.frame(period_7_lower,period_7_upper,diff_avg_period7,proposed_cycle_period_7)
                              colnames(df_error_tmp_period7) <- c("period7_start","period7_end", "period7_error","period7_proposed")
                              for (period8 in period_8_hrs){
                                period_8_lower <- period_7_upper
                                period_8_upper <- period_8_lower+period6*60
                                if(period_8_upper <= 1440){
                                  df_subset_period_8 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_8_lower & df$time < period_8_upper])
                                  colnames(df_subset_period_8) <- c("Nominal_Cycle_Length")
                                  proposed_cycle_period_8 <- as.integer(quantile(df_subset_period_8$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                                  df_proposed_cycle_8 <- as.data.frame(rep(proposed_cycle_period_8,times = nrow(df_subset_period_8)))
                                  colnames(df_proposed_cycle_8) <- c("proposed")
                                  df_subset_period_8$diff <- abs(df_subset_period_8$Nominal_Cycle_Length - df_proposed_cycle_8$proposed)
                                  diff_avg_period8 <- mean(df_subset_period_8$diff)
                                  df_error_tmp_period8 <- data.frame(period_8_lower,period_8_upper,diff_avg_period8,proposed_cycle_period_8)
                                  colnames(df_error_tmp_period8) <- c("period8_start","period8_end", "period8_error","period8_proposed")
                              for (period9 in period_9_hrs){
                                period_9_lower <- period_8_upper
                                period_9_upper <- period_9_lower+period8*60
                                if(period_9_upper == 1440){
                                  df_subset_period_9 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_9_lower & df$time < period_9_upper])
                                  colnames(df_subset_period_9) <- c("Nominal_Cycle_Length")
                                  proposed_cycle_period_9 <- as.integer(quantile(df_subset_period_9$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                                  df_proposed_cycle_9 <- as.data.frame(rep(proposed_cycle_period_9,times = nrow(df_subset_period_9)))
                                  colnames(df_proposed_cycle_9) <- c("proposed")
                                  df_subset_period_9$diff <- abs(df_subset_period_9$Nominal_Cycle_Length - df_proposed_cycle_9$proposed)
                                  diff_avg_period9 <- mean(df_subset_period_9$diff)
                                  df_error_tmp_period9 <- data.frame(period_9_lower,period_9_upper,diff_avg_period9,proposed_cycle_period_9)
                                  colnames(df_error_tmp_period9) <- c("period9_start","period9_end", "period9_error","period9_proposed")
                                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_period8,df_error_tmp_period9,df_error_tmp_periodfinal)
                                  df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                                } else if(period_9_upper < 1440){
                                  period_final_lower <- period_9_upper
                                  period_final_upper <- 1440
                                  df_subset_period_9 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_9_lower & df$time < period_9_upper])
                                  colnames(df_subset_period_9) <- c("Nominal_Cycle_Length")
                                  proposed_cycle_period_9 <- as.integer(quantile(df_subset_period_9$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                                  df_proposed_cycle_9 <- as.data.frame(rep(proposed_cycle_period_9,times = nrow(df_subset_period_9)))
                                  colnames(df_proposed_cycle_9) <- c("proposed")
                                  df_subset_period_9$diff <- abs(df_subset_period_9$Nominal_Cycle_Length - df_proposed_cycle_9$proposed)
                                  diff_avg_period9 <- mean(df_subset_period_9$diff)
                                  df_error_tmp_period9 <- data.frame(period_9_lower,period_9_upper,diff_avg_period9,proposed_cycle_period_9)
                                  colnames(df_error_tmp_period9) <- c("period9_start","period9_end", "period9_error","period9_proposed")
                                  df_subset_period_final <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_final_lower & df$time < period_final_upper])
                                  colnames(df_subset_period_final) <- c("Nominal_Cycle_Length")
                                  proposed_cycle_period_final <- as.integer(mean(df_subset_period_final$Nominal_Cycle_Length)/cycle_length_rounded)*cycle_length_rounded
                                  df_proposed_cycle_final <- as.data.frame(rep(proposed_cycle_period_final,times = nrow(df_subset_period_final)))
                                  colnames(df_proposed_cycle_final) <- c("proposed")
                                  df_subset_period_final$diff <- abs(df_subset_period_final$Nominal_Cycle_Length - df_proposed_cycle_final$proposed)
                                  diff_avg_periodfinal <- mean(df_subset_period_final$diff)
                                  df_error_tmp_periodfinal <- data.frame(period_final_lower,period_final_upper,diff_avg_periodfinal,proposed_cycle_period_final)
                                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_period8,df_error_tmp_period9,df_error_tmp_periodfinal)
                                  df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                                } else {
                                  period_9_upper <- 1440
                                  df_subset_period_9 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_9_lower & df$time < period_9_upper])
                                  colnames(df_subset_period_9) <- c("Nominal_Cycle_Length")
                                  proposed_cycle_period_9 <- as.integer(quantile(df_subset_period_9$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                                  df_proposed_cycle_9 <- as.data.frame(rep(proposed_cycle_period_9,times = nrow(df_subset_period_9)))
                                  colnames(df_proposed_cycle_9) <- c("proposed")
                                  df_subset_period_9$diff <- abs(df_subset_period_9$Nominal_Cycle_Length - df_proposed_cycle_9$proposed)
                                  diff_avg_period9 <- mean(df_subset_period_9$diff)
                                  df_error_tmp_period9 <- data.frame(period_9_lower,period_9_upper,diff_avg_period9,proposed_cycle_period_9)
                                  colnames(df_error_tmp_period9) <- c("period9_start","period9_end", "period9_error","period9_proposed")
                                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_period8,df_error_tmp_period9,df_error_tmp_periodfinal)
                                  df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                                }
                              }
                                }  else if (period_8_upper > 1440){
                                  period_8_upper <- 1440
                                  df_subset_period_8 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_8_lower & df$time < period_8_upper])
                                  colnames(df_subset_period_8) <- c("Nominal_Cycle_Length")
                                  proposed_cycle_period_8 <- as.integer(quantile(df_subset_period_8$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                                  df_proposed_cycle_8 <- as.data.frame(rep(proposed_cycle_period_8,times = nrow(df_subset_period_8)))
                                  colnames(df_proposed_cycle_8) <- c("proposed")
                                  df_subset_period_8$diff <- abs(df_subset_period_8$Nominal_Cycle_Length - df_proposed_cycle_8$proposed)
                                  diff_avg_period8 <- mean(df_subset_period_8$diff)
                                  df_error_tmp_period8 <- data.frame(period_8_lower,period_8_upper,diff_avg_period8,proposed_cycle_period_8,999,999,999,999)
                                  colnames(df_error_tmp_period8) <- c("period8_start","period8_end","period8_error","period8_proposed",
                                                                      "period9_start","period9_end","period9_error","period9_proposed")
                                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_period8,df_error_tmp_periodfinal)
                                  df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                                }
                              }
                            }  else if (period_7_upper > 1440){
                              period_7_upper <- 1440
                              df_subset_period_7 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_7_lower & df$time < period_7_upper])
                              colnames(df_subset_period_7) <- c("Nominal_Cycle_Length")
                              proposed_cycle_period_7 <- as.integer(quantile(df_subset_period_7$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                              df_proposed_cycle_7 <- as.data.frame(rep(proposed_cycle_period_7,times = nrow(df_subset_period_7)))
                              colnames(df_proposed_cycle_7) <- c("proposed")
                              df_subset_period_7$diff <- abs(df_subset_period_7$Nominal_Cycle_Length - df_proposed_cycle_7$proposed)
                              diff_avg_period7 <- mean(df_subset_period_7$diff)
                              df_error_tmp_period7 <- data.frame(period_7_lower,period_7_upper,diff_avg_period7,proposed_cycle_period_7,999,999,999,999,999,999,999,999)
                              colnames(df_error_tmp_period7) <- c("period7_start","period7_end","period7_error","period7_proposed",
                                                                  "period8_start","period8_end","period8_error","period8_proposed",
                                                                  "period9_start","period9_end","period9_error","period9_proposed")
                              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_period7,df_error_tmp_periodfinal)
                              df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                            }
                          }
                        }  else if (period_6_upper > 1440){
                          period_6_upper <- 1440
                          df_subset_period_6 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_6_lower & df$time < period_6_upper])
                          colnames(df_subset_period_6) <- c("Nominal_Cycle_Length")
                          proposed_cycle_period_6 <- as.integer(quantile(df_subset_period_6$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                          df_proposed_cycle_6 <- as.data.frame(rep(proposed_cycle_period_6,times = nrow(df_subset_period_6)))
                          colnames(df_proposed_cycle_6) <- c("proposed")
                          df_subset_period_6$diff <- abs(df_subset_period_6$Nominal_Cycle_Length - df_proposed_cycle_6$proposed)
                          diff_avg_period6 <- mean(df_subset_period_6$diff)
                          df_error_tmp_period6 <- data.frame(period_6_lower,period_6_upper,diff_avg_period6,proposed_cycle_period_6,999,999,999,999,999,999,999,999,999,999,999,999)
                          colnames(df_error_tmp_period6) <- c("period6_start","period6_end","period6_error","period6_proposed",
                                                              "period7_start","period7_end","period7_error","period7_proposed",
                                                              "period8_start","period8_end","period8_error","period8_proposed",
                                                              "period9_start","period9_end","period9_error","period9_proposed")
                          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_period6,df_error_tmp_periodfinal)
                          df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                        }
                      }  
                    }  else if (period_5_upper > 1440){
                      period_5_upper <- 1440
                      df_subset_period_5 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_5_lower & df$time < period_5_upper])
                      colnames(df_subset_period_5) <- c("Nominal_Cycle_Length")
                      proposed_cycle_period_5 <- as.integer(quantile(df_subset_period_5$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                      df_proposed_cycle_5 <- as.data.frame(rep(proposed_cycle_period_5,times = nrow(df_subset_period_5)))
                      colnames(df_proposed_cycle_5) <- c("proposed")
                      df_subset_period_5$diff <- abs(df_subset_period_5$Nominal_Cycle_Length - df_proposed_cycle_5$proposed)
                      diff_avg_period5 <- mean(df_subset_period_5$diff)
                      df_error_tmp_period5 <- data.frame(period_5_lower,period_5_upper,diff_avg_period5,proposed_cycle_period_5,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
                      colnames(df_error_tmp_period5) <- c("period5_start","period5_end","period5_error","period5_proposed",
                                                          "period6_start","period6_end","period6_error","period6_proposed",
                                                          "period7_start","period7_end","period7_error","period7_proposed",
                                                          "period8_start","period8_end","period8_error","period8_proposed",
                                                          "period9_start","period9_end","period9_error","period9_proposed")
                      df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                      colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                      df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_period5,df_error_tmp_periodfinal)
                      df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                    }
                  }   
                }  else if (period_4_upper > 1440){
                  period_4_upper <- 1440
                  df_subset_period_4 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_4_lower & df$time < period_4_upper])
                  colnames(df_subset_period_4) <- c("Nominal_Cycle_Length")
                  proposed_cycle_period_4 <- as.integer(quantile(df_subset_period_4$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
                  df_proposed_cycle_4 <- as.data.frame(rep(proposed_cycle_period_4,times = nrow(df_subset_period_4)))
                  colnames(df_proposed_cycle_4) <- c("proposed")
                  df_subset_period_4$diff <- abs(df_subset_period_4$Nominal_Cycle_Length - df_proposed_cycle_4$proposed)
                  diff_avg_period4 <- mean(df_subset_period_4$diff)
                  df_error_tmp_period4 <- data.frame(period_4_lower,period_4_upper,diff_avg_period4,proposed_cycle_period_4,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
                  colnames(df_error_tmp_period4) <- c("period4_start","period4_end","period4_error","period4_proposed",
                                                      "period5_start","period5_end","period5_error","period5_proposed",
                                                      "period6_start","period6_end","period6_error","period6_proposed",
                                                      "period7_start","period7_end","period7_error","period7_proposed",
                                                      "period8_start","period8_end","period8_error","period8_proposed",
                                                      "period9_start","period9_end","period9_error","period9_proposed")
                  df_error_tmp_periodfinal <- data.frame(999,999,999,999)
                  colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
                  df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_period4,df_error_tmp_periodfinal)
                  df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
                }
              }
            }  else if (period_3_upper > 1440){
              period_3_upper <- 1440
              df_subset_period_3 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_3_lower & df$time < period_3_upper])
              colnames(df_subset_period_3) <- c("Nominal_Cycle_Length")
              proposed_cycle_period_3 <- as.integer(quantile(df_subset_period_3$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
              df_proposed_cycle_3 <- as.data.frame(rep(proposed_cycle_period_3,times = nrow(df_subset_period_3)))
              colnames(df_proposed_cycle_3) <- c("proposed")
              df_subset_period_3$diff <- abs(df_subset_period_3$Nominal_Cycle_Length - df_proposed_cycle_3$proposed)
              diff_avg_period3 <- mean(df_subset_period_3$diff)
              df_error_tmp_period3 <- data.frame(period_3_lower,period_3_upper,diff_avg_period3,proposed_cycle_period_3,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
              colnames(df_error_tmp_period3) <- c("period3_start","period3_end","period3_error","period3_proposed",
                                                  "period4_start","period4_end","period4_error","period4_proposed",
                                                  "period5_start","period5_end","period5_error","period5_proposed",
                                                  "period6_start","period6_end","period6_error","period6_proposed",
                                                  "period7_start","period7_end","period7_error","period7_proposed",
                                                  "period8_start","period8_end","period8_error","period8_proposed",
                                                  "period9_start","period9_end","period9_error","period9_proposed")
              df_error_tmp_periodfinal <- data.frame(999,999,999,999)
              colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
              df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_period3,df_error_tmp_periodfinal)
              df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
            }
          }
        } else if (period_2_upper > 1440){
          period_2_upper <- 1440
          df_subset_period_2 <- as.data.frame(df$Nominal_Cycle_Length[df$time >= period_2_lower & df$time < period_2_upper])
          colnames(df_subset_period_2) <- c("Nominal_Cycle_Length")
          proposed_cycle_period_2 <- as.integer(quantile(df_subset_period_2$Nominal_Cycle_Length, prob = percentile)/cycle_length_rounded)*cycle_length_rounded
          df_proposed_cycle_2 <- as.data.frame(rep(proposed_cycle_period_2,times = nrow(df_subset_period_2)))
          colnames(df_proposed_cycle_2) <- c("proposed")
          df_subset_period_2$diff <- abs(df_subset_period_2$Nominal_Cycle_Length - df_proposed_cycle_2$proposed)
          diff_avg_period2 <- mean(df_subset_period_2$diff)
          df_error_tmp_period2 <- data.frame(period_2_lower,period_2_upper,diff_avg_period2,proposed_cycle_period_2,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
          colnames(df_error_tmp_period2) <- c("period2_start","period2_end", "period2_error","period2_proposed",
                                              "period3_start","period3_end", "period3_error","period3_proposed",
                                              "period4_start","period4_end", "period4_error","period4_proposed",
                                              "period5_start","period5_end", "period5_error","period5_proposed",
                                              "period6_start","period6_end", "period6_error","period6_proposed",
                                              "period7_start","period7_end", "period7_error","period7_proposed",
                                              "period8_start","period8_end", "period8_error","period8_proposed",
                                              "period9_start","period9_end", "period9_error","period9_proposed")
          df_error_tmp_periodfinal <- data.frame(999,999,999,999)
          colnames(df_error_tmp_periodfinal) <- c("periodfinal_start","periodfinal_end", "periodfinal_error","periodfinal_proposed")
          df_error_tmp <- cbind(df_error_tmp_period1,df_error_tmp_period2,df_error_tmp_periodfinal)
          df_error_9_periods <- rbind(df_error_9_periods, df_error_tmp)
        }
      }
    }
  }
  df_error_9_periods  $overall_error <- 
    ((df_error_9_periods  $period1_end-df_error_9_periods  $period1_start)*df_error_9_periods  $period1_error+
       (df_error_9_periods  $period2_end-df_error_9_periods  $period2_start)*df_error_9_periods  $period2_error+
       (df_error_9_periods  $period3_end-df_error_9_periods  $period3_start)*df_error_9_periods  $period3_error+
       (df_error_9_periods  $period4_end-df_error_9_periods  $period4_start)*df_error_9_periods  $period4_error+
       (df_error_9_periods  $period5_end-df_error_9_periods  $period5_start)*df_error_9_periods  $period5_error+
       (df_error_9_periods  $period6_end-df_error_9_periods  $period6_start)*df_error_9_periods  $period6_error+
       (df_error_9_periods  $period7_end-df_error_9_periods  $period7_start)*df_error_9_periods  $period7_error+
       (df_error_9_periods  $period8_end-df_error_9_periods  $period8_start)*df_error_9_periods  $period8_error+
       (df_error_9_periods  $period9_end-df_error_9_periods  $period9_start)*df_error_9_periods  $period9_error+
       (df_error_9_periods  $periodfinal_end-df_error_9_periods  $periodfinal_start)*df_error_9_periods  $periodfinal_error)/1440
  df_error_9_periods_final <- df_error_9_periods[!is.na(df_error_9_periods$overall_error),]
  df_error_9_periods_final[df_error_9_periods_final==999] <- 0
  df_error_9_periods_final <- df_error_9_periods[df_error_9_periods_final$overall_error == min(df_error_9_periods_final$overall_error),]
  df_error_9_periods_final <- df_error_9_periods_final[1,]
  df_error_9_periods_final2 <- as.data.frame(t(as.matrix(df_error_9_periods_final)))
  df_error_9_periods_final2$rn <- row.names(df_error_9_periods_final2)
  colnames(df_error_9_periods_final2) <- c("value","rn")
  df_final <- sqldf::sqldf("select a.*
                           ,b.period1_proposed as proposed1
                           ,c.period2_proposed as proposed2
                           ,d.period3_proposed as proposed3
                           ,e.period4_proposed as proposed4
                           ,f.period5_proposed as proposed5
                           ,g.period6_proposed as proposed6
                           ,h.period7_proposed as proposed7
                           ,i.period8_proposed as proposed8
                           ,j.period9_proposed as proposed9
                           ,k.periodfinal_proposed as proposedfinal
                           from df as a
                           left join df_error_9_periods_final as b
                           on a.time = b.period1_start
                           left join df_error_9_periods_final as c
                           on a.time = c.period2_start
                           left join df_error_9_periods_final as d
                           on a.time = d.period3_start
                           left join df_error_9_periods_final as e
                           on a.time = e.period4_start
                           left join df_error_9_periods_final as f
                           on a.time = f.period5_start
                           left join df_error_9_periods_final as g
                           on a.time = f.period6_start
                           left join df_error_9_periods_final as h
                           on a.time = h.period7_start
                           left join df_error_9_periods_final as i
                           on a.time = i.period8_start
                           left join df_error_9_periods_final as j
                           on a.time = j.period9_start
                           left join df_error_9_periods_final as k
                           on a.time = k.periodfinal_start", drv = "SQLite")
  df_final$proposed <-  rowSums(df_final[,c("proposed1", "proposed2"
                                            ,"proposed3", "proposed4"
                                            ,"proposed5", "proposed6"
                                            ,"proposed7","proposed8",
                                            "proposed9","proposedfinal")], na.rm=TRUE)
  df_final$proposed1 <- NULL
  df_final$proposed2 <- NULL
  df_final$proposed3 <- NULL
  df_final$proposed4 <- NULL
  df_final$proposed5 <- NULL
  df_final$proposed6 <- NULL
  df_final$proposed7 <- NULL
  df_final$proposed8 <- NULL
  df_final$proposed9 <- NULL
  df_final$proposedfinal <- NULL
  df_final$proposed[df_final$proposed == 0] <- NA
  df_final <- df_final %>% tidyr::fill(proposed, .direction = c("down"))
}
return(df_final)
}