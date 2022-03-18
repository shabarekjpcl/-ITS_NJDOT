TOD_phase_optimization_from_files <- function(input_phase_folder,analysis_time_type){
  library(dplyr)
  list_phase_files <- list.files(input_phase_folder, pattern="*.csv")
  df_final_phases <- NULL
  for (p_k in list_phase_files){
    df <- readr::read_csv(paste0(input_phase_folder,"/",p_k))
    df$date <- as.Date(substr(df$Start,1,10))
    df$start_time <- as.integer(hms::as_hms(substr(df$Start,12,19)))
    df$day <- weekdays(df$date)
    df <- df[df$Phase!= "Unknown",]
    phases <- unique(df$Phase)
    if(df$Phase[1]!="<A>"){
      df <- df[-c(1),]
      if(df$Phase[1]!="<A>"){
        df <- df[-c(1),]
        if(df$Phase[1]!="<A>"){
          df <- df[-c(1),]
          if(df$Phase[1]!="<A>"){
            df <- df[-c(1),]
            if(df$Phase[1]!="<A>"){
              df <- df[-c(1),]
              if(df$Phase[1]!="<A>"){
                df <- df[-c(1),]
                if(df$Phase[1]!="<A>"){
                  df <- df[-c(1),]
                  if(df$Phase[1]!="<A>"){
                    df <- df[-c(1),]
                    if(df$Phase[1]!="<A>"){
                      df <- df[-c(1),]
                      if(df$Phase[1]!="<A>"){
                        df <- df[-c(1),]
                        if(df$Phase[1]!="<A>"){
                          df <- df[-c(1),]}}}}}}}}}}}
    df$rank <- rank(df$Phase,ties.method= "first")
    df$rank[df$Phase!="<A>"] <- NA
    df$start_time2 <- df$start_time
    df$start_time2[df$Phase!="<A>"] <- NA
    df <- df %>% tidyr::fill(rank,.direction = c("down"))
    df <- df %>% tidyr::fill(start_time2,.direction = c("down"))
    df <- data.table::as.data.table(df)
    df <- df[, actual_cycle_length := sum(`Duration (s)`), by = "rank"][]
    rank_df <- df %>% dplyr::select("rank","day","date","start_time2","actual_cycle_length")
    rank_df <- data.frame(unique(rank_df))
    phases_df <- data.frame(phases)
    occur_df <- data.frame(1:max(df$rank))
    colnames(occur_df) <- c("rank")
    df1 <- merge(phases_df, occur_df,by=NULL)
    df2 <- sqldf::sqldf("select a.phases, a.rank, b.`Duration (s)`, b.Start, b.End, b.Gapped, c.date, b.start_time, c.day, c.start_time2, c.actual_cycle_length
                    from df1 as a
                    left join df as b
                    on a.rank = b.rank
                    and a.phases = b.Phase
                    left join rank_df as c
                    on a.rank = c.rank", drv = "SQLite")
    df2$`Duration (s)`[is.na(df2$`Duration (s)`)] <- 0
    df2$split_percentage <- df2$`Duration (s)`/df2$actual_cycle_length
    df2$start_time3 <- as.integer(df2$start_time2/60)*60
    df3 <- sqldf::sqldf("select a.start_time3, a.phases, a.day, a.date, avg(a.split_percentage) as split_percentage, avg(a.actual_cycle_length) as actual_cycle_length
                    from df2 as a
                    group by a.start_time3, a.phases, a.day, a.date", drv = "SQLite")
    time_df <- data.frame(seq(60,60*60*24,by=60))
    colnames(time_df) <- c("time")
    time_df2 <- merge(phases_df, time_df,by=NULL)
    df4 <- sqldf::sqldf("select b.time, a.start_time3, b.phases, a.day, a.date, a.split_percentage, a.actual_cycle_length
                    from time_df2 as b
                    left join df3 as a
                    on b.time = a.start_time3
                    and b.phases = a.phases", drv = "SQLite")
    df4 <- df4 %>% tidyr::fill(start_time3,.direction = c("downup"))
    df4 <- df4 %>% tidyr::fill(day,.direction = c("downup"))
    df4 <- df4 %>% tidyr::fill(date,.direction = c("downup"))
    df4 <- df4 %>% tidyr::fill(actual_cycle_length,.direction = c("downup"))
    df5 <-  df4[with(df4, order(phases, time)),]
    df5 <- df5 %>% tidyr::fill(split_percentage,.direction ="downup")
    df5 <-  df5[with(df5, order(time)),]
    rownames(df5) <- 1:nrow(df5)
    df_final_phases <- rbind(df5,df_final_phases)
  }
  df_final_Mon <- df_final_phases[df_final_phases$day == "Monday",]
  df_final_Tue <- df_final_phases[df_final_phases$day == "Tuesday",]
  df_final_Wed <- df_final_phases[df_final_phases$day == "Wednesday",]
  df_final_Thu <- df_final_phases[df_final_phases$day == "Thursday",]
  df_final_Fri <- df_final_phases[df_final_phases$day == "Friday",]
  df_final_Sat <- df_final_phases[df_final_phases$day == "Saturday",]
  df_final_Sun <- df_final_phases[df_final_phases$day == "Sunday",]
  df_final_midweek <- df_final_phases[df_final_phases$day == "Tuesday" | df_final_phases$day == "Wednesday" | df_final_phases$day == "Thursday",]
  if(analysis_time_type == "midweek"){
    df_final_selected <- df_final_midweek
  }
  if(analysis_time_type == "monday"){
    df_final_selected <- df_final_Mon
  }
  if(analysis_time_type == "tuesday"){
    df_final_selected <- df_final_Tue
  }
  if(analysis_time_type == "wednesday"){
    df_final_selected <- df_final_Wed
  }
  if(analysis_time_type == "thursday"){
    df_final_selected <- df_final_Thu
  }
  if(analysis_time_type == "friday"){
    df_final_selected <- df_final_Fri
  }
  if(analysis_time_type == "saturday"){
    df_final_selected <- df_final_Sat
  }
  if(analysis_time_type == "sunday"){
    df_final_selected <- df_final_Sun
  }
  df_final_selected$start_time3 <- NULL
  df_final_selected <- sqldf::sqldf("select a.time, a.phases, avg(a.split_percentage) as split_percentage, avg(a.actual_cycle_length) as avg_cycle_length
                                  from df_final_selected as a
                                  group by a.time, a.phases", drv = "SQLite")
  return(df_final_selected)
}