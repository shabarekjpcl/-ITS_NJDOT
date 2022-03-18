function5_midweek <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    
    df <- df_final
    
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    
    
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
    df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
    Date_Tue_list <- unique(df222_final_Tue$Date)
    for ( i in length(Date_Tue_list)){
      print(as.Date(Date_Tue_list[i]))
      df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
      df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                              " group by a.time, a.weektype"), drv = "SQLite")
      df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
      df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
    }
    df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
    Date_Wed_list <- unique(df222_final_Wed$Date)
    for ( i in length(Date_Wed_list)){
      print(as.Date(Date_Wed_list[i]))
      df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
      df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                              " group by a.time, a.weektype"), drv = "SQLite")
      df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
      df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
    }
    df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
    Date_Thu_list <- unique(df222_final_Thu$Date)
    for ( i in length(Date_Thu_list)){
      print(as.Date(Date_Thu_list[i]))
      df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
      df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                              " group by a.time, a.weektype"), drv = "SQLite")
      df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
      df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
    }
    df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
    Date_Sat_list <- unique(df222_final_Sat$Date)
    for ( i in length(Date_Sat_list)){
      print(as.Date(Date_Sat_list[i]))
      df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
      df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                              " group by a.time, a.weektype"), drv = "SQLite")
      df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
      df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
    }
    df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
    Date_Sun_list <- unique(df222_final_Sun$Date)
    for ( i in length(Date_Sun_list)){
      print(as.Date(Date_Sun_list[i]))
      df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
      df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                              " group by a.time, a.weektype"), drv = "SQLite")
      df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
      df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
    }
    df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                           " group by a.time, a.weektype, a.Day"), drv = "SQLite")
    if(zoneid !=0){
      df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
    }
    df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
    df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                           " group by a.time, a.weektype, a.Day"), drv = "SQLite")
    if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
    }
    df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
    df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                           " group by a.time, a.weektype, a.Day"), drv = "SQLite")
    if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
    }
    df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
    df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                           " group by a.time, a.weektype, a.Day"), drv = "SQLite")
    if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
    }
    df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
    df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                           " group by a.time, a.weektype, a.Day"), drv = "SQLite")
    if(zoneid !=0){
      df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
    }
    df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
    
    
    df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                           " group by a.time, a.weektype, a.Day"), drv = "SQLite")
    if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
    }
    df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
    df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                           " group by a.time, a.weektype, a.Day"), drv = "SQLite")
    if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
    }
    df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
    df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                               " group by a.time, a.weektype"),drv="SQLite")
    if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    } else {
      df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
    }
    
    df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_midweek)
}
function5_mon <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  
  
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
  df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
  Date_Tue_list <- unique(df222_final_Tue$Date)
  for ( i in length(Date_Tue_list)){
    print(as.Date(Date_Tue_list[i]))
    df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
    df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
  }
  df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
  Date_Wed_list <- unique(df222_final_Wed$Date)
  for ( i in length(Date_Wed_list)){
    print(as.Date(Date_Wed_list[i]))
    df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
    df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
  }
  df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
  Date_Thu_list <- unique(df222_final_Thu$Date)
  for ( i in length(Date_Thu_list)){
    print(as.Date(Date_Thu_list[i]))
    df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
    df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
  }
  df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
  Date_Sat_list <- unique(df222_final_Sat$Date)
  for ( i in length(Date_Sat_list)){
    print(as.Date(Date_Sat_list[i]))
    df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
    df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
  }
  df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
  Date_Sun_list <- unique(df222_final_Sun$Date)
  for ( i in length(Date_Sun_list)){
    print(as.Date(Date_Sun_list[i]))
    df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
    df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
  }
  df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
  }
  df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
  df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
  }
  df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
  df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
  }
  df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
  df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
  }
  df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
  df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
  }
  df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
  df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
  }
  df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
  df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
  }
  df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
  df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                             " group by a.time, a.weektype"),drv="SQLite")
  if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
  }
  
  df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_Mon2)
}
function5_tue <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
  df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
  Date_Tue_list <- unique(df222_final_Tue$Date)
  for ( i in length(Date_Tue_list)){
    print(as.Date(Date_Tue_list[i]))
    df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
    df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
  }
  df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
  Date_Wed_list <- unique(df222_final_Wed$Date)
  for ( i in length(Date_Wed_list)){
    print(as.Date(Date_Wed_list[i]))
    df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
    df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
  }
  df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
  Date_Thu_list <- unique(df222_final_Thu$Date)
  for ( i in length(Date_Thu_list)){
    print(as.Date(Date_Thu_list[i]))
    df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
    df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
  }
  df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
  Date_Sat_list <- unique(df222_final_Sat$Date)
  for ( i in length(Date_Sat_list)){
    print(as.Date(Date_Sat_list[i]))
    df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
    df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
  }
  df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
  Date_Sun_list <- unique(df222_final_Sun$Date)
  for ( i in length(Date_Sun_list)){
    print(as.Date(Date_Sun_list[i]))
    df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
    df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
  }
  df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
  }
  df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
  df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
  }
  df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
  df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
  }
  df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
  df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
  }
  df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
  df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
  }
  df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
  df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
  }
  df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
  df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
  }
  df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
  df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                             " group by a.time, a.weektype"),drv="SQLite")
  if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
  }
  
  df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_Tue2)
}
function5_wed <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
  df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
  Date_Tue_list <- unique(df222_final_Tue$Date)
  for ( i in length(Date_Tue_list)){
    print(as.Date(Date_Tue_list[i]))
    df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
    df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
  }
  df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
  Date_Wed_list <- unique(df222_final_Wed$Date)
  for ( i in length(Date_Wed_list)){
    print(as.Date(Date_Wed_list[i]))
    df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
    df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
  }
  df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
  Date_Thu_list <- unique(df222_final_Thu$Date)
  for ( i in length(Date_Thu_list)){
    print(as.Date(Date_Thu_list[i]))
    df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
    df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
  }
  df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
  Date_Sat_list <- unique(df222_final_Sat$Date)
  for ( i in length(Date_Sat_list)){
    print(as.Date(Date_Sat_list[i]))
    df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
    df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
  }
  df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
  Date_Sun_list <- unique(df222_final_Sun$Date)
  for ( i in length(Date_Sun_list)){
    print(as.Date(Date_Sun_list[i]))
    df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
    df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
  }
  df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
  }
  df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
  
  
  df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
  }
  df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
  df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
  }
  df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
  df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
  }
  df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
  df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
  }
  df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
  df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
  }
  df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
  df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
  }
  df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
  df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                             " group by a.time, a.weektype"),drv="SQLite")
  if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
  }
  df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_Wed2)
}
function5_thu <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
  df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
  Date_Tue_list <- unique(df222_final_Tue$Date)
  for ( i in length(Date_Tue_list)){
    print(as.Date(Date_Tue_list[i]))
    df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
    df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
  }
  df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
  Date_Wed_list <- unique(df222_final_Wed$Date)
  for ( i in length(Date_Wed_list)){
    print(as.Date(Date_Wed_list[i]))
    df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
    df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
  }
  df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
  Date_Thu_list <- unique(df222_final_Thu$Date)
  for ( i in length(Date_Thu_list)){
    print(as.Date(Date_Thu_list[i]))
    df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
    df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
  }
  df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
  Date_Sat_list <- unique(df222_final_Sat$Date)
  for ( i in length(Date_Sat_list)){
    print(as.Date(Date_Sat_list[i]))
    df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
    df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
  }
  df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
  Date_Sun_list <- unique(df222_final_Sun$Date)
  for ( i in length(Date_Sun_list)){
    print(as.Date(Date_Sun_list[i]))
    df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
    df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
  }
  df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
  }
  df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
  df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
  }
  df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
  df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
  }
  df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
  df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
  }
  df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
  df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
  }
  df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
  df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
  }
  df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
  df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
  }
  df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
  df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                             " group by a.time, a.weektype"),drv="SQLite")
  if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
  }
  
  df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_Thu2)
}
function5_fri <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
  df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
  Date_Tue_list <- unique(df222_final_Tue$Date)
  for ( i in length(Date_Tue_list)){
    print(as.Date(Date_Tue_list[i]))
    df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
    df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
  }
  df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
  Date_Wed_list <- unique(df222_final_Wed$Date)
  for ( i in length(Date_Wed_list)){
    print(as.Date(Date_Wed_list[i]))
    df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
    df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
  }
  df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
  Date_Thu_list <- unique(df222_final_Thu$Date)
  for ( i in length(Date_Thu_list)){
    print(as.Date(Date_Thu_list[i]))
    df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
    df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
  }
  df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
  Date_Sat_list <- unique(df222_final_Sat$Date)
  for ( i in length(Date_Sat_list)){
    print(as.Date(Date_Sat_list[i]))
    df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
    df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
  }
  df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
  Date_Sun_list <- unique(df222_final_Sun$Date)
  for ( i in length(Date_Sun_list)){
    print(as.Date(Date_Sun_list[i]))
    df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
    df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
  }
  df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
  }
  df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
  df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
  }
  df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
  df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
  }
  df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
  df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
  }
  df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
  df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
  }
  df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
  
  
  df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
  }
  df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
  df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
  }
  df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
  df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                             " group by a.time, a.weektype"),drv="SQLite")
  if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
  }
  
  df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_Fri2)
}
function5_sat <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
  df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
  Date_Tue_list <- unique(df222_final_Tue$Date)
  for ( i in length(Date_Tue_list)){
    print(as.Date(Date_Tue_list[i]))
    df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
    df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
  }
  df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
  Date_Wed_list <- unique(df222_final_Wed$Date)
  for ( i in length(Date_Wed_list)){
    print(as.Date(Date_Wed_list[i]))
    df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
    df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
  }
  df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
  Date_Thu_list <- unique(df222_final_Thu$Date)
  for ( i in length(Date_Thu_list)){
    print(as.Date(Date_Thu_list[i]))
    df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
    df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
  }
  df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
  Date_Sat_list <- unique(df222_final_Sat$Date)
  for ( i in length(Date_Sat_list)){
    print(as.Date(Date_Sat_list[i]))
    df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
    df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
  }
  df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
  Date_Sun_list <- unique(df222_final_Sun$Date)
  for ( i in length(Date_Sun_list)){
    print(as.Date(Date_Sun_list[i]))
    df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
    df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
  }
  df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
  }
  df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
  df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
  }
  df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
  df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
  }
  df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
  df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
  }
  df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
  df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
  }
  df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
  df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
  }
  df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
  df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
  }
  df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
  df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                             " group by a.time, a.weektype"),drv="SQLite")
  if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
  }
  df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_Sat2)
}
function5_sun <- function(inputdir,zoneid){
  sm_list <- list.files(path = inputdir ,pattern = ".*.txt")
  df_final_heat <- NULL
  site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
  phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
  phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
  zones_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/Zones.csv"
  TOD_cycle_length_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/TOD/TOD_cycle_length.csv"
  df222_final <- NULL
  for (kij in sm_list){
    print(kij)
    df <- read.delim2(paste0(inputdir,"/",kij),header = FALSE)
    df_final <- NULL
    df_tmp1 <- df
    library(dplyr)
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    df <- df_final
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    for (i in 1:nrow(df)){
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    df1 <- readr::read_csv(phase_naming_file)
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(subsite = c("")))
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "up")
    df22 <- df22 %>% tidyr::fill(subsite, .direction = "down")
    df22$V1 <- NULL
    df22$Degree_Saturation <- NULL
    df22$type <- NULL
    df22$strategic_approach <- NULL
    df22$notation <- NULL
    df22$phase <- NULL
    df22$phase_time <- NULL
    df22$Avg_DS_Phase <- NULL
    df22$phase_A_GT <- NULL
    df22$phase_B_GT <- NULL
    df22$phase_C_GT <- NULL
    df22$phase_D_GT <- NULL
    df22$phase_E_GT <- NULL
    df22$phase_F_GT <- NULL
    df22$phase_G_GT <- NULL
    df22$phase_H_GT <- NULL
    df22$phase_I_GT <- NULL
    df22$Date_name <- NULL
    df22$SS <- NULL
    df22$Phase_name <- NULL
    df22 <- df22[is.na(df22$Cycle_Length)==FALSE,]
    df_time <- data.frame(matrix(, nrow=(60*24), ncol=1))
    colnames(df_time) <- c("time")
    df_time$time <- (1:(24*60))
    df_time$time <- df_time$time*60
    df222 <- sqldf::sqldf("select b.subsite, b.Date, b.Day, a.time, b.Cycle_Length, b.Required_Cycle_Length  from df_time as a
left join df22 as b
on a.time = b.time",drv="SQLite")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "down")
    df222 <- df222 %>% tidyr::fill(subsite, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Date, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Day, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Cycle_Length, .direction = "up")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df222 <- df222 %>% tidyr::fill(Required_Cycle_Length, .direction = "up")
    for (i in (1:nrow(df222))){
      if(df222$Day[i] == "Tuesday" |  df222$Day[i] == "Wednesday" | df222$Day[i] == "Thursday"){
        df222$weektype[i] <- "Midweek"} else {
          df222$weektype[i] <- df222$Day[i]
        }
    }
    TOD_cycle_length <- readr::read_csv(TOD_cycle_length_file)
    if(zoneid !=0){
      TOD_cycle_length <- TOD_cycle_length[TOD_cycle_length$Zone==zoneid,]
    }
    TOD_cycle_length$Time <- as.integer(hms::as_hms(TOD_cycle_length$Time))
    df222$subsite <- NULL
    df222_final <- rbind(df222,df222_final)}
  df222_final_Tue <- df222_final[df222_final$Day == "Tuesday",]
  Date_Tue_list <- unique(df222_final_Tue$Date)
  for ( i in length(Date_Tue_list)){
    print(as.Date(Date_Tue_list[i]))
    df222_final_Tue2 <- df222_final_Tue[df222_final_Tue$Date == as.Date(Date_Tue_list[i]),]
    df222_final2_Tue <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Tue2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Tue <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Tue$time <- hms::as_hms(df222_final2_Tue $time)
  }
  df222_final_Wed <- df222_final[df222_final$Day == "Wednesday",]
  Date_Wed_list <- unique(df222_final_Wed$Date)
  for ( i in length(Date_Wed_list)){
    print(as.Date(Date_Wed_list[i]))
    df222_final_Wed2 <- df222_final_Wed[df222_final_Wed$Date == as.Date(Date_Wed_list[i]),]
    df222_final2_Wed <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Wed2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Wed <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Wed$time <- hms::as_hms(df222_final2_Wed $time)
  }
  df222_final_Thu <- df222_final[df222_final$Day == "Thursday",]
  Date_Thu_list <- unique(df222_final_Thu$Date)
  for ( i in length(Date_Thu_list)){
    print(as.Date(Date_Thu_list[i]))
    df222_final_Thu2 <- df222_final_Thu[df222_final_Thu$Date == as.Date(Date_Thu_list[i]),]
    df222_final2_Thu <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Thu2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Thu <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Thu$time <- hms::as_hms(df222_final2_Thu $time)
  }
  df222_final_Sat <- df222_final[df222_final$Day == "Saturday",]
  Date_Sat_list <- unique(df222_final_Sat$Date)
  for ( i in length(Date_Sat_list)){
    print(as.Date(Date_Sat_list[i]))
    df222_final_Sat2 <- df222_final_Sat[df222_final_Sat$Date == as.Date(Date_Sat_list[i]),]
    df222_final2_Sat <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sat2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sat <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sat$time <- hms::as_hms(df222_final2_Sat $time)
  }
  df222_final_Sun <- df222_final[df222_final$Day == "Sunday",]
  Date_Sun_list <- unique(df222_final_Sun$Date)
  for ( i in length(Date_Sun_list)){
    print(as.Date(Date_Sun_list[i]))
    df222_final_Sun2 <- df222_final_Sun[df222_final_Sun$Date == as.Date(Date_Sun_list[i]),]
    df222_final2_Sun <- sqldf::sqldf(paste0("select a.Date, a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final_Sun2 as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' ",
                                            " group by a.time, a.weektype"), drv = "SQLite")
    df222_final2_Sun <- sqldf::sqldf("select a.Date, a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final2_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
    df222_final2_Sun$time <- hms::as_hms(df222_final2_Sun $time)
  }
  df222_final_Mon <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Monday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Mon as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Mon2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Mon as a", drv = "SQLite") 
  }
  df222_final_Mon2$time <- hms::as_hms(df222_final_Mon2$time)
  df222_final_Tue <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Tuesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Tue as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Tue2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Tue as a", drv = "SQLite") 
  }
  df222_final_Tue2$time <- hms::as_hms(df222_final_Tue2$time)
  df222_final_Wed <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Wednesday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Wed as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Wed2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Wed as a", drv = "SQLite")
  }
  df222_final_Wed2$time <- hms::as_hms(df222_final_Wed2$time)
  df222_final_Thu <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Thursday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Thu as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Thu2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Thu as a", drv = "SQLite")  
  }
  df222_final_Thu2$time <- hms::as_hms(df222_final_Thu2$time)
  df222_final_Fri <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Friday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Fri as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Fri2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Fri as a", drv = "SQLite") 
  }
  df222_final_Fri2$time <- hms::as_hms(df222_final_Fri2$time)
  df222_final_Sat <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Saturday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sat as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sat2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sat as a", drv = "SQLite")
  }
  df222_final_Sat2$time <- hms::as_hms(df222_final_Sat2$time)
  df222_final_Sun <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.Day = 'Sunday' ",
                                         " group by a.time, a.weektype, a.Day"), drv = "SQLite")
  if(zoneid !=0){
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_Sun as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_Sun2 <- sqldf::sqldf("select a.time, a.Day, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_Sun as a", drv = "SQLite")
  }
  df222_final_Sun2$time <- hms::as_hms(df222_final_Sun2$time)
  df222_final_midweek <- sqldf::sqldf(paste0("select a.time, a.Day, a.weektype, avg(a.Cycle_Length) as Nominal_Cycle_Length, avg(a.Required_Cycle_Length) as Required_Cycle_Length
                             from df222_final as a
                             where a.weektype <> 'Monday' and a.weektype <> 'Friday' and a.weektype ='Midweek' ",
                                             " group by a.time, a.weektype"),drv="SQLite")
  if(zoneid !=0){
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, b.Cycle_Length as TOD_Cycle_Length
                      from df222_final_midweek as a
                      Left join TOD_cycle_length as b
                      On a.time = b.Time and a.Day = b.Day_name", drv = "SQLite")
  } else {
    df222_final_midweek <- sqldf::sqldf("select a.time, a.weektype, a.Nominal_Cycle_Length, a.Required_Cycle_Length, 0 as TOD_Cycle_Length
                      from df222_final_midweek as a", drv = "SQLite")
  }
  df222_final_midweek$time <- hms::as_hms(df222_final_midweek$time)
  return(df222_final_Sun2)
}