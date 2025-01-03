
#should include 'path_manipulated_data' and 'path_raw_data'
source("path.R")

library(foreign)
library(tidyverse)
library(lubridate)



list_all_system_log <- readRDS(file = file.path(path_manipulated_data, "list_all_system_log.rds"))
dat_mars_for_nonema_assessed_proximal_outcome <- readRDS(file = file.path(path_manipulated_data, 
                                                                          "dat_mars_for_nonema_assessed_proximal_outcome.rds"))


###Curate responses to initial notifications and their associated timestamps from the system log file###

dat_mars_for_nonema_assessed_proximal_outcome$ts_emi_resp <- rep(NA,  nrow(dat_mars_for_nonema_assessed_proximal_outcome))
dat_mars_for_nonema_assessed_proximal_outcome$emi_resp <- rep(NA,  nrow(dat_mars_for_nonema_assessed_proximal_outcome))

for (s in 1:length(list_all_system_log)){
  
  curr_id <- list_all_system_log[[s]]$mars_id[1]
  print(curr_id)
  
  curr_df <- dat_mars_for_nonema_assessed_proximal_outcome %>% 
    filter(mars_id == curr_id)
  
  #skip in case system log not found
  if(nrow(curr_df) == 0){
    next
  }
  
  sys_log <- list_all_system_log[[s]]
  
  for (j in 1:nrow(curr_df)){
    
    #skip decision points with no randomization or no prompt
    if(is.na(curr_df$A[j])){
      next
    }
    if(curr_df$A[j] == "none"){
      next
    }
    
    #retain only relevant rows (timestamps fit in current block)
    #divide 1000 because system log uses millisecond
    sys_log_s <- sys_log %>% 
      filter(as.numeric(sys_log$V1) / 1000 >= as.numeric(int_start(curr_df$block_bounds_mountain[j])) & 
               as.numeric(sys_log$V1) / 1000 <= as.numeric(int_end(curr_df$block_bounds_mountain[j])))
    
    if(nrow(sys_log_s) == 0){
      next
    }
    
    for (i in 1:nrow(sys_log_s)){
      
      #find flags for randomization to either low effort or high effort strategy, and the row logging participant's response
      if((sys_log_s$V5[i] == "BackgroundService/Listen(init)/Scheduler(EMI-RANDOM)/what(EMI-RANDOM)[2 0]/state(NOTIFICATION_0_60_120_EMI)" |
          sys_log_s$V5[i] == "BackgroundService/Listen(init)/Scheduler(EMI-RANDOM)/what(EMI-RANDOM)[3 0]/state(NOTIFICATION_0_60_120_EMI)") &
         str_detect(sys_log_s$V6[i], "response=")){
        
        curr_df$ts_emi_resp[j] <- sys_log_s$V1[i]
        
        resp_list <- str_split(sys_log_s$V6[i], " ")
        curr_df$emi_resp[j] <- str_remove(sapply(resp_list, "[", c(1)), "response=")
        
        break
      }
    }
  }
  
  dat_mars_for_nonema_assessed_proximal_outcome[dat_mars_for_nonema_assessed_proximal_outcome$mars_id == curr_id, 'ts_emi_resp'] <- curr_df$ts_emi_resp
  dat_mars_for_nonema_assessed_proximal_outcome[dat_mars_for_nonema_assessed_proximal_outcome$mars_id == curr_id, 'emi_resp'] <- curr_df$emi_resp
  
}



###Below are functions for curating app usage variables###

#Instead of coding everything under a for loop, most operations are inscribed into the functions

curate_mars_int <- function(df, id){
  #count the num of state in V3
  df$num_page <- rep(NA, nrow(df))
  #count the num of "selected" or "pressed" in V6
  df$num_click <- rep(NA, nrow(df))
  #whether they got in to the MARS app
  df$in_mars <- rep(NA, nrow(df))
  #whether they started an activity (has anything other than main_log or menu in V4)
  df$activ_started <- rep(NA, nrow(df))
  #whether they finished an activity (has like_page in V4)
  df$activ_done <- rep(NA, nrow(df))
  #whether they finished multiple activities
  df$activ_done_m <- rep(NA, nrow(df))
  #whether they finished an activity, considering reading time (avg time + 2SD)
  df$activ_done_wtime <- rep(NA, nrow(df))
  #whether they finished an activity, considering reading time (avg time)
  df$activ_done_wavgtime <- rep(NA, nrow(df))
  #time spent in the app
  df$time_spent <- rep(NA, nrow(df))
  
  
  #start and end time for current subject
  start_t <- as.numeric(int_start(df$block_bounds_mountain[1]))
  end_t <- as.numeric(int_end(df$block_bounds_mountain[nrow(df)]))
  
  fname_mars_log <- str_c(path_raw_data, id, 'LOG--org.md2k.mars.csv.bz2',
                          sep = "/")
  if(file.exists(fname_mars_log)){
    mars_log <- read.csv(fname_mars_log, header = FALSE,
                         col.names = paste0("V", c(1, 2, 3, 4, 5, 6)))
    mars_log_s <- mars_log %>% 
      filter(V1 / 1000 >= start_t & 
               V1 / 1000 <= end_t) %>% 
      distinct(.keep_all = TRUE)
    
    if(nrow(mars_log_s) > 0){
      
      
      #going over each decision point for current subject
      for (r in 1:nrow(df)){
        if(is.na(df$ts_coinflip_mountain[r])){
          
          #if no randomization, use block start time and 1 hour after as boundaries
          #if randomization time found, use randomization time as the start boundary, use 1 hour after or block end time (whichever is earlier) as end boundary
          curr_mars <- mars_log_s %>% 
            filter(mars_log_s$V1 / 1000 >= as.numeric(int_start(df$block_bounds_mountain[r])) & 
                     mars_log_s$V1 / 1000 <= (as.numeric(int_start(df$block_bounds_mountain[r])) + 3600))
        } else{
          curr_mars <- mars_log_s %>% 
            filter(mars_log_s$V1 / 1000 >= as.numeric(df$ts_coinflip_mountain[r]) & 
                     mars_log_s$V1 / 1000 <=  min(as.numeric(int_end(df$block_bounds_mountain[r])), 
                                                  as.numeric(df$ts_coinflip_mountain[r]) + 3600))
        }
        
        #if no app activity in the given time period, code zeros
        if(nrow(curr_mars) == 0){
          df$in_mars[r] <- 0
          df$num_page[r] <- 0
          df$num_click[r] <- 0
          df$activ_started[r] <- 0
          df$activ_done[r] <- 0
          df$activ_done_m[r] <- 0
          df$time_spent[r] <- 0
        } else{
          
          #if app activity found, curate the relevant variables
          
          #one has to be in the app for any app activity to be logged
          df$in_mars[r] <- 1
          
          #"state flag" in the third column (V3) reliable indicators for pages subject went through
          df$num_page[r] <- curr_mars %>% 
            filter(V3 == "state") %>% 
            nrow(.)
          
          #"select", "press", "play" covers all types of clicking in the app
          df$num_click[r] <- curr_mars %>% 
            filter(grepl('selected', .$V6, fixed = TRUE) |
                     grepl('pressed', .$V6, fixed = TRUE) |
                     grepl('play', .$V6, fixed = TRUE)) %>% 
            nrow(.)
          
          #if subject is in any exercise, V4 must have logged things other than "main_logo" or "menu"
          df$activ_started[r] <- as.integer(nrow(filter(curr_mars, !grepl('main_logo', curr_mars$V4, fixed = TRUE) & 
                                                          !grepl('menu', curr_mars$V4, fixed = TRUE))) != 0)
          
          ##coding completion status below (consider quitting during meditate and joy)
          
          #simpler decision if not meditate or joy; just find like_page
          if(nrow(filter(curr_mars, grepl('like_page', curr_mars$V4, fixed = TRUE) & 
                         !grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE) &
                         !grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE))) != 0){
            
            df$activ_done[r] <- 1
            
            ##coding completion status with reading speed consideration
            
            #initialize values
            surf_time <- FALSE
            notice_time <- FALSE
            imagine_time <- FALSE
            surf_time_avg <- FALSE
            notice_time_avg <- FALSE
            imagine_time_avg <- FALSE
            
            if(nrow(filter(curr_mars, grepl('moodsurf_6_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              
              #finding start and end time for each exercise
              surf_end <- which(curr_mars$V4 == 'moodsurf_6_like_page')
              surf_end_time <- curr_mars$V1[surf_end[length(surf_end)]]
              surf_start <- which(curr_mars$V4 == 'moodsurf_load_page')
              if(length(surf_start) == 0){
                surf_start_time <- curr_mars$V1[1]
              } else{
                surf_start_time <- curr_mars$V1[surf_start[1]]
              }
              
              #168 is the total number of words in the moodsurf exercise
              #surf_time codes if time spent in the moodsurf exercise meets the criterion using avg + 2sd reading speed
              #surf_time_avg codes if time spent in the moodsurf exercise meets the criterion using avg reading speed
              surf_time <- ((surf_end_time - surf_start_time) / 1000) > (168*60/340)
              surf_time_avg <- ((surf_end_time - surf_start_time) / 1000) > (168*60/240)
            }
            
            
            ##the two sections below is using the same logic to the moodsurf section above
            
            if(nrow(filter(curr_mars, grepl('notice_9_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              notice_end <- which(curr_mars$V4 == 'notice_9_like_page')
              notice_end_time <- curr_mars$V1[notice_end[length(notice_end)]]
              notice_start <- which(curr_mars$V4 == 'notice_load_page')
              if(length(notice_start) == 0){
                notice_start_time <- curr_mars$V1[1]
              } else{
                notice_start_time <- curr_mars$V1[notice_start[1]]
              }
              notice_time <- ((notice_end_time - notice_start_time) / 1000) > (255*60/340)
              notice_time_avg <- ((notice_end_time - notice_start_time) / 1000) > (255*60/240)
            }
            
            if(nrow(filter(curr_mars, grepl('imagine_8_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              imagine_end <- which(curr_mars$V4 == 'imagine_8_like_page')
              imagine_end_time <- curr_mars$V1[imagine_end[length(imagine_end)]]
              imagine_start <- which(curr_mars$V4 == 'imagine_load_page')
              if(length(imagine_start) == 0){
                imagine_start_time <- curr_mars$V1[1]
              } else{
                imagine_start_time <- curr_mars$V1[imagine_start[1]]
              }
              imagine_time <- ((imagine_end_time - imagine_start_time) / 1000) > (183*60/340)
              imagine_time_avg <- ((imagine_end_time - imagine_start_time) / 1000) > (183*60/240)
            }
            
            df$activ_done_wtime[r] <- (surf_time | notice_time | imagine_time)
            df$activ_done_wavgtime[r] <- (surf_time_avg | notice_time_avg | imagine_time_avg)
            
            
          } else{
            
            ##if meditate or joy, further processing
            #check meditate first: find if quitting halfway
            meditate_end <- nrow(filter(curr_mars, grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE)))
            quit_meditate <- nrow(filter(curr_mars, grepl('quit_from_meditation_exercise_yes', curr_mars$V5, fixed = TRUE)))
            meditate_done <- (meditate_end > 0) & (quit_meditate == 0)
            
            
            #for joy: not completed if quitting halfway, but one exception is that if there is the flag for more time it means they completed at least one time without quitting
            joy_end <- nrow(filter(curr_mars, grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE)))
            quit_joy <- nrow(filter(curr_mars, grepl('quit_from_breath_exercise_yes', curr_mars$V5, fixed = TRUE)))
            joy_more_time <- nrow(filter(curr_mars, grepl('want more time', curr_mars$V5, fixed = TRUE) &
                                           grepl('yes pressed', curr_mars$V6, fixed = TRUE)))
            joy_done <- ((joy_end > 0) & (quit_joy == 0)) | ((joy_end > 0) & (joy_more_time > 0))
            
            df$activ_done[r] <- as.integer(meditate_done | joy_done)
            
            
            ##coding completion status with reading speed consideration; same logic with the ones above
            
            meditate_time <- FALSE
            joy_time <- FALSE
            meditate_time_avg <- FALSE
            joy_time_avg <- FALSE
            
            if(nrow(filter(curr_mars, grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              meditate_end <- which(curr_mars$V4 == 'meditate_3_like_page')
              meditate_end_time <- curr_mars$V1[meditate_end[length(meditate_end)]]
              meditate_start <- which(curr_mars$V4 == 'meditate_load_page')
              if(length(meditate_start) == 0){
                meditate_start_time <- curr_mars$V1[1]
              } else{
                meditate_start_time <- curr_mars$V1[meditate_start[1]]
              }
              meditate_time <- ((meditate_end_time - meditate_start_time) / 1000) > (168 + 32*60/340)
              meditate_time_avg <- ((meditate_end_time - meditate_start_time) / 1000) > (168 + 32*60/240)
            }
            
            if(nrow(filter(curr_mars, grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              joy_end <- which(curr_mars$V4 == 'joy_8_like_page')
              joy_end_time <- curr_mars$V1[joy_end[length(joy_end)]]
              joy_start <- which(curr_mars$V4 == 'joy_load_page')
              if(length(joy_start) == 0){
                joy_start_time <- curr_mars$V1[1]
              } else{
                joy_start_time <- curr_mars$V1[joy_start[1]]
              }
              joy_time <- ((joy_end_time - joy_start_time) / 1000) > (60 + 144*60/340)
              joy_time_avg <- ((joy_end_time - joy_start_time) / 1000) > (60 + 144*60/240)
            }
            
            df$activ_done_wtime[r] <- (meditate_time | joy_time)
            df$activ_done_wavgtime[r] <- (meditate_time_avg | joy_time_avg)
            
          }
          
          #count number of completion flags
          num_activ <- curr_mars %>% 
            filter(grepl('like_page', .$V4, fixed = TRUE)) %>% 
            distinct(V4) %>% 
            nrow(.)
          
          #curate whether completed more than one exercise
          df$activ_done_m[r] <- as.integer(num_activ > 1 & df$activ_done[r] == 1)
          
          #curate time spent (one threshold is that it removes durations longer than 10min with nothing happening)
          time_list <- diff(curr_mars$V1)
          time_list <- time_list[time_list < 600000]
          df$time_spent[r] <- sum(time_list, na.rm = TRUE)
          
        } 
      }
    }
  }
  
  return(df)
}




####This is for curating MARS interaction (only prompted usage)###

#Repetitive codes are not annotated. Some unique processing will be annotated.

curate_mars_int_p <- function(df, id){
  #count the num of state in V3
  df$num_page_p <- rep(NA, nrow(df))
  #count the num of selected or pressed in V6
  df$num_click_p <- rep(NA, nrow(df))
  #whether they got in to the MARS app
  df$in_mars_p <- rep(NA, nrow(df))
  #whether they started an activity (has anything other than main_log or menu in V4)
  df$activ_started_p <- rep(NA, nrow(df))
  #whether they clicked either letsgo or show all
  df$first_resp_p <- rep(NA, nrow(df))
  #whether they finished an activity (has like_page in V4)
  df$activ_done_p <- rep(NA, nrow(df))
  #whether they finished multiple activities
  df$activ_done_m_p <- rep(NA, nrow(df))
  #whether the timing meets completion criteria
  df$activ_done_wtime_p <- rep(NA, nrow(df))
  df$activ_done_wavgtime_p <- rep(NA, nrow(df))
  #time spent in the app
  df$time_spent_p <- rep(NA, nrow(df))
  
  start_t <- as.numeric(int_start(df$block_bounds_mountain[1]))
  end_t <- as.numeric(int_end(df$block_bounds_mountain[nrow(df)]))
  
  fname_mars_log <- str_c(path_raw_data, id, 'LOG--org.md2k.mars.csv.bz2',
                          sep = "/")
  if(file.exists(fname_mars_log)){
    mars_log <- read.csv(fname_mars_log, header = FALSE,
                         col.names = paste0("V", c(1, 2, 3, 4, 5, 6)))
    mars_log_s <- mars_log %>% 
      filter(V1 / 1000 >= start_t & 
               V1 / 1000 <= end_t) %>% 
      distinct(.keep_all = TRUE)
    
    if(nrow(mars_log_s) > 0){
      for (r in 1:nrow(df)){
        if(is.na(df$ts_coinflip_mountain[r])){
          
          df$in_mars_p[r] <- 0
          df$num_page_p[r] <- 0
          df$num_click_p[r] <- 0
          df$activ_started_p[r] <- 0
          df$first_resp_p[r] <- 0
          df$activ_done_p[r] <- 0
          df$activ_done_m_p[r] <- 0
          df$time_spent_p[r] <- 0
          
        } else if (df$A[r] != 'mars' | df$emi_resp[r] != 'Ok'){
          
          df$in_mars_p[r] <- 0
          df$num_page_p[r] <- 0
          df$num_click_p[r] <- 0
          df$activ_started_p[r] <- 0
          df$first_resp_p[r] <- 0
          df$activ_done_p[r] <- 0
          df$activ_done_m_p[r] <- 0
          df$time_spent_p[r] <- 0
          
        } else {
          curr_mars_all <- mars_log_s %>% 
            filter(mars_log_s$V1 / 1000 >= as.numeric(df$ts_coinflip_mountain[r]) & 
                     mars_log_s$V1 / 1000 <=  min(as.numeric(int_end(df$block_bounds_mountain[r])), 
                                                  as.numeric(df$ts_coinflip_mountain[r]) + 3600))
          
          
          ##This is a key part of the processing where logs corresponding to prompted app usage are parsed
          
          #The start of any mars app usage has 'main_logo_page' in V4 and 'randomselection' in V5
          start_index <- which(curr_mars_all$V4 == 'main_logo_page' & 
                                 curr_mars_all$V5 == 'RandomSelection')
          
          if (length(start_index) == 0){
            df$in_mars_p[r] <- 0
            df$num_page_p[r] <- 0
            df$num_click_p[r] <- 0
            df$activ_started_p[r] <- 0
            df$first_resp_p[r] <- 0
            df$activ_done_p[r] <- 0
            df$activ_done_m_p[r] <- 0
            df$time_spent_p[r] <- 0
            next
            
          } else if (length(start_index) == 1){
            
            #If only one start_index, it means the subject only accessed the app once during the time period
            #Then just keep all the logs after the start_index
            curr_mars <- as.data.frame(curr_mars_all[c(start_index[1]:nrow(curr_mars_all)), ])
            
          } else {
            
            #If more than one start_index, only keep the logs between the first and second start_index
            #assumption here is that the second app access does not count towards the prompted usage
            curr_mars <- as.data.frame(curr_mars_all[c(start_index[1]:start_index[2] - 1), ])
          }
          
          df$in_mars_p[r] <- 1
          df$num_page_p[r] <- curr_mars %>% 
            filter(V3 == "state") %>% 
            nrow(.)
          df$num_click_p[r] <- curr_mars %>% 
            filter(grepl('selected', .$V6, fixed = TRUE) |
                     grepl('pressed', .$V6, fixed = TRUE) |
                     grepl('play', .$V6, fixed = TRUE)) %>% 
            nrow(.)
          df$activ_started_p[r] <- as.integer(nrow(filter(curr_mars, !grepl('main_logo', curr_mars$V4, fixed = TRUE) & 
                                                            !grepl('menu', curr_mars$V4, fixed = TRUE))) != 0)
          
          showall <- as.integer(nrow(filter(curr_mars, grepl('Show All pressed', curr_mars$V6, fixed = TRUE))) != 0)
          letsgo <- as.integer(nrow(filter(curr_mars, grepl("Let's go pressed", curr_mars$V6, fixed = TRUE))) != 0)
          df$first_resp_p[r] <- ifelse(showall == 1 | letsgo == 1, 1, 0)
          
          
          ##updated completion status (consider quitting during meditate and joy)
          
          if(nrow(filter(curr_mars, grepl('like_page', curr_mars$V4, fixed = TRUE) & 
                         !grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE) &
                         !grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE))) != 0){
            df$activ_done_p[r] <- 1
            
            surf_time <- FALSE
            notice_time <- FALSE
            imagine_time <- FALSE
            surf_time_avg <- FALSE
            notice_time_avg <- FALSE
            imagine_time_avg <- FALSE
            
            if(nrow(filter(curr_mars, grepl('moodsurf_6_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              surf_end <- which(curr_mars$V4 == 'moodsurf_6_like_page')
              surf_end_time <- curr_mars$V1[surf_end[length(surf_end)]]
              surf_start <- which(curr_mars$V4 == 'moodsurf_load_page')
              surf_start_time <- curr_mars$V1[surf_start[1]]
              surf_time <- ((surf_end_time - surf_start_time) / 1000) > (168*60/340)
              surf_time_avg <- ((surf_end_time - surf_start_time) / 1000) > (168*60/240)
            }
            
            if(nrow(filter(curr_mars, grepl('notice_9_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              notice_end <- which(curr_mars$V4 == 'notice_9_like_page')
              notice_end_time <- curr_mars$V1[notice_end[length(notice_end)]]
              notice_start <- which(curr_mars$V4 == 'notice_load_page')
              notice_start_time <- curr_mars$V1[notice_start[1]]
              notice_time <- ((notice_end_time - notice_start_time) / 1000) > (255*60/340)
              notice_time_avg <- ((notice_end_time - notice_start_time) / 1000) > (255*60/240)
            }
            
            if(nrow(filter(curr_mars, grepl('imagine_8_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              imagine_end <- which(curr_mars$V4 == 'imagine_8_like_page')
              imagine_end_time <- curr_mars$V1[imagine_end[length(imagine_end)]]
              imagine_start <- which(curr_mars$V4 == 'imagine_load_page')
              imagine_start_time <- curr_mars$V1[imagine_start[1]]
              imagine_time <- ((imagine_end_time - imagine_start_time) / 1000) > (183*60/340)
              imagine_time_avg <- ((imagine_end_time - imagine_start_time) / 1000) > (183*60/240)
            }
            
            df$activ_done_wtime_p[r] <- (surf_time | notice_time | imagine_time)
            df$activ_done_wavgtime_p[r] <- (surf_time_avg | notice_time_avg | imagine_time_avg)
            
          } else{
            
            
            ##if meditate or joy, further processing
            meditate_end <- nrow(filter(curr_mars, grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE)))
            quit_meditate <- nrow(filter(curr_mars, grepl('quit_from_meditation_exercise_yes', curr_mars$V5, fixed = TRUE)))
            meditate_done <- (meditate_end > 0) & (quit_meditate == 0)
            
            joy_end <- nrow(filter(curr_mars, grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE)))
            quit_joy <- nrow(filter(curr_mars, grepl('quit_from_breath_exercise_yes', curr_mars$V5, fixed = TRUE)))
            joy_more_time <- nrow(filter(curr_mars, grepl('want more time', curr_mars$V5, fixed = TRUE) &
                                           grepl('yes pressed', curr_mars$V6, fixed = TRUE)))
            joy_done <- ((joy_end > 0) & (quit_joy == 0)) | ((joy_end > 0) & (joy_more_time > 0))
            
            df$activ_done_p[r] <- as.integer(meditate_done | joy_done)
            
            
            meditate_time <- FALSE
            joy_time <- FALSE
            meditate_time_avg <- FALSE
            joy_time_avg <- FALSE
            
            if(nrow(filter(curr_mars, grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              meditate_end <- which(curr_mars$V4 == 'meditate_3_like_page')
              meditate_end_time <- curr_mars$V1[meditate_end[length(meditate_end)]]
              meditate_start <- which(curr_mars$V4 == 'meditate_load_page')
              meditate_start_time <- curr_mars$V1[meditate_start[1]]
              meditate_time <- ((meditate_end_time - meditate_start_time) / 1000) > (168 + 32*60/340)
              meditate_time_avg <- ((meditate_end_time - meditate_start_time) / 1000) > (168 + 32*60/240)
            }
            
            if(nrow(filter(curr_mars, grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE))) != 0){
              joy_end <- which(curr_mars$V4 == 'joy_8_like_page')
              joy_end_time <- curr_mars$V1[joy_end[length(joy_end)]]
              joy_start <- which(curr_mars$V4 == 'joy_load_page')
              joy_start_time <- curr_mars$V1[joy_start[1]]
              joy_time <- ((joy_end_time - joy_start_time) / 1000) > (60 + 144*60/340)
              joy_time_avg <- ((joy_end_time - joy_start_time) / 1000) > (60 + 144*60/240)
            }
            
            df$activ_done_wtime_p[r] <- (meditate_time | joy_time)
            df$activ_done_wavgtime_p[r] <- (meditate_time_avg | joy_time_avg)
            
          }
          
          num_activ <- curr_mars %>% 
            filter(grepl('like_page', .$V4, fixed = TRUE)) %>% 
            distinct(V4) %>% 
            nrow(.)
          df$activ_done_m_p[r] <- as.integer(num_activ > 1 & df$activ_done_p[r] == 1)
          time_list <- diff(curr_mars$V1)
          time_list <- time_list[time_list < 600000]
          df$time_spent_p[r] <- sum(time_list, na.rm = TRUE)
        }
      }
    }
  }
  
  return(df)
}

curate_mars_int_preblock <- function(df, id){
  #count the num of state in V3
  df$num_page_preblock <- rep(NA, nrow(df))
  #count the num of selected or pressed in V6
  df$num_click_preblock <- rep(NA, nrow(df))
  #whether they got in to the MARS app
  df$in_mars_preblock <- rep(NA, nrow(df))
  #whether they started an activity (has anything other than main_log or menu in V4)
  df$activ_started_preblock <- rep(NA, nrow(df))
  #whether they finished an activity (has like_page in V4)
  df$activ_done_preblock <- rep(NA, nrow(df))
  #whether they finished multiple activities
  df$activ_done_m_preblock <- rep(NA, nrow(df))
  #time spent in the app
  df$time_spent_preblock <- rep(NA, nrow(df))
  
  start_t <- as.numeric(int_start(df$block_bounds_mountain[1]))
  end_t <- as.numeric(int_end(df$block_bounds_mountain[nrow(df)]))
  
  fname_mars_log <- str_c(path_raw_data, id, 'LOG--org.md2k.mars.csv.bz2',
                          sep = "/")
  if(file.exists(fname_mars_log)){
    mars_log <- read.csv(fname_mars_log, header = FALSE,
                         col.names = paste0("V", c(1, 2, 3, 4, 5, 6)))
    mars_log_s <- mars_log %>% 
      filter(V1 / 1000 >= start_t & 
               V1 / 1000 <= end_t) %>% 
      distinct(.keep_all = TRUE)
    
    if(nrow(mars_log_s) > 0){
      for (r in 1:nrow(df)){
        if(is.na(df$ts_coinflip_mountain[r])){
          
          next
          
        } else if(r == 1){
          
          df$in_mars_preblock[r] <- 0
          df$num_page_preblock[r] <- 0
          df$num_click_preblock[r] <- 0
          df$activ_started_preblock[r] <- 0
          df$activ_done_preblock[r] <- 0
          df$activ_done_m_preblock[r] <- 0
          df$time_spent_preblock[r] <- 0
          
          next
          
        } else if(is.na(df$ts_coinflip_mountain[r - 1])){
          
          df$in_mars_preblock[r] <- 0
          df$num_page_preblock[r] <- 0
          df$num_click_preblock[r] <- 0
          df$activ_started_preblock[r] <- 0
          df$activ_done_preblock[r] <- 0
          df$activ_done_m_preblock[r] <- 0
          df$time_spent_preblock[r] <- 0
          
          next
          
        } else{
          
          curr_mars <- mars_log_s %>% 
            filter(mars_log_s$V1 / 1000 >= as.numeric(df$ts_coinflip_mountain[r - 1]) & 
                     mars_log_s$V1 / 1000 <=  as.numeric(df$ts_coinflip_mountain[r]))
        }
        
        if(nrow(curr_mars) == 0){
          
          df$in_mars_preblock[r] <- 0
          df$num_page_preblock[r] <- 0
          df$num_click_preblock[r] <- 0
          df$activ_started_preblock[r] <- 0
          df$activ_done_preblock[r] <- 0
          df$activ_done_m_preblock[r] <- 0
          df$time_spent_preblock[r] <- 0
          
        } else{
          df$in_mars_preblock[r] <- 1
          df$num_page_preblock[r] <- curr_mars %>% 
            filter(V3 == "state") %>% 
            nrow(.)
          df$num_click_preblock[r] <- curr_mars %>% 
            filter(grepl('selected', .$V6, fixed = TRUE) |
                     grepl('pressed', .$V6, fixed = TRUE) |
                     grepl('play', .$V6, fixed = TRUE)) %>% 
            nrow(.)
          df$activ_started_preblock[r] <- as.integer(nrow(filter(curr_mars, !grepl('main_logo', curr_mars$V4, fixed = TRUE) & 
                                                          !grepl('menu', curr_mars$V4, fixed = TRUE))) != 0)
          
          ##updated completion status (consider quitting during meditate and joy)
          
          #simple decision if not meditate or joy; just find like_page
          if(nrow(filter(curr_mars, grepl('like_page', curr_mars$V4, fixed = TRUE) & 
                         !grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE) &
                         !grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE))) != 0){
            df$activ_done_preblock[r] <- 1
          } else{
            ##if meditate or joy, further processing
            #check meditate first
            meditate_end <- nrow(filter(curr_mars, grepl('meditate_3_like_page', curr_mars$V4, fixed = TRUE)))
            quit_meditate <- nrow(filter(curr_mars, grepl('quit_from_meditation_exercise_yes', curr_mars$V5, fixed = TRUE)))
            meditate_done <- (meditate_end > 0) & (quit_meditate == 0)
            
            joy_end <- nrow(filter(curr_mars, grepl('joy_8_like_page', curr_mars$V4, fixed = TRUE)))
            quit_joy <- nrow(filter(curr_mars, grepl('quit_from_breath_exercise_yes', curr_mars$V5, fixed = TRUE)))
            joy_more_time <- nrow(filter(curr_mars, grepl('want more time', curr_mars$V5, fixed = TRUE) &
                                           grepl('yes pressed', curr_mars$V6, fixed = TRUE)))
            joy_done <- ((joy_end > 0) & (quit_joy == 0)) | ((joy_end > 0) & (joy_more_time > 0))
            
            df$activ_done_preblock[r] <- as.integer(meditate_done | joy_done)
          }
          #df$activ_done[r] <- as.integer(nrow(filter(curr_mars, grepl('like_page', curr_mars$V4, fixed = TRUE))) != 0)
          
          
          
          num_activ <- curr_mars %>% 
            filter(grepl('like_page', .$V4, fixed = TRUE)) %>% 
            distinct(V4) %>% 
            nrow(.)
          df$activ_done_m_preblock[r] <- as.integer(num_activ > 1 & df$activ_done_preblock[r] == 1)
          time_list <- diff(curr_mars$V1)
          time_list <- time_list[time_list < 600000]
          df$time_spent_preblock[r] <- sum(time_list, na.rm = TRUE)
          
        } 
      }
    }
  }
  
  return(df)
}



curate_tips_int <- function(df, id){
  #count the num of messages viewed in V3
  df$num_message <- rep(NA, nrow(df))
  #whether they got in to the quit tips (open by user or scheduler in V3)
  df$in_tips <- rep(NA, nrow(df))
  #whether they got to at least one tip
  df$read_tips <- rep(NA, nrow(df))
  #whether they got to multiple tips
  df$read_tips_m <- rep(NA, nrow(df))
  #time spent in the app
  df$time_spent_tips <- rep(NA, nrow(df))
  
  df$tips_comp_time <- rep(NA, nrow(df))
  df$tips_comp_wps <- rep(NA, nrow(df))
  
  start_t <- as.numeric(int_start(df$block_bounds_mountain[1]))
  end_t <- as.numeric(int_end(df$block_bounds_mountain[nrow(df)]))
  
  fname_tips_log <- str_c(path_raw_data, id, 'INTERVENTION_LOG--org.md2k.loweffortintervention.csv.bz2',
                          sep = "/")
  if(file.exists(fname_tips_log)){
    tips_log <- read.csv(fname_tips_log, header = FALSE,
                         col.names = paste0("V", c(1, 2, 3, 4, 5, 6, 7)))
    tips_log_s <- tips_log %>% 
      filter(V1 / 1000 >= start_t & 
               V1 / 1000 <= end_t) %>% 
      distinct(.keep_all = TRUE)
    
    if(nrow(tips_log_s) > 0){
      for (r in 1:nrow(df)){
        if(is.na(df$ts_coinflip_mountain[r])){
          curr_tips <- tips_log_s %>% 
            filter(tips_log_s$V1 / 1000 >= as.numeric(int_start(df$block_bounds_mountain[r])) & 
                     tips_log_s$V1 / 1000 <= (as.numeric(int_start(df$block_bounds_mountain[r])) + 3600))
        } else{
          curr_tips <- tips_log_s %>% 
            filter(tips_log_s$V1 / 1000 >= as.numeric(df$ts_coinflip_mountain[r]) & 
                     tips_log_s$V1 / 1000 <= min(as.numeric(int_end(df$block_bounds_mountain[r])), 
                                                 as.numeric(df$ts_coinflip_mountain[r]) + 3600))
        }
        
        
        if(nrow(curr_tips) == 0){
          df$in_tips[r] <- 0
          df$num_message[r] <- 0
          df$read_tips[r] <- 0
          df$read_tips_m[r] <- 0
          df$time_spent_tips[r] <- 0
        } else{
          curr_tips[is.na(curr_tips)] <- ""
          
          df$in_tips[r] <- as.integer(nrow(filter(curr_tips, grepl('Open byuser', curr_tips$V3, fixed = TRUE) |
                                                    grepl('Open byscheduler', curr_tips$V3, fixed = TRUE))) != 0)
          num_message <- nrow(filter(curr_tips, grepl('Message shown', curr_tips$V3, fixed = TRUE)))
          df$num_message[r] <- num_message
          
          closed <- as.integer(nrow(filter(curr_tips, grepl('button clicked', curr_tips$V3, fixed = TRUE))) != 0)
          
          if(closed == 1 & df$in_tips[r] == 1){
            df$read_tips[r] <- as.integer(num_message > 0)
            df$read_tips_m[r] <- as.integer(num_message > 1)
            
            time <- c()
            wps <- c()
            for (i in 1:nrow(curr_tips)){
              if (i == nrow(curr_tips)){
                next
              }
              if(str_detect(curr_tips$V3[i], "Message shown") & 
                 str_detect(curr_tips$V3[i + 1], "button clicked")){
                
                duration <- (curr_tips$V1[i + 1] - curr_tips$V1[i]) / 1000
                if ((duration == 0) & (i < nrow(curr_tips) - 1)){
                  duration <- (curr_tips$V1[i + 2] - curr_tips$V1[i]) / 1000
                }
                
                message_1 <- str_split(curr_tips$V3[i], "=")[[1]][5]
                full_mes <- str_c(message_1, curr_tips$V4[i], curr_tips$V5[i], 
                                  curr_tips$V6[i], curr_tips$V7[i])
                word_count <- str_count(full_mes, '\\w+')
                
                if (duration == 0){
                  curr_wps <- 0
                } else {
                  curr_wps <- word_count / duration
                }
                
                time <- c(time, duration)
                wps <- c(wps, curr_wps)
              }
            }
            
            df$tips_comp_time[r] <- max(time)
            df$tips_comp_wps[r] <- min(wps)
            
            
          } else{
            df$read_tips[r] <- 0
            df$read_tips_m[r] <- 0
          }
          
          
          time_list <- diff(curr_tips$V1)
          time_list <- time_list[time_list < 600000]
          df$time_spent_tips[r] <- sum(time_list, na.rm = TRUE)
        } 
      }
    }
  }
  
  return(df)
}

curate_tips_int_p <- function(df, id){
  #count the num of messages viewed in V3
  df$num_message_p <- rep(NA, nrow(df))
  #whether they got in to the quit tips (open by user or scheduler in V3)
  df$in_tips_p <- rep(NA, nrow(df))
  #whether they got to at least one tip
  df$read_tips_p <- rep(NA, nrow(df))
  #whether they got to multiple tips
  df$read_tips_m_p <- rep(NA, nrow(df))
  #time spent in the app
  df$time_spent_tips_p <- rep(NA, nrow(df))
  
  df$tips_comp_time_p <- rep(NA, nrow(df))
  df$tips_comp_wps_p <- rep(NA, nrow(df))
  
  start_t <- as.numeric(int_start(df$block_bounds_mountain[1]))
  end_t <- as.numeric(int_end(df$block_bounds_mountain[nrow(df)]))
  
  fname_tips_log <- str_c(path_raw_data, id, 'INTERVENTION_LOG--org.md2k.loweffortintervention.csv.bz2',
                          sep = "/")
  if(file.exists(fname_tips_log)){
    tips_log <- read.csv(fname_tips_log, header = FALSE,
                         col.names = paste0("V", c(1, 2, 3, 4, 5, 6, 7)))
    tips_log_s <- tips_log %>% 
      filter(V1 / 1000 >= start_t & 
               V1 / 1000 <= end_t) %>% 
      distinct(.keep_all = TRUE)
    
    if(nrow(tips_log_s) > 0){
      for (r in 1:nrow(df)){
        if(is.na(df$ts_coinflip_mountain[r])){
          
          df$in_tips_p[r] <- 0
          df$num_message_p[r] <- 0
          df$read_tips_p[r] <- 0
          df$read_tips_m_p[r] <- 0
          df$time_spent_tips_p[r] <- 0
          
        } else if (df$A[r] != 'low_effort' | df$emi_resp[r] != 'Ok'){
          
          df$in_tips_p[r] <- 0
          df$num_message_p[r] <- 0
          df$read_tips_p[r] <- 0
          df$read_tips_m_p[r] <- 0
          df$time_spent_tips_p[r] <- 0
          
        } else {
          
          curr_tips <- tips_log_s %>% 
            filter(tips_log_s$V1 / 1000 >= as.numeric(df$ts_coinflip_mountain[r]) & 
                     tips_log_s$V1 / 1000 <= min(as.numeric(int_end(df$block_bounds_mountain[r])), 
                                                 as.numeric(df$ts_coinflip_mountain[r]) + 3600)) %>% 
            filter(grepl('Open byscheduler', .$V3, fixed = TRUE))
          
          if(nrow(curr_tips) == 0){
            df$in_tips_p[r] <- 0
            df$num_message_p[r] <- 0
            df$read_tips_p[r] <- 0
            df$read_tips_m_p[r] <- 0
            df$time_spent_tips_p[r] <- 0
          } else{
            curr_tips[is.na(curr_tips)] <- ""
            
            df$in_tips_p[r] <- 1
            num_message <- nrow(filter(curr_tips, grepl('Message shown', curr_tips$V3, fixed = TRUE)))
            df$num_message_p[r] <- num_message
            
            closed <- as.integer(nrow(filter(curr_tips, grepl('button clicked', curr_tips$V3, fixed = TRUE))) != 0)
            
            if(closed == 1 & df$in_tips_p[r] == 1){
              df$read_tips_p[r] <- as.integer(num_message > 0)
              df$read_tips_m_p[r] <- as.integer(num_message > 1)
              
              time <- c()
              wps <- c()
              for (i in 1:nrow(curr_tips)){
                if (i == nrow(curr_tips)){
                  next
                }
                if(str_detect(curr_tips$V3[i], "Message shown") & 
                   str_detect(curr_tips$V3[i + 1], "button clicked")){
                  
                  duration <- (curr_tips$V1[i + 1] - curr_tips$V1[i]) / 1000
                  if ((duration == 0) & (i < nrow(curr_tips) - 1)){
                    duration <- (curr_tips$V1[i + 2] - curr_tips$V1[i]) / 1000
                  }
                  
                  message_1 <- str_split(curr_tips$V3[i], "=")[[1]][5]
                  full_mes <- str_c(message_1, curr_tips$V4[i], curr_tips$V5[i], 
                                    curr_tips$V6[i], curr_tips$V7[i])
                  word_count <- str_count(full_mes, '\\w+')
                  
                  if (duration == 0){
                    curr_wps <- 0
                  } else {
                    curr_wps <- word_count / duration
                  }
                  
                  time <- c(time, duration)
                  wps <- c(wps, curr_wps)
                }
              }
              
              df$tips_comp_time_p[r] <- max(time)
              df$tips_comp_wps_p[r] <- min(wps)
              
              
            } else{
              df$read_tips_p[r] <- 0
              df$read_tips_m_p[r] <- 0
            }
            
            
            time_list <- diff(curr_tips$V1)
            time_list <- time_list[time_list < 600000]
            df$time_spent_tips_p[r] <- sum(time_list, na.rm = TRUE)
          }
        }
      }
    }
  }
  
  return(df)
}

curate_tips_int_preblock <- function(df, id){
  #count the num of messages viewed in V3
  df$num_message_preblock <- rep(NA, nrow(df))
  #whether they got in to the quit tips (open by user or scheduler in V3)
  df$in_tips_preblock <- rep(NA, nrow(df))
  #whether they got to at least one tip
  df$read_tips_preblock <- rep(NA, nrow(df))
  #whether they got to multiple tips
  df$read_tips_m_preblock <- rep(NA, nrow(df))
  #time spent in the app
  df$time_spent_tips_preblock <- rep(NA, nrow(df))
  
  df$tips_comp_time_preblock <- rep(NA, nrow(df))
  df$tips_comp_wps_preblock <- rep(NA, nrow(df))
  
  start_t <- as.numeric(int_start(df$block_bounds_mountain[1]))
  end_t <- as.numeric(int_end(df$block_bounds_mountain[nrow(df)]))
  
  fname_tips_log <- str_c(path_raw_data, id, 'INTERVENTION_LOG--org.md2k.loweffortintervention.csv.bz2',
                          sep = "/")
  if(file.exists(fname_tips_log)){
    tips_log <- read.csv(fname_tips_log, header = FALSE,
                         col.names = paste0("V", c(1, 2, 3, 4, 5, 6, 7)))
    tips_log_s <- tips_log %>% 
      filter(V1 / 1000 >= start_t & 
               V1 / 1000 <= end_t) %>% 
      distinct(.keep_all = TRUE)
    
    if(nrow(tips_log_s) > 0){
      for (r in 1:nrow(df)){
        if(is.na(df$ts_coinflip_mountain[r])){
          
          next
          
        } else if(r == 1){
          
          df$in_tips_preblock[r] <- 0
          df$num_message_preblock[r] <- 0
          df$read_tips_preblock[r] <- 0
          df$read_tips_m_preblock[r] <- 0
          df$time_spent_tips_preblock[r] <- 0
          
          next
          
        } else if(is.na(df$ts_coinflip_mountain[r - 1])){
          
          df$in_tips_preblock[r] <- 0
          df$num_message_preblock[r] <- 0
          df$read_tips_preblock[r] <- 0
          df$read_tips_m_preblock[r] <- 0
          df$time_spent_tips_preblock[r] <- 0
          
          next
          
        } else{
          
          curr_tips <- tips_log_s %>% 
            filter(tips_log_s$V1 / 1000 >= as.numeric(df$ts_coinflip_mountain[r - 1]) & 
                     tips_log_s$V1 / 1000 <= as.numeric(df$ts_coinflip_mountain[r]))
        }
        
        
        if(nrow(curr_tips) == 0){
          
          df$in_tips_preblock[r] <- 0
          df$num_message_preblock[r] <- 0
          df$read_tips_preblock[r] <- 0
          df$read_tips_m_preblock[r] <- 0
          df$time_spent_tips_preblock[r] <- 0
          
        } else{
          curr_tips[is.na(curr_tips)] <- ""
          
          df$in_tips_preblock[r] <- as.integer(nrow(filter(curr_tips, grepl('Open byuser', curr_tips$V3, fixed = TRUE) |
                                                    grepl('Open byscheduler', curr_tips$V3, fixed = TRUE))) != 0)
          num_message <- nrow(filter(curr_tips, grepl('Message shown', curr_tips$V3, fixed = TRUE)))
          df$num_message_preblock[r] <- num_message
          
          closed <- as.integer(nrow(filter(curr_tips, grepl('button clicked', curr_tips$V3, fixed = TRUE))) != 0)
          
          if(closed == 1 & df$in_tips_preblock[r] == 1){
            df$read_tips_preblock[r] <- as.integer(num_message > 0)
            df$read_tips_m_preblock[r] <- as.integer(num_message > 1)
            
            time <- c()
            wps <- c()
            for (i in 1:nrow(curr_tips)){
              if (i == nrow(curr_tips)){
                next
              }
              if(str_detect(curr_tips$V3[i], "Message shown") & 
                 str_detect(curr_tips$V3[i + 1], "button clicked")){
                
                duration <- (curr_tips$V1[i + 1] - curr_tips$V1[i]) / 1000
                if ((duration == 0) & (i < nrow(curr_tips) - 1)){
                  duration <- (curr_tips$V1[i + 2] - curr_tips$V1[i]) / 1000
                }
                
                message_1 <- str_split(curr_tips$V3[i], "=")[[1]][5]
                full_mes <- str_c(message_1, curr_tips$V4[i], curr_tips$V5[i], 
                                  curr_tips$V6[i], curr_tips$V7[i])
                word_count <- str_count(full_mes, '\\w+')
                
                if (duration == 0){
                  curr_wps <- 0
                } else {
                  curr_wps <- word_count / duration
                }
                
                time <- c(time, duration)
                wps <- c(wps, curr_wps)
              }
            }
            
            df$tips_comp_time_preblock[r] <- max(time)
            df$tips_comp_wps_preblock[r] <- min(wps)
            
            
          } else{
            df$read_tips_preblock[r] <- 0
            df$read_tips_m_preblock[r] <- 0
          }
          
          
          time_list <- diff(curr_tips$V1)
          time_list <- time_list[time_list < 600000]
          df$time_spent_tips_preblock[r] <- sum(time_list, na.rm = TRUE)
        } 
      }
    }
  }
  
  return(df)
}


# test_df <- dat_app_usage %>% 
#   filter(mars_id == 'mars_88')
# 
# test_df_mars <- curate_mars_int(test_df, 'mars_14')
# test_df_mars_p <- curate_mars_int_p(test_df_mars, 'mars_14')
# 
# test_df_tips <- curate_tips_int(test_df, 'mars_88')
# test_df_tips_p <- curate_tips_int_p(test_df_tips, 'mars_88')


dat_app_usage <- dat_mars_for_nonema_assessed_proximal_outcome %>% 
  select(mars_id, olson, cluster_id, decision_point, block_number, block_bounds_mountain,
         ts_coinflip_mountain, A, ts_emi_resp, emi_resp) %>% 
  filter(!is.na(block_bounds_mountain))

dat_app_usage$emi_resp <- ifelse(dat_app_usage$A %in% c('low_effort', 'mars') & is.na(dat_app_usage$emi_resp), 
                                 "maybe_missed", dat_app_usage$emi_resp)


id_list <- unique(dat_app_usage$mars_id)

#path_raw_data <- './'


columns <- c("mars_id", "olson", "cluster_id", "decision_point", "block_number",
             "block_bounds_mountain", "ts_coinflip_mountain", "A",
             "ts_emi_resp", "emi_resp", "num_page", "num_click",
             "in_mars", "activ_started", "activ_done", "activ_done_m", 
             "activ_done_wtime", "activ_done_wavgtime", "time_spent",
             "num_page_p", "num_click_p", "in_mars_p", "activ_started_p", 
             "first_resp_p", "activ_done_p",
             "activ_done_m_p", "activ_done_wtime_p", "activ_done_wavgtime_p", "time_spent_p",
             "num_page_preblock", "num_click_preblock", "in_mars_preblock", 
             "activ_started_preblock", "activ_done_preblock",
             "activ_done_m_preblock", "time_spent_preblock",
             "num_message", "in_tips", "read_tips", "read_tips_m",
             "time_spent_tips", "tips_comp_time", "tips_comp_wps",
             "num_message_p", "in_tips_p", "read_tips_p", "read_tips_m_p",
             "time_spent_tips_p", "tips_comp_time_p", "tips_comp_wps_p",
             "num_message_preblock", "in_tips_preblock", "read_tips_preblock", "read_tips_m_preblock",
             "time_spent_tips_preblock", "tips_comp_time_preblock", "tips_comp_wps_preblock")

curated_w_app_usage <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(curated_w_app_usage) <- columns

for (s in 1:length(id_list)){
  print(id_list[s])
  curr_df <- dat_app_usage %>% 
    filter(mars_id == id_list[s])
  curr_df_mars <- curate_mars_int(curr_df, id_list[s])
  curr_df_mars_p <- curate_mars_int_p(curr_df_mars, id_list[s])
  curr_df_mars_preblock <- curate_mars_int_preblock(curr_df_mars_p, id_list[s])
  curr_df_tips <- curate_tips_int(curr_df_mars_preblock, id_list[s])
  curr_df_tips_p <- curate_tips_int_p(curr_df_tips, id_list[s])
  curr_df_tips_preblock <- curate_tips_int_preblock(curr_df_tips_p, id_list[s])
  curated_w_app_usage <- rbind(curated_w_app_usage, curr_df_tips_preblock)
}

curated_w_app_usage_v1 <- curated_w_app_usage %>% 
  select(-c(block_bounds_mountain:ts_coinflip_mountain))

setwd('D:\\MARS_data/')
saveRDS(curated_w_app_usage_v1, file = 'app_usage_for_mainpipeline_updated0130.Rds')


curated_w_app_usage_to_merge <- curated_w_app_usage %>% 
  select(mars_id, decision_point, num_page:tips_comp_wps_preblock)

dat_mars_app_engage <- dat_mars_for_nonema_assessed_proximal_outcome %>% 
  left_join(curated_w_app_usage_to_merge, by = c('mars_id', 'decision_point'))


dat_mars_app_engage$read_tips_cutoff <- ifelse(dat_mars_app_engage$read_tips == 1 & dat_mars_app_engage$tips_comp_wps > 4, 
                                           0, dat_mars_app_engage$read_tips)
dat_mars_app_engage$read_tips_cutoff_p <- ifelse(dat_mars_app_engage$read_tips_p == 1 & dat_mars_app_engage$tips_comp_wps_p > 4, 
                                             0, dat_mars_app_engage$read_tips_p)
dat_mars_app_engage$read_tips_cutoff2 <- ifelse(dat_mars_app_engage$read_tips == 1 & dat_mars_app_engage$tips_comp_wps > (340/60), 
                                               0, dat_mars_app_engage$read_tips)
dat_mars_app_engage$read_tips_cutoff2_p <- ifelse(dat_mars_app_engage$read_tips_p == 1 & dat_mars_app_engage$tips_comp_wps_p > (340/60), 
                                                 0, dat_mars_app_engage$read_tips_p)

dat_mars_app_engage$activ_done_cutoff <- ifelse(dat_mars_app_engage$activ_done == 1 & dat_mars_app_engage$activ_done_wtime == FALSE,
                                                0, dat_mars_app_engage$activ_done)
dat_mars_app_engage$activ_done_cutoff_p <- ifelse(dat_mars_app_engage$activ_done_p == 1 & dat_mars_app_engage$activ_done_wtime_p == FALSE,
                                                0, dat_mars_app_engage$activ_done_p)
dat_mars_app_engage$activ_done_avgcutoff <- ifelse(dat_mars_app_engage$activ_done == 1 & dat_mars_app_engage$activ_done_wavgtime == FALSE,
                                                0, dat_mars_app_engage$activ_done)
dat_mars_app_engage$activ_done_avgcutoff_p <- ifelse(dat_mars_app_engage$activ_done_p == 1 & dat_mars_app_engage$activ_done_wavgtime_p == FALSE,
                                                  0, dat_mars_app_engage$activ_done_p)

# df_exam <- dat_mars_app_engage %>% 
#   select(mars_id, ts_coinflip_mountain, ts_emi_resp, activ_done, activ_done_wtime,
#          activ_done_p, activ_done_wtime_p, activ_done_cutoff, activ_done_cutoff_p)


dat_mars_app_engage[dat_mars_app_engage$mars_id == 'mars_15' & !is.na(dat_mars_app_engage$A), 
                  c("num_page", "num_click", "in_mars", "activ_started", "activ_done", "activ_done_m", "time_spent" ,
                    "num_page_p", "num_click_p", "in_mars_p", "activ_started_p", "first_resp_p", "activ_done_p", "activ_done_m_p", "time_spent_p")] <- 0

dat_mars_app_engage$sum_num_activ <- dat_mars_app_engage$num_click + dat_mars_app_engage$num_message
dat_mars_app_engage$sum_time <- dat_mars_app_engage$time_spent + dat_mars_app_engage$time_spent_tips
dat_mars_app_engage$strat_done <- as.integer((dat_mars_app_engage$activ_done + dat_mars_app_engage$read_tips) > 0)
dat_mars_app_engage$strat_done_wcutoff <- as.integer((dat_mars_app_engage$activ_done + dat_mars_app_engage$read_tips_cutoff) > 0)
dat_mars_app_engage$strat_done_m <- as.integer((dat_mars_app_engage$activ_done_m + 
                                                dat_mars_app_engage$read_tips_m) > 0)
dat_mars_app_engage$strat_done_wcutoff2 <- as.integer((dat_mars_app_engage$activ_done_cutoff + dat_mars_app_engage$read_tips_cutoff2) > 0)
dat_mars_app_engage$strat_done_wcutoff3 <- as.integer((dat_mars_app_engage$activ_done + dat_mars_app_engage$read_tips_cutoff2) > 0)



dat_mars_app_engage$strat_done_p <- as.integer((dat_mars_app_engage$activ_done_p + dat_mars_app_engage$read_tips_p) > 0)
dat_mars_app_engage$strat_done_wcutoff_p <- as.integer((dat_mars_app_engage$activ_done_p + dat_mars_app_engage$read_tips_cutoff_p) > 0)
dat_mars_app_engage$strat_done_wcutoff2_p <- as.integer((dat_mars_app_engage$activ_done_cutoff_p + dat_mars_app_engage$read_tips_cutoff2_p) > 0)


dat_mars_app_engage$in_app <- as.numeric(dat_mars_app_engage$in_mars + dat_mars_app_engage$in_tips > 0)

setwd('/')
saveRDS(dat_mars_app_engage, file = 'xxx.Rds')
