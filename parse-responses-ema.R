# -----------------------------------------------------------------------------
# PART 1: The metalogic of this script is to format the MD2K file to be 
# compatible with what tidyjson::spread_all can recognize.
# -----------------------------------------------------------------------------

source("paths.R")
library(tidyverse)
library(tidyjson)

list_ids <- readRDS(file = file.path(path_manipulated_data, "list_ids.rds"))

list_dat_ema_responses <- list()

for(i in 1:length(list_ids)){
  is_here <- file.exists(file.path(path_raw_data, list_ids[[i]], "EMA_RANDOM--DATA--org.md2k.ema.csv.bz2"))
  if(is_here){
    # Note: I employ a trick here by specifying sep = "&"
    dat_ema_responses <- read.csv(file.path(path_raw_data, list_ids[[i]], "EMA_RANDOM--DATA--org.md2k.ema.csv.bz2"), header = FALSE, sep = "&")
    dat_ema_responses[["mars_id"]] <- list_ids[[i]]
    list_dat_ema_responses <- append(list_dat_ema_responses, list(dat_ema_responses))
  }
}

begin_profile <- Sys.time()

list_collect <- list()

for(i in 1:length(list_dat_ema_responses)){
  dat_ema_responses <- list_dat_ema_responses[[i]]
  list_parsed <- list()
  
  for(rr in 1:nrow(dat_ema_responses)){
    curr_row <- dat_ema_responses[rr,"V1"]
    
    pattern <- "\\{(.*)\\}"
    matches <- gregexpr(pattern, curr_row)
    curr_json <- regmatches(curr_row, matches)
    curr_json <- unlist(curr_json)
    
    pattern <- "\\[(.*)\\]"
    matches <- gregexpr(pattern, curr_json)
    curr_unformatted_response_data <- regmatches(curr_json, matches, invert = FALSE)
    curr_unformatted_response_data <- unlist(curr_unformatted_response_data)
    curr_unformatted_response_data <- substring(curr_unformatted_response_data, first = 2, last = nchar(curr_unformatted_response_data)-1)
    curr_unformatted_response_data <- paste(",", curr_unformatted_response_data, sep = "")
    curr_unformatted_response_data <- strsplit(curr_unformatted_response_data, split = ",{finish_time:", fixed = TRUE)
    curr_unformatted_response_data <- curr_unformatted_response_data[[1]]
    curr_unformatted_response_data <- curr_unformatted_response_data[-1]
    curr_unformatted_response_data <- lapply(curr_unformatted_response_data, 
                                             function(x){
                                               x <- paste("{finish_time:", x, sep = "")
                                               return(x)
                                             })
    
    list_json_converted <- list()
    
    for(j in 1:length(curr_unformatted_response_data)){
      curr_item <- curr_unformatted_response_data[[j]]
      
      if(grepl(pattern = "Thank you for answering this Survey", x = curr_item, fixed = TRUE)){
        next
      }else if(grepl(pattern = "{condition", x = curr_item, fixed = TRUE)){
        curr_many_items <- strsplit(curr_item, split = "{condition:", fixed = TRUE) 
        curr_many_items <- curr_many_items[[1]]
        curr_many_items[1] <- substring(text = curr_many_items[1], first = 1, last = nchar(curr_many_items[1])-1)
        curr_many_items[2:length(curr_many_items)] <- paste("{condition:", curr_many_items[2:length(curr_many_items)], sep="")
        
        if(length(curr_many_items)>2){
          curr_many_items[2:(length(curr_many_items)-1)] <- substring(text = curr_many_items[2:(length(curr_many_items)-1)], first = 1, last = nchar(curr_many_items[2:(length(curr_many_items)-1)])-1)
        }
        
        for(k in 1:length(curr_many_items)){
          curr_item <- curr_many_items[k]
          pattern <- "question\\_text:.*?,question\\_type"
          matches <- gregexpr(pattern, curr_item)
          curr_question_text <- regmatches(curr_item, matches, invert = FALSE)
          curr_question_text <- curr_question_text[[1]]
          curr_question_text <- substring(text = curr_question_text, first = 15, last = nchar(curr_question_text)-14)
          curr_question_text <- paste("\"", curr_question_text, "\"", sep = "")
          
          curr_item <- gsub(pattern = "finish\\_time:", 
                            replacement = "\"finish\\_time\":",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "prompt\\_time:", 
                            replacement = "\"prompt\\_time\":",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "question\\_id:", 
                            replacement = "\"question\\_id\":",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "question\\_text:.*?,question\\_type", 
                            replacement = "question\\_text:marianatrench,question\\_type",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "question\\_text:", 
                            replacement = "\"question\\_text\":",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "marianatrench", 
                            replacement = curr_question_text,
                            x = curr_item)
          
          curr_item <- gsub(pattern = "question\\_type:multiple\\_choice", 
                            replacement = "\"question\\_type\":\"multiple\\_choice\"",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "question\\_type:multiple\\_select", 
                            replacement = "\"question\\_type\":\"multiple\\_select\"",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "question\\_type:string\\_picker", 
                            replacement = "\"question\\_type\":\"string\\_picker\"",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "question\\_type:time", 
                            replacement = "\"question\\_type\":\"time\"",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "response_option:", 
                            replacement = "\"response\\_option\":",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "response:", 
                            replacement = "\"response\":",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "condition:", 
                            replacement = "\"condition\":",
                            x = curr_item)
          
          curr_item <- gsub(pattern = "\\]", replacement = "\"", x = curr_item)
          curr_item <- gsub(pattern = "\\[", replacement = "\"", x = curr_item)
          
          curr_json_converted <- curr_item %>% spread_all()
          list_json_converted <- append(list_json_converted, list(curr_json_converted))
        }
      }else{
        pattern <- "question\\_text:.*?,question\\_type"
        matches <- gregexpr(pattern, curr_item)
        curr_question_text <- regmatches(curr_item, matches, invert = FALSE)
        curr_question_text <- curr_question_text[[1]]
        curr_question_text <- substring(text = curr_question_text, first = 15, last = nchar(curr_question_text)-14)
        curr_question_text <- paste("\"", curr_question_text, "\"", sep = "")
        
        curr_item <- gsub(pattern = "finish\\_time:", 
                          replacement = "\"finish\\_time\":",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "prompt\\_time:", 
                          replacement = "\"prompt\\_time\":",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "question\\_id:", 
                          replacement = "\"question\\_id\":",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "question\\_text:.*?,question\\_type", 
                          replacement = "question\\_text:marianatrench,question\\_type",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "question\\_text:", 
                          replacement = "\"question\\_text\":",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "marianatrench", 
                          replacement = curr_question_text,
                          x = curr_item)
        
        curr_item <- gsub(pattern = "question\\_type:multiple\\_choice", 
                          replacement = "\"question\\_type\":\"multiple\\_choice\"",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "question\\_type:multiple\\_select", 
                          replacement = "\"question\\_type\":\"multiple\\_select\"",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "question\\_type:string\\_picker", 
                          replacement = "\"question\\_type\":\"string\\_picker\"",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "question\\_type:time", 
                          replacement = "\"question\\_type\":\"time\"",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "response_option:", 
                          replacement = "\"response\\_option\":",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "response:", 
                          replacement = "\"response\":",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "condition:", 
                          replacement = "\"condition\":",
                          x = curr_item)
        
        curr_item <- gsub(pattern = "\\]", replacement = "\"", x = curr_item)
        curr_item <- gsub(pattern = "\\[", replacement = "\"", x = curr_item)
        
        curr_json_converted <- curr_item %>% spread_all()
        list_json_converted <- append(list_json_converted, list(curr_json_converted))
      }
    }
    
    list_json_converted <- lapply(list_json_converted, 
                                  function(x){
                                    if(!("condition" %in% colnames(x))){
                                      x <- x %>% 
                                        mutate(condition = "none") %>%
                                        select("..JSON", "document.id", "condition", "finish_time", everything())
                                    }
                                    return(x)
                                  })
    
    dat_json_converted <- bind_rows(list_json_converted)
    dat_json_converted <- dat_json_converted %>%
      rename("document_id" = "document.id") %>%
      mutate(document_id = rr)
    
    pattern <- "\\[(.*)\\]"
    matches <- gregexpr(pattern, curr_json)
    metadata <- regmatches(curr_json, matches, invert = TRUE)
    metadata_clean <- unlist(metadata)
    
    metadata_clean[1] <- substring(text = metadata_clean[1], first = 1, last = nchar(metadata_clean[1])-11)
    metadata_clean[1] <- gsub(pattern = "end_time", replacement = "\"end\\_time\"", x = metadata_clean[1])
    metadata_clean[1] <- gsub(pattern = "id:DATA", replacement = "\"id\":\"DATA\"", x = metadata_clean[1])
    metadata_clean[2] <- gsub(pattern = "start_time", replacement = "\"start\\_time\"", x = metadata_clean[2])
    metadata_clean[2] <- gsub(pattern = "type:EMA_RANDOM", replacement = "\"type\":\"EMA\\_RANDOM\"", x = metadata_clean[2])
    metadata_clean[2] <- gsub(pattern = "status:COMPLETED", replacement = "\"status\":\"COMPLETED\"", x = metadata_clean[2])
    metadata_clean[2] <- gsub(pattern = "status:ABANDONED_BY_TIMEOUT", replacement = "\"status\":\"ABANDONED\\_BY\\_TIMEOUT\"", x = metadata_clean[2])
    
    metadata_json <- paste(metadata_clean[1], metadata_clean[2], sep = "")
    metadata_json_converted <- metadata_json %>% spread_all  # This is the only time we call tidyjson::spreadall 
    metadata_json_converted <- metadata_json_converted %>% select(-"document.id")
    
    dat_json_converted[["end_time"]] <- metadata_json_converted[["end_time"]]
    dat_json_converted[["start_time"]] <- metadata_json_converted[["start_time"]]
    dat_json_converted[["status"]] <- metadata_json_converted[["status"]]
    dat_json_converted[["type"]] <- metadata_json_converted[["type"]]
    
    metadata_timezone <- strsplit(x = curr_row, split = ",{end_time:", fixed = TRUE)
    metadata_timezone <- strsplit(x = metadata_timezone[[1]][1], split = ",")
    metadata_timezone <- metadata_timezone[[1]]
    
    dat_json_converted[["recorded_time"]] <- metadata_timezone[1]
    dat_json_converted[["timezone"]] <- metadata_timezone[2]
    
    list_parsed <- append(list_parsed, list(dat_json_converted))
  }
  
  dat_parsed <- bind_rows(list_parsed)
  dat_parsed <- dat_parsed %>% 
    mutate(mars_id = dat_ema_responses[1,"mars_id"]) %>%
    select(mars_id, everything())
  
  list_collect <- append(list_collect, list(dat_parsed))
  
  print(paste("Current Done: ", i, sep=""))
}

dat_collect <- bind_rows(list_collect)

end_profile <- Sys.time()
how_long_mins <- difftime(time1 = end_profile, time2 = begin_profile, units = "mins")
print(how_long_mins)

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------

saveRDS(dat_collect, file = file.path(path_manipulated_data, "dat_long_ema_responses.rds"))

rm(list = ls())
