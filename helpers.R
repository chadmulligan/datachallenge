#necessary packages and functions

library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)
library(magrittr)
library(lazyeval)
library(grid)
library(wesanderson)
library(forecast)
library(tseries)


###modify the JSON - add commas, brackets...
json_modif <- function(name=character()){ 
  
  filepath = paste0(name, ".json")
  res <- gsub("}", "},", readLines(con = file(filepath)))
  save(res, file = paste0(name, "_gsubs.RData"))
  
  fileConn <- file(descr = paste0(name, "_mods.json"), "w")
  writeLines("[", fileConn)
  writeLines(res, fileConn)
  #writeLines("test", fileConn)
  ###adding an extra void element to control for the last "," that yields an error when importing
  writeLines("{} \n]", fileConn)
  rm(res)
  close(fileConn)
  gc()
}


####get data from json_modif and converts to R object
####removes unnecessary columnms and saves intermediary state of the data
prep_data <- function(name=character()) {
  
  filepath = paste0(name, "_mods.json")
  
  res <- jsonlite::fromJSON(readLines(con = filepath))

  ###removing the extra void element added during json_modif
  res <- res[-nrow(res), ]
  
  ###save raw data
  save(res, file = paste0("raw_", name, ".RData"))
  
  ##identify columns of interest (not having a single value, nor unique per observation)
  nrow_obs <- nrow(res)
  non_unique_var <- sapply(res, function(x) (length(unique(x)) > 1) & length(unique(x)) != nrow_obs)
  res <- res[, non_unique_var]
  
  ###searching for incomplete cases and removing them
  if(!nrow(res) == sum(complete.cases(res))) { 
    res <- res[complete.cases(res), ]
    }
  
  ###saves data light version
  save(res, file = paste0(name, ".RData"))
  
  gc() 
  
  res
  
}



###data -> 1 min buckets
to_bucket <- function(df, key = char(), timeCol = char(), durationCol = char()){
  
  ###strip seconds ffrom date and make POSIXct object
  df[, timeCol] <- str_sub(df[, timeCol], 1, 16)
  df[, timeCol] <- gsub("T", " ", df[, timeCol])
  df[, timeCol]  <- ymd_hm(df[, timeCol])
  
  
  df %>%
    group_by_(timeCol, key) %>%
    summarise_(throughput = "n()", 
               avg_respTime = interp(~mean(v), v=as.name(durationCol)), 
               worktime = interp(~sum(v), v=as.name(durationCol))) %>%
    as.data.frame()
  
}