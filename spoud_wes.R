setwd("D:/jisnard/Documents/spoud/")
source("helpers.R")

name <- "wes"

json_modif(name)
wes <- prep_data(name)

# load("raw_wes.RData")
# wes <- res
# rm(res)

glimpse(wes)

#remove callID as it is unique within correlationId and beatID which is serialized per minute
wes %<>%
  select(-beatId, -callId)


###data to 1min buckets
wes_bucket <- to_bucket(wes, "servername", "logTime", "dTF")


###subset for GANYMED
gany <- grepl("Ganymed", wes_bucket$servername, ignore.case = TRUE)
wes_gany_bucket <- wes_bucket[gany, ]

save(wes_gany_bucket, file = "wes_gany_bucket.RData")

rm(wes)
