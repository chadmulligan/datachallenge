setwd("D:/jisnard/Documents/spoud/")
source("helpers.R")

name <- "apache"

json_modif(name)
apache <- prep_data(name)

# load("raw_apache.RData")
# apache <- res
# rm(res)

glimpse(apache)

#remove callID as it is unique within correlationId and beatID which is serialized per minute
apache %<>%
  select(-beatId, -callId)


###data to 1min buckets
apache_bucket <- to_bucket(apache, "meta_host", "logTime", "time")


###subset for GANYMED
gany <- grepl("Ganymed", apache_bucket$meta_host, ignore.case = TRUE)
apache_gany_bucket <- apache_bucket[gany, ]

save(apache_gany_bucket, file = "apache_gany_bucket.RData")

rm(apache)
