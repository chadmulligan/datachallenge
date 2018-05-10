setwd("D:/jisnard/Documents/spoud/")
source("helpers.R")

name <- "applications"


##get data
json_modif(name)
applis <- prep_data(name)

glimpse(applis)

# load("raw_applications.RData")


#remove callID as it is unique within correlationId and beatID which is serialized per minute
length(unique(applis[, "beatId"]))

applis %<>%
  select(-beatId, -callId)

#format data
applis[, c("tkNameIdProvider", "userId", "orgBezeichnung")] = lapply(applis[, c("tkNameIdProvider", 
                                                                                "userId", "orgBezeichnung")],
                                                                     function(x) as.factor(x))

###making sure time is absolutely always expressed the same and stripping off the seconds decimals
test <- sapply(applis[, "eventTime"], function(x) nchar(x) != 32)
sum(test)
rm(test)


###data to 1min buckets
applis_bucket <- to_bucket(applis, "tkNameIdProvider", "eventTime", "durationMs")

save(applis_bucket, file= "applis_bucket.RData")

rm(applis)



###Plotting Throughput
g1 <- ggplot(applis_bucket, aes(x=eventTime, 
                                y=throughput, group = tkNameIdProvider, col=tkNameIdProvider)) +
  geom_point() + 
  facet_grid(tkNameIdProvider ~ . , labeller = label_parsed) +
  theme_bw() +
  ggtitle("Throughput") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(date_breaks = "1 hour",
                   labels = date_format("%H:%M")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  guides(col=guide_legend(title = element_blank()))

g1


###Plotting Average Response time
g2 <- ggplot(applis_bucket, aes(x=eventTime, 
                                y=avg_respTime, group = tkNameIdProvider, col=tkNameIdProvider)) +
  geom_point() + 
  facet_grid(tkNameIdProvider ~ . , labeller = label_parsed) +
  theme_bw() +
  ggtitle("Average Response Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(date_breaks = "1 hour",
                   labels = date_format("%H:%M")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  guides(col=guide_legend(title = element_blank())) 

g2


  ###Plotting Worktime
g3 <- ggplot(applis_bucket, aes(x=eventTime, 
                                y=worktime, group = tkNameIdProvider, col=tkNameIdProvider)) +
  geom_point() + 
  facet_grid(tkNameIdProvider ~ . , labeller = label_parsed) +
  theme_bw() +
  ggtitle("Worktime") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(date_breaks = "1 hour",
                   labels = date_format("%H:%M")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  guides(col=guide_legend(title = element_blank())) 

g3


