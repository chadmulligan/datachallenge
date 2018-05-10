setwd("D:/jisnard/Documents/spoud/")
source("helpers.R")

load("applis_bucket.RData")
load("wes_gany_bucket.RData")
load("apache_gany_bucket.RData")


###subset GANYMED in applications
gany <- grepl("Ganymed", applis_bucket$tkNameIdProvider, ignore.case = TRUE)
applis_gany_bucket <- applis_bucket[gany, ]

###drop unused levels from factors in tkNameIdProvider 
applis_gany_bucket$tkNameIdProvider <- factor(applis_gany_bucket$tkNameIdProvider)  

save(applis_gany_bucket, file = "applis_gany_bucket.RData")


###combine all gany data in a dataframe
combine_gany <- data.frame(
  logTime = c(as.character(applis_gany_bucket$eventTime),
              as.character(apache_gany_bucket$logTime),
              as.character(wes_gany_bucket$logTime)),
  worktime = c(applis_gany_bucket$worktime,
               apache_gany_bucket$worktime,
               wes_gany_bucket$worktime),  
  type = factor(
    c(rep("applications", length(applis_gany_bucket$worktime)),
      rep("apache", length(apache_gany_bucket$worktime)),
      rep("wes", length(wes_gany_bucket$worktime))
    ),
    levels = c("apache", "applications", "wes")
  )
)

###deal with date formating hell
combine_gany[, "logTime"] <- ymd_hms(combine_gany[, "logTime"])

save(combine_gany, file = "combine_gany.RData")



###get event data only
combine_gany_event <- data.frame(
  logTime = c(as.character(applis_gany_bucket$eventTime)),
  applis = applis_gany_bucket$worktime,
  apache = apache_gany_bucket$worktime,
  wes = wes_gany_bucket$worktime
  )
combine_gany_event[, "logTime"] <- ymd_hms(combine_gany_event[, "logTime"])

index <- c(match("2018-05-05 12:24:00", as.character(combine_gany_event$logTime)) : 
             match("2018-05-05 13:53:00", as.character(combine_gany_event$logTime)))

combine_gany_event <- combine_gany_event[index , ] %>%
  gather(type, worktime, -logTime)




###Plottting all data
g4 <- ggplot(combine_gany, aes(x=logTime, y=worktime, col = type)) +
  geom_point() +
  facet_grid(type ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("Worktime per Layer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(date_breaks = "1 hour",
                   labels = date_format("%H:%M")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.position="none")

g4



###without floating y_axis
g5 <- ggplot(combine_gany, aes(x=logTime, y=worktime, col = type)) +
  geom_point() +
  facet_grid(type ~ .) +
  theme_bw() +
  ggtitle("Worktime per Layer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(date_breaks = "1 hour",
                   labels = date_format("%H:%M")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  theme(legend.position="none")

g5



###Zooming on same graph
colors <- wes_palette(n=3, "FantasticFox")

gg_apache <- ggplot(combine_gany_event[combine_gany_event$type=="apache", ], 
                    aes(x=logTime, y=worktime)) +
  geom_point(col = colors[1]) +
  theme_bw() +
  scale_x_datetime(date_breaks = "10 mins",
                   labels = date_format("%H:%M")) +
  theme(axis.title.x = element_blank()) +
  ggtitle("Ganymed Layers Worktime") +
  theme(plot.title = element_text(hjust=.5))

gg_apache +
  geom_point(aes(y = combine_gany_event[combine_gany_event$type=="wes", "worktime"] * 1000 ), col = colors[2]) +
  geom_point(aes(y = combine_gany_event[combine_gany_event$type=="applis", "worktime"] * 1000 ), col = colors[3]) + 
  scale_y_continuous(name = "Apache Worktime", sec.axis = sec_axis(~./1000, name="WES, Application Worktime")) +
  guides(col=guide_legend(title=NULL))





