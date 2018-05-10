setwd("D:/jisnard/Documents/spoud/")
source("helpers.R")

load("applis_gany_bucket.RData")

library(forecast)
library(tseries)

tsGany <- ts(applis_gany_bucket$avg_respTime, frequency = 60)
plot(tsGany)

decomp = stl(tsGany, s.window="periodic")
deseasonal <- seasadj(decomp)

plot(deseasonal)

adf.test(deseasonal)

acf(deseasonal)
pacf(deseasonal)



g7 <- ggplot(applis_gany_bucket, aes(x=eventTime, y=avg_respTime)) +
  geom_point(col = colors[3]) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 hour",
                   labels = date_format("%H:%M")) +
  theme(axis.title.x = element_blank()) 

g7 +
  geom_point(aes(y = applis_gany_bucket[, "throughput"] * 3.5 ), col = colors[2]) +
  scale_y_continuous(name = "avg_respTime", sec.axis = sec_axis(~./3.5, name="Throughput")) +
  guides(col=guide_legend(title=NULL))


