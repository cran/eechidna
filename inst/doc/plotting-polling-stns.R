## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.width = 6,
                      fig.height = 4,
                      fig.align='center',
                      dev = "png", cache=FALSE)

## ----load-read, message=FALSE, error = TRUE, warning=FALSE, echo=FALSE, tidy=TRUE----
library(eechidna)
library(plyr)
library(dplyr)
library(ggplot2)
library(readr)

stns <- read_csv("GeneralPollingPlaces2013.csv")

# could also get polling locations from aec2013_2pp
# stns <- aec2013_2pp %>% 
#   group_by(PollingPlaceID) %>% 
#   select(Latitude, Longitude) %>% 
#   slice(1)

## ----mapit, fig.width=7, fig.height=4, message=FALSE, error = TRUE, warning=FALSE, echo=TRUE, tidy=TRUE----
# Make it look like a map
theme_map <- theme_bw()
theme_map$line <- element_blank()
theme_map$strip.text <- element_blank()
theme_map$axis.text <- element_blank()
theme_map$plot.title <- element_blank()
theme_map$axis.title <- element_blank()
theme_map$panel.border <- element_rect(colour = "white", size=1, fill=NA)

data(nat_map)
ggplot(data=nat_map) +
  geom_polygon(aes(x=long, y=lat, group=group, order=order),
               fill="grey90", colour="white") + 
  geom_point(data=stns, aes(x=Longitude, y=Latitude), colour="red", size=1, alpha=0.3) +
  xlim(c(112,157)) + ylim(c(-44,-11)) +
  theme_map + coord_equal() 

## ----addresults, fig.width=7, fig.height=5, message=FALSE, error = TRUE, warning=FALSE, echo=FALSE, tidy=TRUE----
data(aec2013_2pp)
all <- merge(stns, aec2013_2pp, by.x="PollingPlaceID", by.y="PollingPlaceID")
# Find winner
all$winner <- apply(all[,c(42,44)], 1, which.max)
all$winner <- ifelse(all$winner==1, "ALP", "LNC")
ggplot(data=nat_map) +
  geom_polygon(aes(x=long, y=lat, group=group, order=order),
               fill="grey90", colour="white") + 
  geom_point(data=all, aes(x=Longitude.x, y=Latitude.x, colour=winner), size=1, alpha=0.3) +
  scale_color_manual("Party", values=c("ALP"="#FF0033", "LNC"="#0066CC")) + 
  xlim(c(112,157)) + ylim(c(-44,-11)) +
  theme_map + coord_equal() + theme(legend.position="bottom")

