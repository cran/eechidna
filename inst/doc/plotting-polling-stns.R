## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.width = 6,
                      fig.height = 4,
                      fig.align='center',
                      dev = "png", cache=FALSE)

## ----message=FALSE, error = TRUE, warning=FALSE, echo=FALSE, tidy=TRUE----
library(eechidna)
library(plyr)
library(dplyr)
library(ggplot2)
library(readr)

stns <- read_csv("http://results.aec.gov.au/17496/Website/Downloads/GeneralPollingPlacesDownload-17496.csv", skip=1)

# could also get polling locations from aec2013_2pp
# stns <- aec2013_2pp %>% 
#   group_by(PollingPlaceID) %>% 
#   select(Latitude, Longitude) %>% 
#   slice(1)

## ----fig.width=7, fig.height=4, message=FALSE, error = TRUE, warning=FALSE, echo=TRUE, tidy=TRUE----
library(ggthemes)

data(nat_map)
data(nat_data)
ggplot(data=nat_data, aes(map_id=id)) +
  geom_map(map=nat_map, fill="grey90", colour="white") + 
  geom_point(data=stns, aes(x=Longitude, y=Latitude), colour="red", size=1, alpha=0.3, inherit.aes=FALSE) +
  xlim(c(112,157)) + ylim(c(-44,-11)) +
  theme_map() + coord_equal() 

## ----message=FALSE, error = TRUE, warning=FALSE, echo=FALSE, tidy=TRUE----
stns <- read_csv("http://results.aec.gov.au/20499/Website/Downloads/GeneralPollingPlacesDownload-20499.csv", skip=1)

## ----mapit, fig.width=7, fig.height=4, message=FALSE, error = TRUE, warning=FALSE, echo=TRUE, tidy=TRUE----
data(nat_map_2016)
data(nat_data_2016)
ggplot(data=nat_data_2016, aes(map_id=id)) +
  geom_map(map=nat_map_2016, fill="grey90", colour="white") + 
  geom_point(data=stns, aes(x=Longitude, y=Latitude), colour="red", size=1, alpha=0.3, inherit.aes=FALSE) +
  xlim(c(112,157)) + ylim(c(-44,-11)) +
  theme_map() + coord_equal() 

## ----addresults, fig.width=7, fig.height=5, message=FALSE, error = TRUE, warning=FALSE, echo=FALSE, tidy=TRUE----
data(aec2016_2pp)
all <- merge(stns, aec2016_2pp, by.x="PollingPlaceID", by.y="PollingPlaceID")
# Find winner
all$winner <- apply(all[,c(42,44)], 1, which.max)
all$winner <- ifelse(all$winner==1, "ALP", "LNC")
ggplot(data=nat_data_2016, aes(map_id=id)) +
  geom_map(map=nat_map_2016, fill="grey90", colour="white") + 
  geom_point(data=all, aes(x=Longitude.x, y=Latitude.x, colour=winner), size=2, alpha=0.3, inherit.aes=FALSE) +
  scale_color_manual("Party", values=c("LNC"="#FF0033", "ALP"="#0066CC")) + 
  xlim(c(112,157)) + ylim(c(-44,-11)) +
  theme_map() + coord_equal() + theme(legend.position="bottom")

