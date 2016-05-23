## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.width = 6,
                      fig.height = 4,
                      fig.align='center',
                      dev = "png", cache=FALSE)

## ----load-read-merge, message=FALSE, error = TRUE, warning=FALSE, echo=FALSE, tidy=TRUE----
library(eechidna)
library(plyr)
library(dplyr)
library(ggplot2)
data(nat_data_cart)
data(nat_map)
data(aec2013_fp_electorate)
map.winners <- aec2013_fp_electorate %>% filter(Elected == "Y") %>% 
  select(Electorate, PartyNm) %>% 
  merge(nat_map, by.x="Electorate", by.y="ELECT_DIV")

## ----group, message=FALSE, error = TRUE, warning=FALSE, echo=TRUE, tidy=TRUE----
# Grouping different Lib/Nats togethers
map.winners$PartyNm <- as.character(map.winners$PartyNm)
coalition <- c("Country Liberals (NT)", "Liberal", 
               "Liberal National Party of Queensland", "The Nationals")
map.winners.grouped <- mutate(map.winners, 
    PartyNm = ifelse(as.character(PartyNm) %in% coalition,
       "Liberal National Coalition", PartyNm))
map.winners.grouped <- map.winners.grouped %>% arrange(group, order)

# Colour cells to match that parties colours
# Order = Australian Labor Party, Independent, Katters, Lib/Nats Coalition, Palmer, The Greens
partycolours = c("#FF0033", "#000000", "#CC3300", "#0066CC", "#FFFF00", "#009900")

## ----fig.width=7, fig.height=6, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, tidy=TRUE----
# Make it look like a map
theme_map <- theme_bw()
theme_map$line <- element_blank()
theme_map$strip.text <- element_blank()
theme_map$axis.text <- element_blank()
theme_map$plot.title <- element_blank()
theme_map$axis.title <- element_blank()
theme_map$panel.border <- element_rect(colour = "white", size=1, fill=NA)

ggplot(data=map.winners.grouped) + 
  geom_polygon(aes(x=long, y=lat, group=group, order=order, fill=PartyNm)) +
  scale_fill_manual(name="Politcal Party", values=partycolours) +
  theme_map + coord_equal() + theme(legend.position="bottom")

## ----fig.width=7, fig.height=6, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE----
# Load election results
cart.winners <- aec2013_fp_electorate %>% filter(Elected == "Y") %>% 
  select(Electorate, PartyNm) %>% 
  merge(nat_data_cart, by.x="Electorate", by.y="ELECT_DIV")

# Grouping different Lib/Nats togethers
cart.winners$PartyNm <- as.character(cart.winners$PartyNm)
coalition <- c("Country Liberals (NT)", "Liberal", "Liberal National Party of Queensland",
               "The Nationals")
cart.winners.grouped <- mutate(cart.winners, 
  PartyNm = ifelse(as.character(PartyNm) %in% coalition, 
                   "Liberal National Coalition", PartyNm))

# Plot it
ggplot(data=nat_map) +
  geom_polygon(aes(x=long, y=lat, group=group, order=order),
               fill="grey90", colour="white") +
  geom_point(data=cart.winners.grouped, aes(x=x, y=y, colour=PartyNm), size=2, alpha=0.8) +
  scale_colour_manual(name="Political Party", values=partycolours) +
  theme_map + coord_equal() + theme(legend.position="bottom")

