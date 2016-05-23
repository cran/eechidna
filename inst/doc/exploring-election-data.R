## ----setup, echo=FALSE---------------------------------------------------
library(knitr)
opts_chunk$set(fig.width = 6,
                      fig.height = 4,
                      fig.align='center',
                      dev = "png",
                      warning = FALSE,
                      message = FALSE)


## ----load-read-polling, message=FALSE, error = FALSE---------------------
library(eechidna)
library(plyr)
library(dplyr)
library(purrr)
library(knitr)
library(broom)
library(tidyr)
# inspect the data frame
glimpse(aec2013_fp)
# show the first few rows
head(aec2013_fp) %>% kable

## ----who_won-------------------------------------------------------------
who_won <- aec2013_2pp_electorate %>% 
  group_by(PartyNm) %>% 
  tally() %>% 
  arrange(desc(n)) 

# inspect
who_won %>% 
  kable()

# plot
library(ggplot2)
library(scales)
ggplot(who_won, 
       aes(reorder(PartyNm, n), 
           n)) +
  geom_point(size = 2) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_bw() +
  ylab("Total number of electorates") +
  xlab("Party") +
  theme(text = element_text(size=10))

## ----total_votes_for_parties---------------------------------------------
total_votes_for_parties <- aec2013_fp %>% 
  select(PartyNm, OrdinaryVotes) %>% 
  group_by(PartyNm) %>% 
  summarise(total_votes = sum(OrdinaryVotes, rm.na = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(total_votes))

#  inspect
total_votes_for_parties %>% 
  head %>% # just the first 10 rows
  kable

## ----plot_total_votes_for_parties,  fig.height = 6-----------------------
ggplot(total_votes_for_parties, 
       aes(reorder(PartyNm, total_votes), 
           total_votes)) +
  geom_point(size = 2) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) +
  theme_bw() +
  ylab("Total ordinary votes") +
  xlab("Party") +
  theme(text = element_text(size=10))

## ----who_most-votes------------------------------------------------------
who_most_votes <- 
aec2013_fp %>% 
  filter(CandidateID != 999) %>% #exclude informal votes
  mutate(candidate_full_name = paste0(GivenNm, " ", Surname, " (", CandidateID, ")")) %>% 
  group_by(candidate_full_name) %>% 
  summarise(total_votes_for_candidate = sum(OrdinaryVotes, rm.na = TRUE)) %>% 
  arrange(desc(total_votes_for_candidate))
# inspect
who_most_votes %>% 
  mutate(total_votes_for_candidate = 
           prettyNum(total_votes_for_candidate, 
                     big.mark = ","))  %>% 
  head %>% 
  kable

## ----who_highest_proportion-votes----------------------------------------
who_most_votes_prop <- 
aec2013_fp %>% 
  filter(CandidateID != 999) %>% #exclude informal votes
  mutate(candidate_full_name = 
           paste0(GivenNm, " ", Surname, " (", CandidateID, ")")) %>% 
  group_by(Electorate, candidate_full_name) %>% 
  summarise(sum_votes = sum(OrdinaryVotes))  %>% 
  mutate(prop_votes = round(sum_votes / sum(sum_votes), 3),
         sum_votes = prettyNum(sum_votes, ",")) %>% 
  ungroup %>% 
  arrange(desc(prop_votes))

# inspect
who_most_votes_prop %>% 
  data.frame %>% 
  head %>% 
  kable

## ----who_won_least_votes_prop--------------------------------------------
who_won_least_votes_prop <- 
 aec2013_fp %>% 
   filter(CandidateID != 999) %>% # keep only the winners
   mutate(candidate_full_name = 
            paste0(GivenNm, " ", Surname, " (", CandidateID, ")")) %>% 
   group_by(Electorate, candidate_full_name) %>% 
   summarise(sum_votes = sum(OrdinaryVotes)) %>% 
   mutate(prop_votes = round(sum_votes / sum(sum_votes), 2)) %>% 
   ungroup %>% 
   left_join(aec2013_2pp_electorate %>% 
               mutate(candidate_full_name = 
            paste0(GivenNm, " ", Surname, " (", CandidateID, ")")), "candidate_full_name") %>% 
   filter(Elected == "Y") %>% 
   select(Electorate.x, 
          candidate_full_name, 
          prop_votes, 
          PartyNm) %>% 
   arrange(prop_votes)

# have a look
who_won_least_votes_prop %>%
 head %>%
 kable

## ----fairfax-------------------------------------------------------------
fairfax <- 
aec2013_fp %>% 
  filter(CandidateID != 999) %>% # exclude informal votes
  filter(Electorate == "Fairfax") %>% 
  mutate(candidate_full_name = 
           paste0(GivenNm, " ", 
                  Surname, 
                  " (", 
                  CandidateID, 
                  ")")) %>% 
  group_by(Electorate, candidate_full_name) %>% 
  summarise(sum_votes = sum(OrdinaryVotes)) %>% 
  mutate(prop_votes = round(sum_votes / sum(sum_votes), 2),
         sum_votes = prettyNum(sum_votes, big.mark = ",")) %>% 
  ungroup %>% 
  arrange(desc(prop_votes))

# inspect
fairfax %>% 
  head %>% 
  kable

## ----plot_prop_particular_party, fig.height = 12, fig.width=10-----------
# summarise and compute proportion of votes for a particular party
p <- aec2013_fp %>%
    filter(CandidateID != 999) %>% # exclude informal votes
  group_by(Electorate, State) %>%
  summarise(
    TotalVotes = sum(OrdinaryVotes),
    ProportionLabor = round(sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / TotalVotes, 3)) %>%
  filter(TotalVotes != 0) %>% 
  arrange(desc(ProportionLabor)) %>% 
  group_by(State) %>% 
  # send the data to the plotting function
  do(plots=ggplot(data = .) + 
       aes(x = ProportionLabor, 
           y = reorder(Electorate, ProportionLabor), 
           size = TotalVotes, 
           label = State) +
       geom_point() +
       ylab("Electorate") +
       labs(title = .$State) + 
       scale_x_continuous("Proportion voting Labor Party", 
                          label = percent) +
       scale_size("Number of\nvotes cast", 
                  label = comma)  +
       theme_bw() +
       theme(text = element_text(size=10)))

# draw the plots
library(gridExtra)
n <- length(p$plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(p$plots, ncol=nCol))

## ----plot_prop_by_particular_party, fig.height = 6-----------------------
# Comparing party and candidate votes of several parties -------
proportions <- aec2013_fp %>%
  filter(CandidateID != 999) %>% # exclude informal votes
  group_by(Electorate) %>%
  summarise(Prop_Labour = sum(OrdinaryVotes[PartyNm == "Australian Labor Party"]) / sum(OrdinaryVotes),
            Prop_Coalition = sum(
              OrdinaryVotes[PartyNm == "Liberal"],
              OrdinaryVotes[PartyNm == " Country Liberal Party"],
              OrdinaryVotes[PartyNm == "The Nationals"],
              OrdinaryVotes[PartyNm == "Country Liberals (NT)"]
              ) / sum(OrdinaryVotes),
            Prop_Greens = sum(OrdinaryVotes[PartyNm == "The Greens"]) / sum(OrdinaryVotes)) 

# make a scatterplot matrix
library(GGally)
ggpairs(proportions, columns = 2:ncol(proportions)) + theme_bw()


## ----join_census_and_election--------------------------------------------
census_and_election <- left_join(aec2013_2pp_electorate, 
                                 abs2011,
                                 by = c("Electorate" = "Electorate"))

## ------------------------------------------------------------------------
# subset only the columns we want for the model
census_and_election_subset <-  
  census_and_election %>% 
  ungroup %>% 
  select(Electorate, 
         Average_Australian_Labor_Party_Percentage_in_electorate,
         Population:NotOwned)  %>% 
  rename(ALP_perc = Average_Australian_Labor_Party_Percentage_in_electorate)

library(corrplot)
M <- cor(census_and_election_subset[, c(2:ncol(census_and_election_subset))], 
         use = "pairwise.complete.obs")
corrplot.mixed(M, 
               lower="ellipse", 
               upper="number", 
               tl.pos = "lt",
               tl.cex = 0.5,
               tl.col = "black",
               number.cex= 0.5)

## ----explore-------------------------------------------------------------
options(scipen = 10) # for more readable numbers
census_variables <- names(abs2011)[-c(1:3)]

# compute the multiple regressions
mutliple_regression_model <- 
  census_and_election %>% 
  ungroup %>% 
  select(
         Average_Australian_Labor_Party_Percentage_in_electorate,
         Population:NotOwned) %>% 
  lm(Average_Australian_Labor_Party_Percentage_in_electorate ~ ., 
     data = . )

mutliple_regression_model %>% 
  glance %>% 
  dmap(round, 3) %>% 
  kable

## ------------------------------------------------------------------------
# find the variables with a significant effect
mutliple_regression_model %>% 
  tidy %>% 
  filter(p.value < 0.05) %>% 
  dmap_if(is.numeric, round, 3) %>% 
  arrange(p.value) %>% 
  kable

## ------------------------------------------------------------------------
# model that includes all census variables
all_vars <- mutliple_regression_model %>% 
  tidy %>% 
  filter(p.value < 0.05) %>% 
  arrange(p.value) 

census_and_election %>% 
  ungroup %>% 
  select_(.dots = all_vars$term,
          "Average_Australian_Labor_Party_Percentage_in_electorate")  %>% 
  gather(variable, 
         value, 
         -Average_Australian_Labor_Party_Percentage_in_electorate) %>% 
  ggplot(aes(value, Average_Australian_Labor_Party_Percentage_in_electorate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ variable, scales = "free") +
  theme_bw()

## ----senate, echo=TRUE, tidy=TRUE, warning=FALSE, error=FALSE, message=FALSE, fig.width=5, fig.height=3----
senate <-
  read.csv("SenateSenatorsElected2013.csv", stringsAsFactors = FALSE)

  coalition <- c(
  "Country Liberals (NT)",
  "Liberal",
  "Liberal National Party of Queensland",
  "The Nationals"
  )
  
  labor <-
  c("Australian Labor Party",
  "Australian Labor Party (Northern Territory) Branch",
  "Labor")
  
  greens <- c("The Greens", "Australian Greens", "The Greens (WA)")
  
  senate <- senate %>%
  mutate(PartyNm = ifelse(
                   as.character(PartyNm) %in% coalition,
                   "Liberal National Coalition",
                   PartyNm
  ))
  
  senate <- senate %>%
                mutate(PartyNm = ifelse(
                as.character(PartyNm) %in% labor,
                "Australian Labor Party",
                PartyNm
  ))
  
  senate <- senate %>%
               mutate(PartyNm = ifelse(as.character(PartyNm) %in% greens,
               "Australian Greens", PartyNm))
  
  senate$PartyNm <-
    factor(senate$PartyNm, 
         levels = names(sort(table(senate$PartyNm), 
                             decreasing =
                              T)))
  
  # Order = Liberal National Coalition, Australian Labor Party, The Greens, Palmer, motoring, sports, family first,  Lib Dems, Nick Xenophon
  partycolours = c(
  "#0066CC",
  "#FF0033",
  "#009900",
  "#FFFF00",
  "#00008B",
  "#0000FF",
  "#87CEFA",
  "#C71585",
  "#FF4500"
  )
  
  ggplot(data = senate, 
         aes(x = PartyNm, 
             fill = PartyNm)) + 
    geom_bar() +
  xlab("") + 
    ylab("") +
  scale_fill_manual(name = "Party", 
                    values = partycolours) +
  coord_flip() + 
    theme(legend.position = "None")

## ----bystate, echo=TRUE, tidy=TRUE, warning=FALSE, error=FALSE, message=FALSE, fig.width=7, fig.height=3----
senate$StateAb <- factor(senate$StateAb, 
                         levels=c("QLD", "NSW", "WA", "VIC", "SA", "TAS", "ACT", "NT"))

ggplot(data=senate, 
       aes(x=StateAb, 
           fill=PartyNm)) + 
  geom_bar() +
  xlab("") + 
  ylab("") +
  scale_fill_manual(name="Party", 
                    values=partycolours) 

