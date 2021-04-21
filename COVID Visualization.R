#This figure shows the daily incidence of COVID-19 in Chicago (not Cook county) by race/ethncicity. 
#Each line represents the rolling average of daily cases for the race/ethnicity groups recorded, as well 
#as those who responded as "Other" or did not respond (Unknown). The light blue columns show the overall (total) 
#incidence in Chicago. Some features of the plot to consider are:
#1. The y-axis is on the log-scale
#2. Each line is annotated using the geom_dl function in the directlabels package. The function has several options 
#for the methods= parameter, including "last.point" and "last.qp". See its documentation and try different values so 
#that the labels do not overlap when the lines are close to one another on the last day.
#3. Several values were removed to make sure that the rolling average is not <1. The scale_y_continuous() can be used to 
#set limits on the axis. It can also be used to set break points for the ticks on the y-axis; scale_x_continuous() can be 
#used for the x-axis.
#4. Be careful when calculating the rolling average as you reshape the date (wide to long), since values can repeat 
#(recall what we did for the different counties). You can see my note on this as a reply on the Discussion Board for Practice Exercise 2.

library(janitor)
library(COVID19)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(zoo)
library(directlabels)


chicagocovid=COVID_19_Daily_Testing_By_Person %>%
  clean_names()

chicagocovid= chicagocovid %>%
  mutate(date = mdy(date)) %>%
  arrange(date) %>%
  mutate(incidence = people_positive_total) %>%
  mutate(incidence.7day=rollapply(incidence, 
                                  width=7, 
                                  FUN=mean, 
                                  align="right",
                                  partial=T))
chicagocovid=chicagocovid %>% 
rename(
  Latinx = people_positive_latinx,
   Black = people_positive_black_non_latinx,
   Asian = people_positive_asian_non_latinx,
    White = people_positive_white_non_latinx,
    Other = people_positive_other_race_non_latinx,
    Unknown = people_positive_unknown_race_ethnicity)

COVID.long= chicagocovid %>%  
  pivot_longer(cols= Latinx:Unknown, names_to="PositiveRace", values_to="raceincidence")

COVID.long %>%
  group_by(PositiveRace) %>%
  mutate(raceincidence.7day=rollapply(raceincidence, 
                                  width=7, 
                                  FUN=mean, 
                                  align="right",
                                  partial=T)) %>%
  ggplot(aes(x=date, y=raceincidence.7day)) +
  geom_line(aes(color=PositiveRace)) +
  geom_col(data=chicagocovid, aes(y=incidence.7day), fill="gray", alpha=0.3) +
  scale_y_continuous(trans='log', breaks=c(1,10,100,1000,2000), limits=c(1,5000)) +
  geom_dl(aes(y=raceincidence.7day, label = PositiveRace), method=list(cex=0.8, "last.points", 'last.bumpup')) +
  #scale_color_discrete(breaks=c("people_positive_asian_non_latinx", "people_positive_black_non_latinx", 
                               # "people_positive_latinx", "people_positive_other_race_non_latinx",
                               # "people_positive_unknown_race_ethnicity", "people_positive_white_non_latinx"),
                      # labels=c("Asian", "Black", "Latinx", "Other", "Unknown", "White")) +
  guides(color=guide_legend(title = "Race/Ethnicity")) +
  labs(y="COVID-19 Incidence", x="Date", caption="Data obtained from Chicago Data Portal") +
  ggtitle("Daily Chicago COVID-19 Incidence by Race/Ethnicity") +
  theme_clean() +
  theme(legend.position = "bottom") 














## Extra Credit

xtracreditcovid=COVID_19_Daily_Testing_By_Person %>%
  clean_names()

xtracreditcovid= xtracreditcovid %>%
  mutate(date = mdy(date))

xtracreditcovid1 = xtracreditcovid %>%
#  median weekly daily positivity rate
  mutate(Asian=(people_positive_asian_non_latinx/people_tested_asian_non_latinx)) %>%
  mutate(Black=(people_positive_black_non_latinx/people_tested_black_non_latinx)) %>%
  mutate(White=(people_positive_white_non_latinx/people_tested_white_non_latinx)) %>%
  mutate(Latinx=(people_positive_latinx/people_tested_latinx)) %>%
  mutate(Other=(people_positive_other_race_non_latinx/people_tested_other_race_non_latinx)) %>%
  mutate(Unknown=(people_positive_unknown_race_ethnicity/people_tested_unknown_race_ethnicity))


xtracreditcovidlong=xtracreditcovid1 %>%
  pivot_longer(cols= Asian:Unknown, names_to="Race", values_to="RaceRate")

# week
library(lubridate)
xtracreditcovidlong= xtracreditcovidlong %>%
  mutate(dayofweek = wday(date),
       month = month(date),
       week=epiweek(date),
       year = year(date))

  
xtracreditcovidlong %>%
  filter(year=="2020") %>%
  group_by(week, Race) %>%
  filter(RaceRate<1) %>%
  filter(RaceRate>0) %>%
  summarise(rate_median=median(RaceRate, na.rm=T),
            rate_iqr=IQR(RaceRate, na.rm=T)) %>%
  ggplot(aes(color=rate_median)) +
  geom_point(aes(x=week, y=rate_median)) +
  geom_errorbar(aes(ymin=rate_median-rate_iqr, 
                    ymax=rate_median+rate_iqr,
                    x=week)) +
  geom_line(aes(x=week, y=rate_median), linetype='dashed', col='gray') +
  geom_hline(yintercept=0.05, linetype='dotted', col = 'blue') +
  scale_y_continuous(limit=c(0,1),labels=scales::percent)+
  facet_wrap(vars(Race)) +
  scale_color_viridis_c() +
  scale_color_gradient( low="navy", high="green") +
  guides(fill=guide_colorbar(title='rate_median')) +
  labs(x="Week", y="Positivity Rate (%)", caption="Data obtained from Chicago Data Portal
       Blue dashed line is the target positivity of 5%") +
  ggtitle("Median COVID-19 Positivity Rates by Race/Ethnicity") +
  theme_clean() + theme(legend.position = "bottom")