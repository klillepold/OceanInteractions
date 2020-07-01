# Script to produce figure 2 in paper. 


#Setting up the environment -----
library(tidyverse)

#********************************************************
#Import the data ----
#Import the data from DOI XXXXXXXX

#Remove this once on public repo
setwd("C:/Users/emmy/Dropbox/Ocean Stewardship Project/DATA") #Remember to change username for different user. 

#Edit to name of final datasheet, plus correct sheet names. 
Direct<-read_excel("final_interactiondata_june2020.xlsx",sheet=2)
Mechanisms<-read_excel("final_interactiondata_june2020.xlsx",sheet=3,skip=1)

#**********************************************
#Analysis of impactors and outcomes
direct_clean<-Direct%>%
  rename(Interaction=`Sector-Interaction`)%>%
  select(Interaction,Outcome,From, To, Bidirectional,Comment) %>% 
  mutate(mech_node=NA)

mechanism_clean<-Mechanisms%>%
  rename(mech_node=`Mechanism Node`)%>%
  select(Interaction,Outcome,From, To, Bidirectional,Comment,mech_node) 

interactions_full<-rbind(direct_clean,mechanism_clean)%>% 
  rowid_to_column()

#Since this plot does not want to focus on mpa and eco interactions, remove these. 
interactions_full_sectors<-interactions_full %>% 
  filter(!str_detect(Interaction, 'eco')) %>% 
  filter(!str_detect(Interaction, 'mpa'))

bidirectional<-interactions_full_sectors %>% 
  filter(Bidirectional==1)

onedirectional<-interactions_full_sectors %>% 
  filter(is.na(Bidirectional))


#Ugly that these are separated and then merged. Should ideally figure out a lapply/purr:map solution
#But cannot be bothered to spend more time now. 

agg_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'agg'))%>%
  mutate(sector="agg") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

agg<-onedirectional%>%
  filter(str_detect(Interaction, 'agg'))%>%
  mutate(sector="agg") %>% 
  mutate(impact=case_when(From=="agg"~"impactor",
                          To=="agg"~"impacted"))

aqua_bi<-bidirectional %>%
  filter(str_detect(Interaction, 'aqua'))%>%
  mutate(sector="aqua") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

aqua<-onedirectional%>%
  filter(str_detect(Interaction, 'aqua'))%>%
  mutate(sector="aqua") %>% 
  mutate(impact=case_when(From=="aqua"~"impactor",
                          To=="aqua"~"impacted"))

bio_bi<-bidirectional %>%
  filter(str_detect(Interaction, 'bio'))%>%
  mutate(sector="bio") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

bio<-onedirectional %>%
  filter(str_detect(Interaction, 'bio'))%>%
  mutate(sector="bio") %>% 
  mutate(impact=case_when(From=="bio"~"impactor",
                          To=="bio"~"impacted"))

des_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'des'))%>%
  mutate(sector="des") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

des<-onedirectional%>%
  filter(str_detect(Interaction, 'des'))%>%
  mutate(sector="des") %>% 
  mutate(impact=case_when(From=="des"~"impactor",
                          To=="des"~"impacted"))

dril_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'dril'))%>%
  mutate(sector="dril") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

dril<-onedirectional%>%
  filter(str_detect(Interaction, 'dril'))%>%
  mutate(sector="dril") %>% 
  mutate(impact=case_when( From=="dril"~"impactor",
                           To=="dril"~"impacted"))

mil_bi<-bidirectional %>%
  filter(str_detect(Interaction, 'mil'))%>%
  mutate(sector="mil") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

mil<-onedirectional %>%
  filter(str_detect(Interaction, 'mil'))%>%
  mutate(sector="mil") %>% 
  mutate(impact=case_when( From=="mil"~"impactor",
                           To=="mil"~"impacted"))

min_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'min'))%>%
  mutate(sector="min") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

min<-onedirectional%>%
  filter(str_detect(Interaction, 'min'))%>%
  mutate(sector="min") %>% 
  mutate(impact=case_when(From=="min"~"impactor",
                          To=="min"~"impacted"))

ren_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'ren'))%>%
  mutate(sector="ren") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

ren<-onedirectional%>%
  filter(str_detect(Interaction, 'ren'))%>%
  mutate(sector="ren") %>% 
  mutate(impact=case_when(From=="ren"~"impactor",
                          To=="ren"~"impacted"))

ship_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'ship'))%>%
  mutate(sector="ship") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

ship<-onedirectional%>%
  filter(str_detect(Interaction, 'ship'))%>%
  mutate(sector="ship") %>% 
  mutate(impact=case_when(From=="ship"~"impactor",
                          To=="ship"~"impacted"))


tel_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'tel'))%>%
  mutate(sector="tel") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))


tel<-onedirectional%>%
  filter(str_detect(Interaction, 'tel'))%>%
  mutate(sector="tel") %>% 
  mutate(impact=case_when(From=="tel"~"impactor",
                          To=="tel"~"impacted"))

wave_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'wave'))%>%
  mutate(sector="wave") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

wave<-onedirectional%>%
  filter(str_detect(Interaction, 'wave'))%>%
  mutate(sector="wave") %>% 
  mutate(impact=case_when(From=="wave"~"impactor",
                          To=="wave"~"impacted"))

wind_bi<-bidirectional %>%
  filter(str_detect(Interaction, 'wind'))%>%
  mutate(sector="wind") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

wind<-onedirectional %>%
  filter(str_detect(Interaction, 'wind'))%>%
  mutate(sector="wind") %>% 
  mutate(impact=case_when(From=="wind"~"impactor",
                          To=="wind"~"impacted"))

fish_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'fish'))%>%
  mutate(sector="fish") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

fish<-onedirectional%>%
  filter(str_detect(Interaction, 'fish'))%>%
  mutate(sector="fish") %>% 
  mutate(impact=case_when(From=="fish"~"impactor",
                          To=="fish"~"impacted"))

tou_bi<-bidirectional%>%
  filter(str_detect(Interaction, 'tou'))%>%
  mutate(sector="tou") %>% 
  mutate(impact=case_when(Bidirectional==1~"mutual"))

tou<-onedirectional%>%
  filter(str_detect(Interaction, 'tou'))%>%
  mutate(sector="tou") %>% 
  mutate(impact=case_when(From=="tou"~"impactor",
                          To=="tou"~"impacted"))

sector_analysis_bi<-rbind(agg_bi,aqua_bi,bio_bi,des_bi,dril_bi,mil_bi,min_bi,ren_bi,ship_bi,tel_bi,
                          wave_bi,wind_bi,fish_bi,tou_bi) %>% 
  unique()

sector_analysis<-rbind(agg,aqua,bio,des,dril,mil,min,ren,ship,tel,wave,wind,fish,tou) %>% 
  unique()

#Don't know what this is useful for or if i should keep it. 
sector_summary<-sector_analysis %>% 
  group_by(sector, impact) %>% 
  tally()

#Double checking that the sample sizes are correct. 
#I.e. there are 97 interactions (53 of those are bidirectional and 44 are one-directional)
#This means that there should be a sample size of 106 for mutual and 44 for both impactor and impacted. 

#I.e. there are 90 unique interactions (49 of those are bidirectional and 41 are one-directional)
#This means that there should be a sample size of 98 for mutual and 41 for both impactor and impacted. 

sample_size<-sector_analysis %>% 
  group_by(impact) %>% 
  tally()

sample_size_bi<-sector_analysis_bi %>% 
  group_by(impact) %>% 
  tally()

sector_analysis_full<-rbind(sector_analysis,sector_analysis_bi)

sector_summary_withoutcome<-sector_analysis_full %>% 
  mutate(outcome_direction=case_when(Outcome=="space-crowd"|Outcome=="space-ex"|Outcome=="natcap-dimin"|Outcome=="operation-dimin"|
                                       Outcome=="value-dimin"~"negative",
                                     Outcome=="space-syn"|Outcome=="natcap-enhance"|Outcome=="operation-enhance"|
                                       Outcome=="value-enhance"~"positive")) %>% 
  group_by(sector, impact,outcome_direction, Outcome) %>% 
  tally()


#*************************************************
#Figures for interaction breakdown ----
#Split on the different types of impact (impactor, impacted and mutual) to make cleaner


sector_plot_summary <- sector_summary_withoutcome %>%
  mutate(n = ifelse(outcome_direction == "positive",n, -1*n)) %>% 
  ungroup() %>% 
  mutate(sector=factor(sector)) %>% 
  mutate(Outcome=factor(Outcome)) %>% 
  mutate(Outcome= fct_relevel(Outcome, 
                              "value-dimin","operation-dimin","space-crowd","space-ex","natcap-dimin",
                              "value-enhance","operation-enhance","space-syn","natcap-enhance"))


breaks_values <- pretty(sector_plot_summary$n, n=20)
sector_order<-c("mil","dril","ship","min","fish","agg","aqua","wave","wind","des","tou","tel","bio")


mutual_plot<-sector_plot_summary %>% 
  filter(impact=="mutual") %>%
  ggplot(aes(x = sector, y = n, fill = Outcome))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(breaks = breaks_values,
                     labels = abs(breaks_values))+
  scale_x_discrete(drop=F,limits = rev(sector_order))+
  #scale_x_discrete(drop=F,limits = rev(levels(sector_plot_summary$sector)))+
  theme_minimal()+
  scale_fill_manual(values = c("value-dimin" = "#FA8072",
                               "value-enhance" = "#0b6623",
                               "operation-dimin" = "#8D021F",
                               "operation-enhance" = "#9dc183",
                               "space-crowd" = "#CA3433",
                               "space-ex" = "#FF0800",
                               "space-syn" = "#3CB371",
                               "natcap-dimin" = "#FF0000",
                               "natcap-enhance" = "#c7ea46"))+
  labs(y = "No. of distinct observed interactions",x= "Sectors",title="Bidirectional interactions" )


impactor_plot<-sector_plot_summary %>% 
  filter(impact=="impactor") %>%
  ggplot(aes(x = sector, y = n, fill = Outcome))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(breaks = breaks_values,
                     labels = abs(breaks_values))+
  scale_x_discrete(drop=F,limits = rev(sector_order))+
  #scale_x_discrete(drop=F,limits = rev(levels(sector_plot_summary$sector)))+
  theme_minimal()+
  theme(legend.position = "none")+ #To get full legend set this to bottom
  scale_fill_manual(values = c("value-dimin" = "#FA8072",
                               "value-enhance" = "#0b6623",
                               "operation-dimin" = "#8D021F",
                               "operation-enhance" = "#9dc183",
                               "space-crowd" = "#CA3433",
                               "space-ex" = "#FF0800",
                               "space-syn" = "#3CB371",
                               "natcap-dimin" = "#FF0000",
                               "natcap-enhance" = "#c7ea46"))+
  labs(y = "No. of distinct observed interactions",x= "Sectors",title="Impactors" )


impacted_plot<-sector_plot_summary %>% 
  filter(impact=="impacted") %>%
  ggplot(aes(x = sector, y = n, fill = Outcome))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(breaks = breaks_values,
                     labels = abs(breaks_values))+
  scale_x_discrete(drop=F,limits = rev(sector_order))+
  #scale_x_discrete(drop=F,limits = rev(levels(sector_plot_summary$sector)))+
  theme_minimal()+
  scale_fill_manual(values = c("value-dimin" = "#FA8072",
                               "value-enhance" = "#0b6623",
                               "operation-dimin" = "#8D021F",
                               "operation-enhance" = "#9dc183",
                               "space-crowd" = "#CA3433",
                               "space-ex" = "#FF0800",
                               "space-syn" = "#3CB371",
                               "natcap-dimin" = "#FF0000",
                               "natcap-enhance" = "#c7ea46"))+
  labs(y = "No. of distinct observed interactions",x= "Sectors",title="Impacted" )

#For saving figures
#Set your own saving space. 
setwd("C:/Users/emmy/Dropbox/Ocean Stewardship Project/FIGURES/Individual plots") #Remember to change username for different user. 

mutual_plot
ggsave("bidirectional_plot.png", dpi=300)
ggsave("bidirectional_plot.svg")

impactor_plot
ggsave("impactor_plot.png", dpi=300)
ggsave("impactor_plot.svg")

impacted_plot
ggsave("impacted_plot.png", dpi=300)
ggsave("impacted_plot.svg")


# Plots are combined in a panel in Inkscape
#***********************************************