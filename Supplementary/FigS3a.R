#Post-abstract methods
#Selection of articles for full read and framework synthesis

#Set up environment
library(tidyverse)
library(readxl)

#****************************************
#Import data
#abstracts<-read_csv("C:/Users/emmy/Dropbox/Ocean Stewardship_personal/included_sector_breakdown.csv")
abstracts<-read_excel("C:/Users/kate/Dropbox/Ocean Stewardship Project/DATA/included_sector_breakdown.xlsx")
#******************************************
abstracts<-abstracts%>%
  rowid_to_column("id")

abstract_short<-abstracts%>%
  select(id,agg:military)%>%
  replace(., is.na(.), 0)



sector_summary<-abstract_short %>%
  summarise_at(vars(agg:military), sum, na.rm = TRUE)%>%
  gather(sector,number)

#Since space, ecohealth and mpas are not really sectors (plus discrepancy at space), I will remove these for now
desalination<-abstract_short%>%
  filter(desalination==1)%>%
  mutate(sector="Desalination")

agg<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==1)%>%
  mutate(sector="Aggregates")

military<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==1)%>%
  mutate(sector="Military")

cable<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==1)%>%
  mutate(sector="Cables")

pipeline<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==1)%>%
  mutate(sector="Pipeline")

bioprospect<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==1)%>%
  mutate(sector="Bioprospecting")

dredging<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==1)%>%
  mutate(sector="Dredging")

reclamation<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==1)%>%
  mutate(sector="Land Reclamation")

wave<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==1)%>%
  mutate(sector="Wave Energy")

renew<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==1)%>%
  mutate(sector="Renewables (other)")

disposal<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==1)%>%
  mutate(sector="Disposal")

wind<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==0)%>%
  filter(wind==1)%>%
  mutate(sector="Wind Energy")

mining<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==0)%>%
  filter(wind==0)%>%
  filter(mining==1)%>%
  mutate(sector="Mining")

shipping<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==0)%>%
  filter(wind==0)%>%
  filter(mining==0)%>%
  filter(shipping==1)%>%
  mutate(sector="Shipping")

drilling<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==0)%>%
  filter(wind==0)%>%
  filter(mining==0)%>%
  filter(shipping==0)%>%
  filter(drilling==1)%>%
  mutate(sector="Drilling")

aquaculture<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==0)%>%
  filter(wind==0)%>%
  filter(mining==0)%>%
  filter(shipping==0)%>%
  filter(drilling==0)%>%
  filter(aquaculture==1)%>%
  mutate(sector="Aquaculture")

tourism<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==0)%>%
  filter(wind==0)%>%
  filter(mining==0)%>%
  filter(shipping==0)%>%
  filter(drilling==0)%>%
  filter(aquaculture==0)%>%
  filter(tourism==1)%>%
  mutate(sector="Tourism")

fishing<-abstract_short%>%
  filter(desalination==0)%>%
  filter(agg==0)%>%
  filter(military==0)%>%
  filter(cable==0)%>%
  filter(pipeline==0)%>%
  filter(bioprospect==0)%>%
  filter(dredging==0)%>%
  filter(reclamation==0)%>%
  filter(wave==0)%>%
  filter(renew==0)%>%
  filter(disposal==0)%>%
  filter(wind==0)%>%
  filter(mining==0)%>%
  filter(shipping==0)%>%
  filter(drilling==0)%>%
  filter(aquaculture==0)%>%
  filter(tourism==0)%>%
  filter(fishing==1)%>%
  mutate(sector="Fishing")

#Make one dataframe to plot
all_articles<-rbind(desalination,agg,military,cable,pipeline,bioprospect,dredging, reclamation,
                    wave,renew,disposal,wind,mining,shipping,drilling,aquaculture,tourism,fishing)


article_summary<-all_articles%>%
  select(id,sector)%>%
  group_by(sector)%>%
  dplyr::summarise(Freq=n())%>%
  arrange(Freq)

#To plot in ascending order
article_summary$sector <- factor(article_summary$sector, 
                                 levels = article_summary$sector[order(-article_summary$Freq)])


ggplot(article_summary,aes(sector))+
  geom_bar(aes(weight=Freq),fill="darkgrey")+
  # geom_text(aes(label=Freq),stat='count') +
  theme_classic()+
  scale_y_continuous(breaks = seq(0, 155, by = 10))+
  xlab("Sector") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle=45,hjust = 1, family = "sans", size=14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())

