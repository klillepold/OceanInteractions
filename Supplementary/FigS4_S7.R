### Script for Supplementary Figures S3 and S7 (Time series) ###

#Setting up the environment
library(tidyverse)
library(splitstackshape)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(DescTools)
library(xlsx)
library(reshape2)
library(igraph)
library(gdata)
setwd("C:/Users/kate/Desktop/Ocean Stewardship/Coded Articles") #Raw MAXQDA data


#Import coded material of all coders
ICR<-read_excel('ICRround3Codes_KL.xlsx')
week27<-read_excel('Week27Codes_KL.xlsx')
week28<-read_excel('Week28Codes_KL.xlsx')
week29<-read_excel('Week29Codes_KL.xlsx')
week32<-read_excel('Week32Codes_KL.xlsx')
week33<-read_excel('Week33Codes_KL.xlsx')
week34<-read_excel('Week34Codes_KL.xlsx')
week35<-read_excel('Week35Codes_KL.xlsx')
week36<-read_excel('Week36Codes_KL.xlsx')
week37<-read_excel('Week37Codes_KL.xlsx')
week38<-read_excel('Week38Codes_KL.xlsx')
week39<-read_excel('Week39Codes_KL.xlsx')
week40<-read_excel('Week40Codes_KL.xlsx')
week41<-read_excel('Week41Codes_KL.xlsx')
week42<-read_excel('Week42Codes_KL.xlsx')
week43<-read_excel('Week43Codes_KL.xlsx')
Bea<-read_excel('BeaCodes.xlsx')
binSupps<-read_excel('binSupplementCodes_KL.xlsx')
Emmy_KL<-read_excel('emmyArticlesCodes_KL.xlsx')
Emmy<-read_excel('EmmyCodes.xlsx')
data<-rbind(ICR,week27,week28,week29,week32,week33,week34,week35,week36,week37,week38,week39,week40,week41,week42,week43,Bea,binSupps,Emmy_KL,Emmy)



#****************************************
# GENERAL DATA CLEANING
#****************************************

#Filter away non-relevant material. For this, only interaction codes are needed.
data<-data %>%
  filter(str_detect(Code, 'group1')) #this extracts any codes that have 'group 1', ie an interaction was assigned.


# Separate the codes into separate columns
data<-data %>% separate(Code, c("Sector1","Sector2","Group", "PresORInten", "Intensity"),remove = F)
data$Interaction <- paste(data$Sector1, data$Sector2, sep="-")


#Make sure there's no interactions that are switched (ex. tel-eco vs eco-tel)
listSectors<-c("agg","des","tel","mil","bio","ren","wave","wind","dril","min","ship",
               "tou","aqua","fish","eco","dred","cab","pipe","rec","disp","mpa")
AllInteractions<-sort(paste(combn(listSectors,2)[1,], combn(listSectors,2)[2,], sep="-")) #generates all possible combinations
reverseInts<-which(!(data$Interaction %in% AllInteractions)) #which rows have interactions that do not match the above list
data[reverseInts,]

#fix the reversed interactions
for(i in 1:NROW(data)){
  if(!(data$Interaction[i] %in% AllInteractions)){
    data$Interaction[i]=paste(data$Sector2[i], data$Sector1[i], sep="-")
  }
}

which(!(data$Interaction %in% AllInteractions)) #This should be 0 if they all match.



#*******************************************
# Detect year of papers and remove Eco/Mpa
#******************************************

# Detect the year of each paper
data$paperyear <- regmatches(data$`Document name`, regexpr("[[:digit:]]+", data$`Document name`))
data$paperyear<-as.numeric(data$paperyear)

# Remove eco and mpa interactions - but note that doing this will remove them as mediators too as they are not differentiated in this dataset.
# However this should be ok as the sector-[eco/mpa]-sector interactions will still have the sector-sector codes.
data2<-data[!grepl("eco", data$Interaction),]
data2<-data2[!grepl("mpa", data2$Interaction),]

#remove data point from 1978. This is because it is not a sector-sector interaction (it is min-disp-NoSector) and does not work with the below graphs.
# This needs to be noted that this is the only data point removed (other than eco and mpa interactions)
data2<-data2[data2$paperyear!= 1978,]
#Leave in the mechanisms for now, even though this means that each sector-mediator-sector interaction has 3 edges. 
# Once detecting the 'first occurrence' is done, need to remove all sector-mediator-sector interaction edges except for sector-sector edges.

#*****************************************************************************
# Detect when was the first occurrence of each sector (including mechanisms)
#*****************************************************************************

#Take away duplicates, ie do not sum multiple appearances of interactions within each paper
data_time_perArticle<-as.data.frame(data2 %>% group_by(`Document name`, Sector1, Sector2, paperyear, Interaction) %>% summarise(sumPerPaper=n()))

#Take away duplicates within each year, ie count the appearance of an interaction in a given year only once.
data_time_perYear<-as.data.frame(data_time_perArticle %>% group_by(Sector1, Sector2, paperyear, Interaction) %>% summarise(NumArticles=n()))

#Plot showing number of types of interactions per each year (this includes all mediator edges, though)
ggplot(data=data_time_perYear, aes(x=paperyear)) +
  geom_line(stat="count") +
  labs(y="distinct interaction types per year", x= "Year of Article(s)", subtitle = "")



#detect first year each sector and mediator appearred and create a label
data_time_perYear_melt<-melt(data_time_perYear, id.vars = c("paperyear","Interaction"),measure.vars = c("Sector1", "Sector2"))
sectorsPerYear<-data_time_perYear_melt %>% group_by(paperyear, value) %>% summarise(n=n())
data_time_perYear_melt <-data_time_perYear_melt  %>% arrange(value,paperyear)
data_time_perYear_melt$firstSector <- as.numeric(!duplicated(data_time_perYear_melt$value))
data_time_perYear_melt$SectorLabel <-ifelse(data_time_perYear_melt$firstSector==1, data_time_perYear_melt$value,NA)
labelPoints<-data_time_perYear_melt[!is.na(data_time_perYear_melt$SectorLabel),]

#add in full names for the labels
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="agg", "Aggregates", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="aqua", "Aquaculture", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="bio", "Bioprospecting", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="cab", "Cables", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="des", "Desalination", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="disp", "Disposal", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="dred", "Dredging", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="dril", "Drilling", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="fish", "Fishing", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="mil", "Military", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="min", "Mining", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="pipe", "Pipelines", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="rec", "Reclamation", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="ren", "Renewable Energy (other)", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="ship", "Shipping", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="tel", "Telecommunications", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="tou", "Tourism", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="wave", "Wave Energy", labelPoints$SectorLabel)
labelPoints$SectorLabel<- ifelse(labelPoints$SectorLabel=="wind", "Wind Energy", labelPoints$SectorLabel)
labelPoints
data_time_perYear_melt %>% arrange(paperyear)

#Now combine labels for years where there are multiple
labelPointsCombo<-as.data.frame(labelPoints %>% arrange(paperyear) %>%  group_by(paperyear) %>% mutate(SectorLabelCombo=paste(unlist(SectorLabel), collapse =" ")))
#labelPoints %>% arrange(paperyear) %>%  group_by(paperyear) %>% mutate(SectorLabel2=paste(unlist(SectorLabel), collapse =",")) %>% mutate(SectorLabelCombo=paste0("atop(",SectorLabel2)) %>% mutate(SectorLabelCombo=paste0(SectorLabelCombo,")"))




labelPointsCombo
labelPointsCombo<-labelPointsCombo %>% distinct(SectorLabelCombo, .keep_all = T)
labelPointsCombo<-labelPointsCombo[,-which(names(labelPointsCombo) %in% c("Interaction","variable","value","firstSector","SectorLabel"))]



#detect first year each sector-sector interaction appearred 
#at this point, remove the mediators, as we are just interested in new sector-sector interactions.
data_time_perYear_SOnly<-data_time_perYear[with(data_time_perYear,!grepl("dred|cab|pipe|rec|disp", paste(Sector1,Sector2))),]
data_time_perYear_SOnly <- data_time_perYear_SOnly %>% arrange(Interaction, paperyear)
data_time_perYear_SOnly$firstInteraction <- as.numeric(!duplicated(data_time_perYear_SOnly$Interaction))
NewInteractions_perYear <- data_time_perYear_SOnly %>% group_by(paperyear) %>% summarise(NumNewInteractions=sum(firstInteraction))
NewInteractions_perYear %>% arrange(paperyear) #it removes 1978 because it was a disp interaction..



#**********************************************************************
# Create a cumulative count of number of distinct interactions per year
#**********************************************************************

data_time_perYear_Sum <- data_time_perYear_SOnly %>% group_by(paperyear)%>% summarise(NumDistinctInts=n())
data_time_perYear_Cum <- data_time_perYear_Sum %>% arrange(paperyear) %>% group_by(paperyear) 
data_time_perYear_Cum$CumDistinct <- cumsum(data_time_perYear_Sum$NumDistinctInts) 
#merge on the labels
data_time_perYear_Cum_labels<-merge(x=data_time_perYear_Cum, y= labelPointsCombo, by.x = c("paperyear"), by.y= c("paperyear"), all.x = T, all.y = T) 


#**********************************************************************
# Create a cumulative count of number of distinct *new* interactions per year
#**********************************************************************
NewInteractions_perYear$CumNew <- cumsum(NewInteractions_perYear$NumNewInteractions) 
NewInteractions_perYear

#Overlay labels 
NewInteractions_perYear_labels<-merge(x=NewInteractions_perYear, y= labelPointsCombo, by.x = c("paperyear"), by.y= c("paperyear"), all.x = T, all.y = T) 
NewInteractions_perYear_labels

tsplot1<-ggplot(data=NewInteractions_perYear_labels, aes(x=paperyear, y=CumNew)) +
  geom_line(size = 1.5) +
  # geom_point(size=1) +
  geom_point(data=NewInteractions_perYear_labels[!is.na(NewInteractions_perYear_labels$SectorLabelCombo), ], aes(x=paperyear, y=CumNew), colour="lightblue", size=6) +
  # geom_text(label=stringr::str_wrap(NewInteractions_perYear_labels$SectorLabelCombo,4), nudge_y = 6, size= 3) +
  geom_text_repel(aes(label = stringr::str_wrap(NewInteractions_perYear_labels$SectorLabelCombo,8)), 
                  nudge_y      = 3,
                  nudge_x = -4,
                  direction    = "x",
                  vjust        = 0,
                  size = 6, 
                  # segment.size = 0.2,
                  # point.padding = unit(0.80, "line")
  ) +
  scale_x_continuous(breaks = round(seq(min(NewInteractions_perYear_labels$paperyear), max(NewInteractions_perYear_labels$paperyear), by = 2),1)) +
  #ggtitle("New Interactions in Reviewed Literature (Cumulative)") +
  labs(x= "Year", y= "New Interaction Types") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=16))
tsplot1







#*******************************************************************
# Part 2: Time in literature vs number of interactions
#*******************************************************************

YearsInLit<-labelPoints %>% arrange(paperyear) %>% mutate(YearsInLit=2018-paperyear)
YearsInLit<-YearsInLit[with(YearsInLit,!grepl("dred|cab|pipe|rec|disp", value)),]

#Take away all duplicate interactions 
NumInts<-as.data.frame(data_time_perArticle %>% group_by(Sector1, Sector2, Interaction) %>% summarise(NumInts=n()))
NumInts<-melt(NumInts, id.vars = c("Interaction"),measure.vars = c("Sector1", "Sector2"))
NumInts<- NumInts %>% group_by(value) %>% summarise(NumInts=n())


Correlation<- merge(NumInts, YearsInLit, by.x = "value", by.y = "value")
Correlation

#Bring in the colours I want
listSectors<-c("wave","wind","ren","min","bio","rec","disp","dred","cab","pipe","mil","ship","fish","tel","dril","agg","des",
               "tou","aqua")
colourGroups<-c("Renewables","Renewables","Renewables", "DeepSea","DeepSea", "Mediators", "Mediators", "Mediators", "Mediators", "Mediators",
                "historical","historical","historical","historical","historical","other","other","other","other")
colours<-c("#7ECD9D","#258A4D","#19F570","#190A79","#165CF6","#C2CE61","#CEF244","#D4E055","#F4F711","#F7E211","#A08E74","#DDE456",
           "#F75011", "#E5C07A", "#664C20", "#9112D7", "#E47AE5", "#17879E", "#64E7F2")
sectorColours<-as.data.frame(cbind(listSectors, colourGroups, colours))
Correlation<-merge(Correlation, sectorColours, by.x="value", by.y = "listSectors", all.y=F)
Correlation<-Correlation %>% mutate(SectorLabel = case_when(SectorLabel == "Aggregates"~"Aggregate mining",
                                               SectorLabel ==  "Drilling"~"Fossil fuels energy",
                                               SectorLabel == "Military"~"Military activities",
                                               SectorLabel == "Mining"~"Metal/minerals mining",
                                               SectorLabel == "Renewable Energy (other)"~"Renewable energy",
                                               TRUE ~ as.character(.$SectorLabel))) # Assign full name to labels as there is room in the legend.


tsplot2<-ggplot(data = Correlation, aes(x=YearsInLit, y=NumInts, colour=colours)) +
  geom_point(aes(colour=SectorLabel), size = 3.5, position=position_jitter(h=0.5, w=0.5)) +
  geom_smooth(method = "lm", se = F, colour = "grey", size=1) + 
  #geom_text(label=Correlation$value) +
  scale_colour_manual(breaks = Correlation$SectorLabel, 
                      values = unique(as.character(Correlation$colours))) +
  scale_x_continuous(breaks = round(seq(min(Correlation$YearsInLit), max(Correlation$YearsInLit), by = 2),1))+ 
  labs(x="Years Present in Literature", y="Interaction Types", colour = "Sector")  +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=16))
tsplot2


#Correlation
# Shapiro-Wilk normality test for mpg
shapiro.test(Correlation$NumInts) # => p = 0.01, not normally distributed
# Shapiro-Wilk normality test for wt
shapiro.test(Correlation$YearsInLit) # => p = 0.003, not normally distributed
#Therefore can't use Pearsons, need to use nonparamtric.
#also there are ties in the data (points with the same values) so need to use Kendall
cor.test(Correlation$YearsInLit, Correlation$NumInts, method = c("kendall"))


