### Figure 3
### Network of specific examples by outcome type

#Setting up the environment
library(tidyverse)
library(splitstackshape)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DescTools)
library(xlsx)
library(reshape2)
library(igraph)
library(visNetwork)
library(gridExtra)
library(magrittr) 
library(data.table)
setwd("C:/Users/kate/Desktop/") #using a local copy just for script writing
Direct<-read_excel("final_interactiondata_METADATA_LOCALCOPY_june2020.xlsx",sheet=3)
Mediators<-read_excel("final_interactiondata_METADATA_LOCALCOPY_june2020.xlsx",sheet=4)
colnames(Mediators) <- Mediators[1, ] # fix the header on the mediator df
Mediators <- Mediators[-1, ] 
Mediators<-Mediators[!is.na(Mediators$`Mediator Node`), ] # exclude blank rows if there are any.

#Merge into one dataframe 
direct_clean<-Direct%>%
  select(Interaction,Outcome,From, To, Bidirectional) %>% 
  mutate(med_node=NA)

mediators_clean<-Mediators%>%
  rename(med_node=`Mediator Node`)%>%
  select(Interaction,Outcome,From, To, Bidirectional,med_node) 

interactions_full<-rbind(direct_clean,mediators_clean)%>% #bind, then add whether each interaction is positive or negative.
  rowid_to_column() %>% 
  mutate(outcome_direction=case_when(Outcome=="space-crowd"|Outcome=="space-ex"|Outcome=="natcap-dimin"|Outcome=="operation-dimin"|
                                       Outcome=="value-dimin"~"negative",
                                     Outcome=="space-syn"|Outcome=="natcap-enhance"|Outcome=="operation-enhance"|
                                       Outcome=="value-enhance"~"positive")) 





# Step 1: Create Nodes dataset.
id<-c("agg","des","tel","mil","bio","ren","wave","wind","dril","min","ship", #now have eco and mpa included.
      "tou","aqua","fish","mpa","eco")
nodes_coll<-as.data.frame(id) #coll stands for collapsed.



#Step 2: Create the links dataset.

# Direct Interactions
directlinks<-interactions_full %>% filter(is.na(med_node)) 

directlinks$From<- ifelse(is.na(directlinks$From), sub("\\-.*", "", directlinks$Interaction), directlinks$From)
directlinks$To<- ifelse(is.na(directlinks$To), sub(".*-", "", directlinks$Interaction), directlinks$To)
directlinks$EdgeType<-"Direct"
names(directlinks)[names(directlinks) == 'From'] <- 'from'
names(directlinks)[names(directlinks) == 'To'] <- 'to'


# Mediated Interactions
mechlinks<-interactions_full %>% filter(!is.na(med_node))
mechlinks$From<- ifelse(is.na(mechlinks$From), sub("\\-.*", "", mechlinks$Interaction), mechlinks$From)
mechlinks$To<- ifelse(is.na(mechlinks$To), sub(".*-", "", mechlinks$Interaction), mechlinks$To)
mechlinks$EdgeType<-"Indirect"
names(mechlinks)[names(mechlinks) == 'From'] <- 'from'
names(mechlinks)[names(mechlinks) == 'To'] <- 'to'

#Bind together
links_full_coll<-rbind(directlinks,mechlinks)

#Have to add an extra edge for bidirectional interactions 
links_full_coll_bi<-links_full_coll[links_full_coll$Bidirectional==1,]
links_full_coll_bi2<-links_full_coll_bi
links_full_coll_bi2$from<-links_full_coll_bi$to
links_full_coll_bi2$to<-links_full_coll_bi$from
links_full_coll_bi2
links_full_coll<-rbind(links_full_coll,links_full_coll_bi2)
links_full_coll<-links_full_coll[!is.na(links_full_coll$from), ] # exclude blank rows if there are any.




##############################################################################################
### Sub-graphs: show a plot for each type of outcome.
##############################################################################################

# Subset for each type of interaction

Space<-subset(links_full_coll, grepl("space", links_full_coll$Outcome))
Space$color=c("red", "forestgreen")[as.factor(Space$outcome_direction)]
Operations<-subset(links_full_coll, grepl("operation", links_full_coll$Outcome))
Operations$color=c("red", "forestgreen")[as.factor(Operations$outcome_direction)]
Natcap<-subset(links_full_coll, grepl("natcap", links_full_coll$Outcome))
Natcap$color=c("red", "forestgreen")[as.factor(Natcap$outcome_direction)]
Value<-subset(links_full_coll, grepl("value", links_full_coll$Outcome))
Value$color=c("red", "forestgreen")[as.factor(Value$outcome_direction)]


#############################################################
## Specific examples
############################################################

# Natcap fish examples - none are bidirectional.
natcap_fish<-subset(Natcap, grepl("fish", Natcap$to))
natcap_fish_nodes<-as.data.frame(unique(c(natcap_fish$to,natcap_fish$from))) 
names(natcap_fish_nodes)[1] <- "id"
natcap_fish_nodes$label<-natcap_fish_nodes$id


# igraph version
#Static igraph version
natcapfishlinks_igraph<-natcap_fish[,-c(1,2,3)]
igraphnatcap_fish <- graph_from_data_frame(d=natcapfishlinks_igraph, vertices=natcap_fish_nodes)
igraphnatcap_fish  <- simplify(igraphnatcap_fish , remove.multiple = F, remove.loops = T) 
lines<-as.factor(as.character((c(1,2))))
E(igraphnatcap_fish)$lty <- lines[as.factor(E(igraphnatcap_fish)$EdgeType)]
graph_attr(igraphnatcap_fish, "layout") <- layout_as_star
plot(igraphnatcap_fish ,edge.arrow.size=1,vertex.color="lightblue",
     vertex.frame.color="black",
     vertex.label.color="black",edge.width=2,vertex.label.font=2,vertex.label.cex=.8,vertex.size=20) 




# value tou example - none are bidirectional
value_tou<-subset(Value, grepl("tou", Value$to))
value_tou_nodes<-as.data.frame(unique(c(value_tou$to,value_tou$from))) 
names(value_tou_nodes)[1] <- "id"
value_tou_nodes$label<-value_tou_nodes$id
value_toulinks_igraph<-value_tou[,-c(1,2,3)]
igraphvalue_tou<- graph_from_data_frame(d=value_toulinks_igraph, vertices=value_tou_nodes)
igraphvalue_tou  <- simplify(igraphvalue_tou , remove.multiple = F, remove.loops = T) 
lines<-as.factor(as.character((c(1,2))))
E(igraphvalue_tou )$lty <- lines[as.factor(E(igraphvalue_tou )$EdgeType)]
graph_attr(igraphvalue_tou, "layout") <- layout_as_star
plot(igraphvalue_tou ,edge.arrow.size=1,vertex.color="lightblue",
     vertex.frame.color="black",
     vertex.label.color="black",edge.width=2,vertex.label.font=2,vertex.label.cex=.8,vertex.size=20)




# space synergy example
spacesyn <- Space[ (grepl("syn",Space$Outcome)),]
spacesyn_nodes<-as.data.frame(unique(c(spacesyn$to,spacesyn$from))) 
names(spacesyn_nodes)[1] <- "id"
spacesyn_nodes$label<-spacesyn_nodes$id
spacesynlinks_igraph<-spacesyn[,-c(1,2,3)]
spacesynlinks_igraph$Bidirectional<-ifelse(is.na(spacesynlinks_igraph$Bidirectional),0,1)
igraphspacesyn<- graph_from_data_frame(d=spacesynlinks_igraph, vertices=spacesyn_nodes)
igraphspacesyn  <- simplify(igraphspacesyn, remove.multiple = F, remove.loops = T) 
lines<-as.factor(as.character((c(1,2))))
E(igraphspacesyn)$lty <- lines[as.factor(E(igraphspacesyn)$EdgeType)]
arrows<-(as.factor(as.numeric((c(2,3)))))
E(igraphspacesyn)$arrow.mode <- as.numeric(as.character(arrows[as.factor(as.character(E(igraphspacesyn)$Bidirectional))])) #this is ugly but iam rushed
graph_attr(igraphspacesyn, "layout") <- layout_with_lgl
plot(igraphspacesyn ,edge.arrow.size=1,vertex.color="lightblue",
     vertex.frame.color="black",
     vertex.label.color="black",edge.width=2,vertex.label.font=2,vertex.label.cex=.8,vertex.size=25)



# Operations cables example
Operations<-subset(links_full_coll, grepl("operation", links_full_coll$Outcome))
Operations$color=c("red", "forestgreen")[as.factor(Operations$outcome_direction)]

operations_cables <- Operations[ (grepl("cab",Operations$med_node)),]
operations_cables_nodes<-as.data.frame(unique(c(operations_cables$to,operations_cables$from))) 
names(operations_cables_nodes)[1] <- "id"
operations_cables_nodes$label<-operations_cables_nodes$id
operations_cableslinks_igraph<-operations_cables[,-c(1,2,3)]
operations_cableslinks_igraph$Bidirectional<-ifelse(is.na(operations_cableslinks_igraph$Bidirectional),0,1)
igraphoperations_cables<- graph_from_data_frame(d=operations_cableslinks_igraph, vertices=operations_cables_nodes)
igraphoperations_cables  <- simplify(igraphoperations_cables, remove.multiple = F, remove.loops = T) 
arrows<-(as.factor(as.numeric((c(2,3)))))
E(igraphoperations_cables)$arrow.mode <- as.numeric(as.character(arrows[as.factor(as.character(E(igraphoperations_cables)$Bidirectional))])) #this is ugly but iam rushed
graph_attr(igraphoperations_cables, "layout") <- layout_with_lgl
plot(igraphoperations_cables,edge.arrow.size=1,vertex.color="lightblue",
     vertex.frame.color="black",
     vertex.label.color="black",edge.width=3,vertex.label.font=2,vertex.label.cex=1,vertex.size=30,edge.lty=3)







