### Figure 1 a
### Network of direct sector-sector interactions Only (eco and mpa not included)

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

#Import the data from https://doi.org/10.7910/DVN/SI6TUS

Direct<-read_excel("Data_Repository_PART_IIII_OceanInteractionData.xlsx",sheet=2)
Mediators<-read_excel("Data_Repository_PART_IIII_OceanInteractionData.xlsx",sheet=3)
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





########################################################################
# Put into a network-compatible format
########################################################################

# Step 1: Create links dataset

# Specify from/to for bidirectional interactions, these interactions will get extra edges so that it has an arrow both ways.
# Unidirectional interactions will only have one edge so that there is only one arrow.
directlinks<-interactions_full %>% filter(is.na(med_node)) #filter for the direct interactions only
directlinks$From<- ifelse(is.na(directlinks$From), sub("\\-.*", "", directlinks$Interaction), directlinks$From)
directlinks$To<- ifelse(is.na(directlinks$To), sub(".*-", "", directlinks$Interaction), directlinks$To)
directlinks$EdgeType<-"Direct"
directlinks<-directlinks[!(directlinks$From %in% 
                             c("mpa","eco"))&!(directlinks$To %in% c("mpa","eco")), ] #run if we don't want eco or mpa as nodes

names(directlinks)[names(directlinks) == 'From'] <- 'from'
names(directlinks)[names(directlinks) == 'To'] <- 'to'
directlinks$smooth<- T

directlinks
directlinks_igraph <- directlinks %>% add_row(rowid=nrow(directlinks) + 1, Interaction= "tel-xxx", from = "tel",to="tel") #add a blank row with tel as a node so that it is included in the diagram.
directlinks_igraph <- directlinks_igraph %>% add_row(rowid=nrow(directlinks_igraph) + 1, Interaction= "bio-xxx", from = "bio", to="bio")


#add an extra edge pointing the opposite way for bidirectional interactions
directlinks_igraph_bi<-directlinks_igraph[directlinks_igraph$Bidirectional==1,]
directlinks_igraph_bi2<-directlinks_igraph_bi
directlinks_igraph_bi2$from<-directlinks_igraph_bi$to
directlinks_igraph_bi2$to<-directlinks_igraph_bi$from
directlinks_igraph_bi2
directlinks_igraph<-rbind(directlinks_igraph,directlinks_igraph_bi2)
directlinks_igraph<-directlinks_igraph[!is.na(directlinks_igraph$from), ] # exclude blank rows if there are any.



# Step 2a: Create nodes dataset (all sectors, no eco or mpa)
id<-c("agg","des","mil","bio","ren","wave","wind","dril","min","ship","tou","aqua","fish", "tel") 
nodes_direct<-as.data.frame(id)
nodes_direct$label<-nodes_direct$id

# Step 2b: Calculate degree centrality in order to specify node size.

net_direct <- graph_from_data_frame(d=directlinks[,c("from","to")], vertices=nodes_direct,directed=F)
V(net_direct)$indegree <-centr_degree(net_direct,mode = "all")$res
net_direct.nodes1 <- get.data.frame(net_direct, what="vertices")
net_direct.nodes1 <- data.frame(id = net_direct.nodes1$name, title = net_direct.nodes1$name, indegree = net_direct.nodes1$indegree)
setnames(net_direct.nodes1, "indegree", "Degreecentrality")
net_direct.nodes1 <- net_direct.nodes1[order(net_direct.nodes1$Degreecentrality, decreasing = F),]
net_direct.nodes1$Range<- cut(net_direct.nodes1$Degreecentrality,c(0,10,20,30,50))

#Set the vertex sizes
sizes<-data.frame("Range"=c(NA,"(0,10]", "(10,20]", "(20,30]","(30,50]"), size=c(20,27.5,35,42.5,50))
net_direct.nodes1<-left_join(net_direct.nodes1, sizes, by="Range")
nodes_direct<-left_join(nodes_direct, net_direct.nodes1, by="id")

nodes_direct






# Step 3: Set the igraph attributes
igraph1a <- graph_from_data_frame(d=directlinks_igraph[,c("from","to")], vertices=nodes_direct)
igraph1a  <- igraph::simplify(igraph1a, remove.multiple = F, remove.loops = T) 

V(igraph1a)$name
V(igraph1a)$size <- nodes_direct$size
E(igraph1a)$curved<-curve_multiple(igraph1a,start=0.2) #this curves lines that are double lines pointing in the same direction
windowsFonts("Calibri" = windowsFont("Calibri"))

#Use the exact layout positioning of the outer nodes in Figs 1b and 1c so that it is consistent all across.
cola<-matrix(c(1.0000000,0.9009689,   0.2225209, -0.2225209, -0.6234898, -0.9009689, -1.0000000, -0.9009689, -0.6234898, -0.2225209,  0.2225209,  0.6234898,  0.9009689, 0.6234898 ),nrow=14,ncol=1)
colb<-matrix(c(0.000000e+00, 4.338837e-01,   9.749279e-01 , 9.749279e-01 , 7.818315e-01,  4.338837e-01,  1.224606e-16, -4.338837e-01, -7.818315e-01, -9.749279e-01, -9.749279e-01,-7.818315e-01, -4.338837e-01,7.818315e-01),nrow=14,ncol=1)
l1<-cbind(cola,colb)
layout.circle(igraph1a)

graph_attr(igraph1a, "layout") <- l1

plot(igraph1a,vertex.color="lightblue", asp=1,
     vertex.frame.color="black",
     vertex.label.color="black",
     vertex.label.font=2, #bold
     vertex.label.family="Calibri",
     vertex.label.cex=1,
     edge.color="#ABABAB",           # Edge color
     edge.width=1,                        # Edge width, defaults to 1
     edge.arrow.size=0.3,                           # Arrow size, defaults to 1
     edge.arrow.width=0.5,                          # Arrow width, defaults to 1
     edge.lty="solid"                           # Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed", 3 or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
)


# Plots are combined in Inkscape
#***********************************************






