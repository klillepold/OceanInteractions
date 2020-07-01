### Figure 1 b
### Network of direct and indirect interactions (eco and mpa included as both types of nodes)

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
setwd("C:/Users/kate/Desktop/") #using a local copy 
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





################################################################################################################
#### Network Diagram #3 - Same layout as network 2 but with centrality for collapsed network, and eco/Mpa included
###############################################################################################################

#Step 1: Create the links dataset (Do direct and mediated separately)

### Direct ###
directlinks<-interactions_full %>% filter(is.na(med_node)) #filter for the direct interactions only
directlinks$From<- ifelse(is.na(directlinks$From), sub("\\-.*", "", directlinks$Interaction), directlinks$From)
directlinks$To<- ifelse(is.na(directlinks$To), sub(".*-", "", directlinks$Interaction), directlinks$To)
directlinks$EdgeType<-"Direct"
names(directlinks)[names(directlinks) == 'From'] <- 'from'
names(directlinks)[names(directlinks) == 'To'] <- 'to'

#have to create the extra lines so there are double arrows
directlinks_igraph <- directlinks %>% add_row(rowid=nrow(directlinks) + 1, Interaction= "tel-xxx", from = "tel",to="tel") #add a blank row with tel as a node so that it is included in the diagram.
directlinks_igraph <- directlinks_igraph %>% add_row(rowid=nrow(directlinks_igraph) + 1, Interaction= "bio-xxx", from = "bio", to="bio")

#Have to add an extra edge for bidirectional interactions - direct links first
directlinks_igraph_bi<-directlinks_igraph[directlinks_igraph$Bidirectional==1,]
directlinks_igraph_bi2<-directlinks_igraph_bi
directlinks_igraph_bi2$from<-directlinks_igraph_bi$to
directlinks_igraph_bi2$to<-directlinks_igraph_bi$from
directlinks_igraph_bi2
directlinks_igraph<-rbind(directlinks_igraph,directlinks_igraph_bi2)
directlinks_igraph<-directlinks_igraph[!is.na(directlinks_igraph$from), ] # exclude blank rows if there are any.

### Mediated ###

# Set up links for the mediated interactions separately, since there are two 'pathways' per interaction (sector1-mediator & mediator-sector2)
# E.g. tou-->mpa-->fish, will have a tou-->mpa AND a mpa-->fish arrow.
medlinks<-interactions_full %>% filter(!is.na(med_node))

# oD stands for one-directional and bD stands for bi-directional.
medlinks[c("oD_a", "oD_b", "bD_a", "bD_b")]<-NA
medlinks$oD_a<-ifelse(medlinks$Bidirectional==0,paste(medlinks$From,medlinks$med_node,sep = "_"),NA)
medlinks$oD_b<-ifelse(medlinks$Bidirectional==0,paste(medlinks$med_node,medlinks$To,sep = "_"),NA)
medlinks$bD_a<-ifelse(medlinks$Bidirectional==1,paste(sub("\\-.*", "", medlinks$Interaction),medlinks$med_node,sep = "_"),NA)
medlinks$bD_b<-ifelse(medlinks$Bidirectional==1,paste(medlinks$med_node,sub(".*-", "", medlinks$Interaction),sep = "_"),NA)
medlinks[c("From","To")]<-NULL #get rid of the previous From To columns because now we have new ones that explicitly show the two pathways.



medlinks_melt<-melt(medlinks, id.vars=c("rowid","Interaction","Outcome",
                                        "Bidirectional", #make each link its own row.
                                        "med_node","outcome_direction"))
medlinks_melt<-medlinks_melt[order(medlinks_melt$rowid),]
medlinks_melt<-medlinks_melt[!is.na(medlinks_melt$value),] #now the 'value' column shows each link. We still have the 'bidirectional' column for indicating directionality
medlinks_melt$variable<-NULL
medlinks_melt$EdgeType<-"Mediated"
medlinks_melt<- medlinks_melt %>% separate(value, c("from", "to")) #finally, create the from and to columns that show all the mediated links.

#Have to add an extra edge for bidirectional interactions 
medlinks_melt_igraph<-medlinks_melt
medlinks_melt_igraph_bi<-medlinks_melt_igraph[medlinks_melt_igraph$Bidirectional==1,]
medlinks_melt_igraph_bi2<-medlinks_melt_igraph_bi
medlinks_melt_igraph_bi2$from<-medlinks_melt_igraph_bi$to
medlinks_melt_igraph_bi2$to<-medlinks_melt_igraph_bi$from
medlinks_melt_igraph_bi2
medlinks_melt_igraph<-rbind(medlinks_melt_igraph,medlinks_melt_igraph_bi2)


#### Bind Direct and Mediated together ####
links_igraph<-rbind(directlinks_igraph, medlinks_melt_igraph)






# Step 2a: Create the Nodes dataset.

sectorsOnly<-c("agg","des","tel","mil","bio","ren","wave","wind","dril","min","ship",
               "tou","aqua","fish")
meds<-c("dred","cab","pipe","rec","disp","mpa","eco")
id<-c("agg","des","tel","mil","bio","ren","wave","wind","dril","min","ship",
      "tou","aqua","fish","dred","cab","pipe","rec","disp","mpa","eco")
nodes_full<-as.data.frame(id)
nodes_full$sector<-nodes_full$id
nodes_full$sector.type<-NA
nodes_full$sector.label<-NA
for(i in 1:NROW(nodes_full)){
  if(nodes_full$id[i] %in% sectorsOnly){
    nodes_full$sector.type[i]<-1
    nodes_full$sector.label[i]<-"sec"
  }else{nodes_full$sector.type[i]<-2
  nodes_full$sector.label[i]<-"med"
  }
}
nodes_full

# Step 2b: Calculate the centrality for node sizes. Have to use a collapsed link dataset for this.

## nodes collapsed ##
id_coll<-c("agg","des","tel","mil","bio","ren","wave","wind","dril","min","ship", #now have eco and mpa included.
           "tou","aqua","fish","mpa","eco")
nodes_coll<-as.data.frame(id_coll) #coll stands for collapsed.

## Collapsed full links##
directlinks_cent<-interactions_full %>% filter(is.na(med_node)) #filter for the direct interactions only
directlinks_cent$From<- ifelse(is.na(directlinks_cent$From), sub("\\-.*", "", directlinks_cent$Interaction), directlinks_cent$From)
directlinks_cent$To<- ifelse(is.na(directlinks_cent$To), sub(".*-", "", directlinks_cent$Interaction), directlinks_cent$To)
directlinks_cent$EdgeType<-"Direct"
names(directlinks_cent)[names(directlinks_cent) == 'From'] <- 'from'
names(directlinks_cent)[names(directlinks_cent) == 'To'] <- 'to'

medlinks_cent<-interactions_full %>% filter(!is.na(med_node))
medlinks_cent$From<- ifelse(is.na(medlinks_cent$From), sub("\\-.*", "", medlinks_cent$Interaction), medlinks_cent$From)
medlinks_cent$To<- ifelse(is.na(medlinks_cent$To), sub(".*-", "", medlinks_cent$Interaction), medlinks_cent$To)
medlinks_cent$EdgeType<-"Indirect"
names(medlinks_cent)[names(medlinks_cent) == 'From'] <- 'from'
names(medlinks_cent)[names(medlinks_cent) == 'To'] <- 'to'
links_full_cent<-rbind(directlinks_cent,medlinks_cent)


## Centrality Calculation - sectors ##
net2 <- graph_from_data_frame(d=links_full_cent[,c("from","to")], vertices=nodes_coll,directed=F)

V(net2)$indegree <-centr_degree(net2,mode = "all")$res
net.nodes2 <- get.data.frame(net2, what="vertices")
net.nodes2 <- data.frame(id = net.nodes2$name,indegree = net.nodes2$indegree)
setnames(net.nodes2, "indegree", "Degreecentrality_coll")
net.nodes2 <- net.nodes2[order(net.nodes2$Degreecentrality, decreasing = F),]


## Centrality Calculation - Mediators ##
centr_meds<-as.data.frame(interactions_full %>% filter(!is.na(med_node)) %>% group_by(med_node) %>% summarize(Degreecentrality_coll=n()))
centr_meds<-centr_meds %>% rename(id=med_node)
net.nodes2<-rbind(net.nodes2, centr_meds)
net.nodes2<-as.data.frame(net.nodes2 %>% group_by(id) %>% summarise(Degreecentrality_coll=sum(Degreecentrality_coll)))


# Assign node sizes based on centrality.

net.nodes2$Range<- cut(net.nodes2$Degreecentrality,c(0,10,20,30,100))
sizes<-data.frame("Range"=c(NA,"(0,10]", "(10,20]", "(20,30]","(30,100]"), size=c(20,27.5,35,42.5,50))

net.nodes2<-left_join(net.nodes2, sizes, by="Range")

nodes_full_igraph <-nodes_full
nodes_full_igraph<-left_join(nodes_full_igraph, net.nodes2, by="id")
#nodes_full_igraph$shape<-ifelse(nodes_full_igraph$sector.label=="med","rectangle","circle") #run if both circles and rectangles are wanted
#nodes_full_igraph$shape<-ifelse(nodes_full_igraph$id=="eco"|nodes_full_igraph$id=="mpa", "circle",nodes_full_igraph$shape) #run if both circles and rectangles are wanted
nodes_full_igraph$sector.label<-ifelse(nodes_full_igraph$id=="eco"|nodes_full_igraph$id=="mpa", "Biosphere",nodes_full_igraph$sector.label)
#nodes_full_igraph$shape<-ifelse(nodes_full_igraph$sector.label=="Biosphere", "circle",nodes_full_igraph$shape) #run if both circles and rectangles are wanted






################ Setting up the inner circle layout ############################
# edges within nodes1

linksfull1 <- links_igraph[links_igraph$from %in% (nodes_full_igraph[nodes_full_igraph$sector.label=="sec",])$sector & links_igraph$to%in%(nodes_full_igraph[nodes_full_igraph$sector.label=="sec",])$sector, ] 
linksfull1 <- linksfull1 %>% add_row(rowid=nrow(linksfull1) + 1, Interaction= "tel-xxx", from = "tel",to="tel") #add a blank row with tel as a node so that it is included in the diagram.
linksfull1 <- linksfull1 %>% add_row(rowid=nrow(linksfull1) + 1, Interaction= "bio-xxx", from = "bio",to="bio") #add a blank row with tel as a node so that it is included in the diagram.


# edges within nodes2 
linksfull2 <- links_igraph[links_igraph$from %in% (nodes_full_igraph[nodes_full_igraph$sector.label=="med",])$sector & links_igraph$to%in%(nodes_full_igraph[nodes_full_igraph$sector.label=="med",])$sector, ]
# edges within nodes1 or 2 = all edges if nodes1, 2 account for all nodes
linksfull12 <-links_igraph[links_igraph$from%in%nodes_full_igraph$id &
                             links_igraph$to%in%nodes_full_igraph$id, ]


igraph1 <- graph_from_data_frame(d=linksfull1[,c("from","to")], vertices=nodes_full_igraph[nodes_full_igraph$sector.label=="sec",], directed = T)
igraph2 <- graph.data.frame(linksfull2, directed=T, vertices=meds)

# inner circle can connect to outer:
igraph12b <- graph.data.frame(linksfull12[,c("from","to")], directed = F, vertices =
                                nodes_full_igraph$id)

l1 = layout.circle(igraph1) 

l2 = layout.circle(igraph2)
l12 = rbind(l1, l2*0.45) # 0.5 term scales the inner circle size

# plot both circles with possible connections between them
plot.igraph(igraph12b, layout = l12, rescale = F)

###########################################







#Step 3: Create the network

links_igraph$EdgeType<-ifelse(is.na(links_igraph$EdgeType),"Direct",links_igraph$EdgeType)
#links_igraph$color<-ifelse(links_igraph$to=="eco","#1874CD","#A9A9A9") #run this line if you want blue lines for lines going into eco
#links_igraph$color<-ifelse(links_igraph$to=="mpa","orange",links_igraph$color) #run this line if you want orange lines for lines going into mpa
links_igraph$color<-ifelse(links_igraph$to=="eco"|links_igraph$to=="mpa","#1874CD","#ABABAB") #run this lines if you want blue lines for lines going into mpa AND eco


links_igraph<-links_igraph[!(links_igraph$from=="bio" & links_igraph$to=="bio"),] #remove,dummy row not needed anymore
links_igraph<-links_igraph[!(links_igraph$from=="tel" & links_igraph$to=="tel"),]
igraph1c <- graph_from_data_frame(d=links_igraph[,c("from","to")], vertices=nodes_full_igraph)
igraph1c  <- igraph::simplify(igraph1c, remove.multiple = F, remove.loops = T) 

E(igraph1c) %>% print(full = TRUE)

#Set the parameters

V(igraph1c)$name
V(igraph1c)$size <- nodes_full_igraph$size
V(igraph1c)$color<-c("lightyellow","gray","lightblue")[as.factor(nodes_full_igraph$sector.label)]
#V(igraph1c)$shape<-c("circle","rectangle","circle")[as.factor(nodes_full_igraph$sector.label)] #run this line if you want rectanges for mediators and circles for biosphere/sectors
V(igraph1c)$shape <- "circle"
E(igraph1c)$lty <- c("solid","dashed")[as.factor(links_igraph$EdgeType)] # Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed", 3 or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
#E(igraph1c)$color<- c("#1874CD", "#A9A9A9", "orange")[as.factor(as.character(links_igraph$color))] #run this line if you want blue or orange lines going into eco/mpa
#E(igraph1c)$color<- "#CFD0D2" #light grey lines #use this line if you want only grey lines in the network
E(igraph1c)$color<-c("#1761A7", "#ABABAB")[as.factor(as.character(links_igraph$color))]
E(igraph1c)$width<-c(3,1)[as.factor(as.character(links_igraph$color))] #run if you want thicker edges for mpa and eco.
E(igraph1c)$curved<-curve_multiple(igraph1c,start=0.2) #this curves lines that are double lines pointing in the same direction
windowsFonts("Calibri" = windowsFont("Calibri"))

graph_attr(igraph1c, "layout") <- l12
plot.igraph(igraph1c,asp=1,
            vertex.frame.color="black",
            vertex.label.color="black",
            vertex.label.font=2, #bold
            vertex.label.family="Calibri",
            vertex.label.cex=1,
            edge.arrow.size=0.3,                           # Arrow size, defaults to 1
            edge.arrow.width=0.5                          # Arrow width, defaults to 1
)


#EXPORT SIZE W918 H741













