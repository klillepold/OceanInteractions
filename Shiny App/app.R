### R Shiny App for Ocean Interactions
### Link to App: https://gedb.shinyapps.io/OceanInteractions/


# Set up environment
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(igraph)
library(visNetwork)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringr)
library(rsconnect)
library(shinyWidgets)
#setwd("C:/Users/kate/Desktop/Ocean Stewardship/ocean_stew/Shiny app") # Note: Do not run a setwd line when deploying app.

# Load Data
Direct<-read_excel("final_interactiondata_METADATA_LOCALCOPY_june2020.xlsx",sheet=3)
Mediators<-read_excel("final_interactiondata_METADATA_LOCALCOPY_june2020.xlsx",sheet=4)
colnames(Mediators) <- Mediators[1, ] # fix the header on the mediator df
Mediators <- Mediators[-1, ] 
Mediators<-Mediators[!is.na(Mediators$`Mediator Node`), ] # exclude blanks rows if there are any.
sectorCodes<-read_excel("sectorCodes.xlsx")
references<-read_excel("references.xlsx")

#Merge direct and indirect interactions into one dataframe 
direct_clean<-Direct%>%
  select(Interaction,Outcome,From, To, Bidirectional,context_dependent,Network_popup, Summary, References) %>% 
  mutate(med_node=NA)

mediator_clean<-Mediators%>%
  rename(med_node=`Mediator Node`)%>%
  select(Interaction,Outcome,From, To, Bidirectional,med_node,context_dependent,Network_popup, Summary, References) 

interactions_full<-rbind(direct_clean,mediator_clean)%>% #bind, then add whether each interaction is positive or negative.
  rowid_to_column() %>% 
  mutate(outcome_direction=case_when(Outcome=="space-crowd"|Outcome=="space-ex"|Outcome=="natcap-dimin"|Outcome=="operation-dimin"|
                                       Outcome=="value-dimin"~"negative",
                                     Outcome=="space-syn"|Outcome=="natcap-enhance"|Outcome=="operation-enhance"|
                                       Outcome=="value-enhance"~"positive")) 


#################################################
# Put into a network-compatible format.
#################################################

#### Step 1: Create Nodes dataframe 
id<-c("agg","des","tel","mil","bio","ren","wave","wind","dril","min","ship", 
      "tou","aqua","fish","mpa","eco")
nodes<-as.data.frame(id) 


#### Step 2: Create the edges dataframe

### Direct Interactions ###
directlinks<-interactions_full %>% filter(is.na(med_node)) #filter for the direct interactions only

# Specify from/to for bidirectional interactions, then specify that bidirectional edges need double arrows.
# Unidirectional edges only get one arrow.
directlinks$From<- ifelse(is.na(directlinks$From), sub("\\-.*", "", directlinks$Interaction), directlinks$From) 
directlinks$To<- ifelse(is.na(directlinks$To), sub(".*-", "", directlinks$Interaction), directlinks$To)
directlinks$arrows.from.type<-ifelse(directlinks$Bidirectional==1,"arrow",NA)
directlinks$arrows.to.type<-"arrow"
directlinks$EdgeType<-"Direct"
names(directlinks)[names(directlinks) == 'From'] <- 'from'
names(directlinks)[names(directlinks) == 'To'] <- 'to'
directlinks$color.highlight=c("red", "forestgreen")[as.factor(directlinks$outcome_direction)] # positive edges = green, negative = red

### Indirect Interactions (mediated) ###
medlinks<-interactions_full %>% filter(!is.na(med_node))
medlinks$From<- ifelse(is.na(medlinks$From), sub("\\-.*", "", medlinks$Interaction), medlinks$From)
medlinks$To<- ifelse(is.na(medlinks$To), sub(".*-", "", medlinks$Interaction), medlinks$To)
medlinks$arrows.from.type<-ifelse(medlinks$Bidirectional==1,"arrow",NA)
medlinks$arrows.to.type<-"arrow"
medlinks$EdgeType<-"Indirect"
names(medlinks)[names(medlinks) == 'From'] <- 'from'
names(medlinks)[names(medlinks) == 'To'] <- 'to'
medlinks$color.highlight=c("red", "forestgreen")[as.factor(medlinks$outcome_direction)]

### Bind together ###
links_full_coll<-rbind(directlinks,medlinks) #'coll' means this network is collapsed, ie mediator nodes are not shown.



#### Step 3: Set network characteristics
links_full_coll$dashes<-c(FALSE,TRUE)[as.factor(links_full_coll$EdgeType)] #set the line type (dashes vs normal) for indirect vs direct
links_full_coll$smooth<-T

links_full_coll$title<-links_full_coll$Network_popup
links_full_coll$Bidirectional<-ifelse(links_full_coll$Bidirectional==0, "no", "yes")
links_full_coll$context_dependent<-ifelse(is.na(links_full_coll$context_dependent), links_full_coll$context_dependent, "yes")

nodes$label<-nodes$id

# set up the legend 
ledges <- data.frame(color = c("grey", "grey"), 
                     label = c("Direct", "Indirect"),dashes=c(FALSE,TRUE), 
                     font.align = "top")



#################################################
# Gather tables to be shown in the app.
#################################################

# Create a cleaner table to show in the shiny app.
shinytable <- links_full_coll %>% select (-c(rowid,Network_popup, arrows.from.type, arrows.to.type,color.highlight,dashes,smooth,title))%>% 
  rename(From=from, To=to, `Mediator Node`= med_node, `Positive or Negative` = outcome_direction, `Direct or Indirect` = EdgeType, `Context-Dependent`=context_dependent,Summary = Summary, References = References) 

sectorCodes

references


########################################
# Shiny code
########################################


# Step 1: Build the UI

buttonchoicelist<-c("Natural Capital Diminishment"="natcap-dimin","Natural Capital Enhancement" ="natcap-enhance", "Operations diminishment"="operation-dimin",
                    "Operations enhancement"="operation-enhance","Space Competition"="space-crowd","Space synergism"="space-syn",
                    "Value Enhancement"="value-enhance","Value Diminishment"="value-dimin")

ui <- fluidPage(
  tags$head(tags$style(HTML('* {font-family: Calibri;}'))),
  titlePanel(h1(strong("Ocean sector interactions and outcomes"),
                h2(em("Database and interactive network tool")))),
  br(), #line break
  em("This is an interactive network tool to visualize interactions among ocean sectors, and associated categories of outcomes. Interactions and outcomes were identified through a systematic review of peer reviewed literature [INSERT LINK TO PAPER]. "),
  br(),
  br(),
  em(strong("Direct interactions")),em("are those that occur directly between two ocean sectors, and are not mediated by any mediating activity or ecosystem."),
  br(),
  em(strong("Indirect interactions")),em("are interactions between two ocean sectors mediated by another activity, such as dredging, laying of cables and pipelines, reclaiming land etc."),
  br(),
  br(),
  em("Direct interactions are shown with solid lines."),
  br(),
  em("Interactions where a mediating activity is involved are shown with dashed lines. To see which mediating activity is involved, either hover over the line or see the table at the bottom of the page."),
  br(),
  br(),
  em("Use the options in the grey area below to select the type of outcome you want to see display. All ocean sectors interactions associated with this outcome type(s) will be displayed."),
  br(),
  em("Sector/mediating activity/biosphere proxy codes are listed below."),
  br(),
  br(),
  em("To highlight the involvement of a certain sector, click on the nodes and outcomes will be displayed in different colors (green for enhancement, red for diminishment)."),
  br(),
  em("Hold down CTRL to highlight multiple sectors."),
  br(),
  br(),
  em("Below the network diagram a table is displayed containing all the different entries in the database, relating to the specific sectors and outcomes chosen for display. The latter may be easier to interpret (than the network diagram) in cases where there are many interactions. "),
  br(),
  em("This data table also displays more detailed information for each interaction, including the references supporting the identification of this interaction."),
  br(),
  br(),
  em(strong("NOTE:")),em("'Context dependent' in relation to space outcomes indicates that the extent of crowding can range from slight crowding to full exclusion depending on context. In relation to value outcomes it indicates that the impact of the interaction is dependent on value of tourists in a particular context."),
  
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "OutcomeInput",label = "Outcome",choices = buttonchoicelist),
      br(),
      tableOutput("codes")),                                                   
    mainPanel(visNetworkOutput("network", height="500"),
              br(),
              DT::dataTableOutput("results"),
              br(),
              tableOutput("references"))
    
  )
)


# Step 2: Set up the server code 

server <- function(input, output,session) {
  
  
  output$network <- renderVisNetwork({ 
    
    edges_filtered <- links_full_coll %>% filter(Outcome %in% input$OutcomeInput)
    
    
    visNetwork(nodes,edges_filtered,height = "120%",width ="20%")%>% 
      visPhysics(solver = "forceAtlas2Based",stabilization = T) %>%
      visNodes(shape='box',shadow=F,borderWidth=1, scaling = list(label = list(enabled = T,max=30,min=30)),font=list(size=30,bold=T), 
               color=list(background="lightblue",border="black",highlight=list(background="lightblue",border="black")),) %>%
      visEdges(smooth = list(enabled=T), shadow=F, length=7, width = 3,selectionWidth = 3) %>%
      visIgraphLayout(layout = "layout_in_circle") %>% 
      visLayout(improvedLayout = T,randomSeed = 2069) %>%
      visOptions(highlightNearest = list(degree=list(from=1,to=1),labelOnly=T,hideColor=grey))  %>% #can add selectedBy = "sector", for dropdown lists; #nodesIdSelection = list(enabled=T,main="Select sector (hold CTRL to select multiple", style = list(width="220px"))
      visInteraction(multiselect=T, dragNodes = T, dragView = T, zoomView = T,tooltipDelay=10, tooltipStay=300,tooltipStyle = 'position: fixed;visibility:hidden;padding: 1px;white-space: nowrap;
      font-family: arial;font-size:12px;font-color:black;background-color: white;') %>%
      visLegend(width = 0.1, position = "right",addEdges = ledges)  
    
  })
  
  output$results <- DT::renderDataTable({
    filtered_table <- shinytable %>%
      filter(Outcome %in% input$OutcomeInput)
    filtered_table})
  
  
  output$codes <-renderTable({sectorCodes})
  output$references <-renderTable({references})
  
}

shinyApp(ui = ui, server = server) #needs to be the last line in the file.

