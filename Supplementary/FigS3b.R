### Bin distribution ###
# Compare bin distribution attributed by abstract to bin distribution after full paper read ##

# Set up environment
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(forcats)

# Loan distribution of bins assigned during absract screening and during full text reading
bins<-read_xlsx("C:/Users/kate/Desktop/Ocean Stewardship/ocean_stew/Bins/bins_dec2019.xlsx")

# Two paper-copy articles were read, add these to the dataframe.
paper<-data.frame(c("disp_swamy_2012","disp_swamy_2012","pipe_mazzarese_2017","pipe_mazzarese_2017"),c("bin_orig\\disp","bin\\ship","bin_orig\\pipe","bin\\ship"))
colnames(paper) <- c("Document name","Code")

bins<-bind_rows(bins,paper)

mapvalues(bins$Code, from=c("min","aqua","dril","fish","ship","tou","wind","disp","wave","rec","ren","bio","dred","pipe","mil","cab","agg", "des"),to=c("Mining","Aquaculture","Drilling","Fishing","Shipping","Tourism","Wind Energy","Disposal","Wave Energy","Land Reclamation","Renewables (other)","Bioprospecting","Dredging","Pipelines","Military","Cables","Aggregates", "Desalination"))

## Orig bin distribution - number read per bin
codes_binOrig<-filter(bins,grepl("bin_orig",Code)) 
codes_binOrig$Code<-gsub("bin_orig\\", "",codes_binOrig$Code, fixed=TRUE) #remove "bin_orig" part of code
codes_binOrig$Code <- fct_infreq(codes_binOrig$Code)
codes_binOrig$Code<-mapvalues(codes_binOrig$Code, from=c("min","aqua","dril","fish","ship","tou","wind","disp","wave","rec","ren","bio","dred","pipe","mil","cab","agg", "des"),to=c("Mining","Aquaculture","Drilling","Fishing","Shipping","Tourism","Wind Energy","Disposal","Wave Energy","Land Reclamation","Renewables (other)","Bioprospecting","Dredging","Pipelines","Military","Cables","Aggregates", "Desalination"))

windowsFonts("Calibri" = windowsFont("Calibri"))

bin_origPlot<-ggplot(data=codes_binOrig,aes(x=Code)) + 
  geom_bar(stat = "count",fill="lightblue",colour="blue") +
  geom_text(aes(label=..count..),vjust=1,stat='count')+
  theme(axis.text.x = element_text(angle=45,hjust = 1)) +
  ggtitle(label= "Distribution of articles read from their original bins") + 
  labs(x="Bin")
bin_origPlot  # Note that extra articles were read for mining. This is because after rebinning the articles, there were less than 25 left in the mining bin. 
# Thus, since extra articles were available, additional articles were read in order to bring this bin to a final bin size of 25 articles.




## final bin distribution after rebinning process
codes_bin_NoMeds<-filter(bins,grepl("\\bbin\\b",Code)) 
codes_bin_NoMeds$Code<-gsub("bin\\", "",codes_bin_NoMeds$Code, fixed=TRUE) 
codes_bin_NoMeds<-codes_bin_NoMeds[!grepl("msp", codes_bin_NoMeds$Code),]
codes_bin_NoMeds$Code <- fct_infreq(codes_bin_NoMeds$Code)
codes_bin_NoMeds$Code<-mapvalues(codes_bin_NoMeds$Code, from=c("min","aqua","dril","fish","ship","tou","wind","disp","wave","rec","ren","bio","dred","pipe","mil","cab","agg", "des", "tel"),to=c("Mining","Aquaculture","Drilling","Fishing","Shipping","Tourism","Wind Energy","Disposal","Wave Energy","Land Reclamation","Renewables (other)","Bioprospecting","Dredging","Pipelines","Military","Cables","Aggregates", "Desalination","Telecommunications"))


rebinPlot<-ggplot(data=codes_bin_NoMeds,aes(x=Code)) + 
  geom_bar(stat = "count",fill="darkgrey") +
  #ggtitle(label= "Distribution of Re-classified bins")+
  # geom_text(aes(label=..count..),
  #            vjust=1,stat='count')+
  theme_classic()+
  labs(x = "Bin", y="Count") +
  theme(axis.text.x = element_text(angle=45,hjust = 1, size=14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())
rebinPlot




