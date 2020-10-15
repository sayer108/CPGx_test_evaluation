###Altered Metabolizer Frequency
setwd("~/Desktop/Clinical_Sensitivity/Altered_Metabolizer_Frequency")
library(readxl)
library(tidyr)
library(ggplot2)

###Altered Metabolizer frequencies come from PharmGKB database and are loaded here as a single table
AltMetFreq<- read_excel("AlteredMetabolizerFrequency.xlsx")
AltMetFreq<- as.data.frame(AltMetFreq)
AltMetFreq[,2:6]<- as.numeric(unlist(AltMetFreq[,2:6]))
AltMetFreq[,2:6]<- AltMetFreq[,2:6]*100
AltMetFreq$`Race/Ethnic Background`<- c("AAAC", "Ame", "CSA", "EA", "Eur", "Lat", "NE", "Oc", "SSA" )

###Changes format to long form to be used for GGPLOT function
AltMetFreq.long<- gather(data= AltMetFreq, key= "Race/Ethnic Background")
AltMetFreq.long$Variant<- rep(AltMetFreq$`Race/Ethnic Background`,5)
colnames(AltMetFreq.long)<- c("Enzyme", "Percent", "Race")
AltMetFreq.long$Percent<- round(AltMetFreq.long$Percent, digits= 0)

###Color Scheme
cbp1 <- c("#999999", "palegreen","#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###Final Plot 
Altered_Metabolizer_Frequency_Plot<- 
  ggplot(AltMetFreq.long, aes(x= Race, y= Percent, fill= Race)) +geom_bar(stat= "identity", position= position_dodge())+ scale_fill_manual(values= cbp1) + theme_bw() +
  facet_wrap(~Enzyme, scales= 'free_x')+ ylab("% of Population with an Altered Metabolizing Pheontype")+ geom_text(aes(label= Percent), vjust=-0.3, size= 3.5, position= position_dodge(width= 1))+ xlab("Race/Ethnic Group")+ 
  theme(axis.title= element_text(face= "bold"),axis.text= element_text(face= "bold"), plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), strip.background = element_rect(color= "white", fill= "white"))
Altered_Metabolizer_Frequency_Plot
