
setwd("~/Desktop/Clinical_Sensitivity/Cumm_Plot")
library(dplyr)



Cyp2B6_Sensitivity_Tests1<- Cyp2B6_Sensitivity_Tests
Cyp2B6_Sensitivity_Tests1<- Cyp2B6_Sensitivity_Tests1[,-c(1,2)]
length(colnames(Cyp2B6_Sensitivity_Tests1))

Cyp2C19_Sensitivity_Tests1<- Cyp2C19_Sensitivity_Tests
Cyp2C19_Sensitivity_Tests1<- Cyp2C19_Sensitivity_Tests1[,-c(1,2)]
length(colnames(Cyp2C19_Sensitivity_Tests1))

Cyp2C9_Sensitivity_Tests1<- Cyp2C9_Sensitivity_Tests
Cyp2C9_Sensitivity_Tests1<- Cyp2C9_Sensitivity_Tests1[,-c(1,2)]
length(colnames(Cyp2C9_Sensitivity_Tests1))

Cyp2D6_Sensitivity_Tests1<- Cyp2D6_Sensitivity_Tests
Cyp2D6_Sensitivity_Tests1<- Cyp2D6_Sensitivity_Tests1[,-c(1,2)]
length(colnames(Cyp2C9_Sensitivity_Tests1))
                
Cyp3A5_Sensitivity_Tests1<- Cyp3A5_Sensitivity_Tests
Cyp3A5_Sensitivity_Tests1<- Cyp3A5_Sensitivity_Tests1[,-c(1,2)]
length(colnames(Cyp3A5_Sensitivity_Tests1))
Cyp3A5_Sensitivity_Tests1$American<- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN)
Cyp3A5_Sensitivity_Tests1$Oceanian<- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN)


Cyp2B6_Sensitivity_Tests1$Enzyme<- "CYP2B6"
Cyp2C19_Sensitivity_Tests1$Enzyme<- "CYP2C19"
Cyp2C9_Sensitivity_Tests1$Enzyme<- "CYP2C9"
Cyp2D6_Sensitivity_Tests1$Enzyme<- "CYP2D6"
Cyp3A5_Sensitivity_Tests1$Enzyme<- "CYP3A5"

Cumm_Results<- rbind(Cyp2B6_Sensitivity_Tests1,
                      Cyp2C19_Sensitivity_Tests1,
                      Cyp2C9_Sensitivity_Tests1, 
                      Cyp2D6_Sensitivity_Tests1,
                      Cyp3A5_Sensitivity_Tests1)




Cumm_Results1<- Cumm_Results
colnames(Cumm_Results1)<- c("AAAC", "Amer", "CSA", "EA", "Euro", "Lat", "NE", "Oc", "SSA", "Enzyme")
Cumm_Results1.1<- gather(Cumm_Results1, key= "Race/Ethnic Group")
Cumm_Results1.1$Test<- rep(rownames(Cumm_Results1), 10)
Cumm_Results1.1$Enzyme<- rep(Cumm_Results1$Enzyme, 10)
colnames(Cumm_Results1.1)<- c("Race", "Sensitivity", "Test", "Enzyme")
Cumm_Results1.1<- Cumm_Results1.1 %>% filter(Race != "Enzyme")
Cumm_Results1.1<- Cumm_Results1.1 %>% filter(Sensitivity != "NaN")
Cumm_Results1.1$Sensitivity<- as.numeric(Cumm_Results1.1$Sensitivity)

Plot<- Cumm_Results1.1 %>% ggplot(aes(x= Enzyme, y= Sensitivity, fill= Enzyme)) + geom_boxplot()+ facet_grid(~Race)+ theme_classic()+
  theme(axis.title= element_text(face= "bold"), plot.background = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor= element_blank(), axis.text.x= element_text(angle=90, size= 8))

tiff(file= "Cummumlative_Boxplot_TIFF", res= 600, width= 178, height = 200, units= 'mm')
Cumm_Results1.1 %>% ggplot(aes(x= Enzyme, y= Sensitivity, fill= Enzyme)) + geom_boxplot()+ facet_grid(~Race)+ theme_classic()+
  theme(axis.title= element_text(face= "bold"), plot.background = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor= element_blank(), axis.text.x= element_text(angle=90, size= 8))
dev.off()


tiff()
x





