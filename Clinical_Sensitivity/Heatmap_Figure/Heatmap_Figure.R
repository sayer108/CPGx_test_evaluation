#Heatmap Figure
setwd("~/Desktop/Clinical_Sensitivity/Heatmap_Figure")
library(ComplexHeatmap)
library(pdp)
library(ggplot2)
library(ggpubr)
library(grid)
library(tidyr)

#Heatmaps for each CYP enzyme evaluated, imported from file and assigned a new variable name
HEATC2D6.1<- HEATC2D6
HEATC2C9.1<- HEATC2C9
HEATC2C19.1<- HEATC2C19
HEATC2B6.1<- HEATC2B6
HEATC3A5.1<- HEATC3A5

#Converting Heatmaps to the appropriate Datatype
C3A5 <- grid.grabExpr(draw(HEATC3A5.1))
C2C9 <- grid.grabExpr(draw(HEATC2C9.1))
C2C19 <- grid.grabExpr(draw(HEATC2C19.1))
C2B6 <- grid.grabExpr(draw(HEATC2B6.1))
C2D6 <- grid.grabExpr(draw(HEATC2D6.1))

#Build a legend for Heatmap in GGPLOT format to add to figure below, heatmaps used in figure
#were created using the "ComplexHeatmap" Package, this was just used to add a legend
Cyp2C9_Sensitivity_Tests1<- Cyp2C9_Sensitivity_Tests
Cyp2C9_Sensitivity_Tests1<- gather(Cyp2C9_Sensitivity_Tests1)
Cyp2C9_Sensitivity_Tests1$Test<- rep(rownames(Cyp2C9_Sensitivity_Tests))
colnames(Cyp2C9_Sensitivity_Tests1)<- c("Race", "Value", "Test")

X<- ggplot(data= Cyp2C9_Sensitivity_Tests1, aes(x= Test, y= Race)) + geom_tile(aes(fill= Value)) +
  scale_fill_gradient2(low= "blue", mid= "white",high= "red", midpoint = 50, name= "Sensitivity %") 



#combine plots and formed 
a<- ggarrange(C2C9, C2D6, nrow= 2, labels= c("A", "B"), vjust= .9, hjust = 0)
b<- ggarrange(C2B6, C3A5, C2C19, nrow= 3, labels= c("C", "D", "E"),vjust= .9, hjust = 0)
Final_Heatmap_Figure<- ggarrange(a,b,ncol=2, legend.grob= get_legend(X), common.legend = TRUE, legend= "right")


