library(ComplexHeatmap)
library(circlize)
setwd("~/Desktop/Clinical_Sensitivity/Cyp2B6")

#Dataset used comes from sensitivity calculation 
Cyp2B6_Sensitivity_Tests

Heat_Cyp2B6<- as.matrix(Cyp2B6_Sensitivity_Tests[,c(3:10)])
colnames(Heat_Cyp2B6)<- c("AAAC", "CSA", "EA", "Euro", "Lat", "NE", "Oc", "SSA")
Cyp2B6_Coverage<- as.matrix(Cyp2B6_Sensitivity_Tests$Percent)
rownames(Cyp2B6_Coverage)<- rownames(Cyp2B6_Sensitivity_Tests)
Ha2<- rowAnnotation('Coverage %'= anno_barplot(Cyp2B6_Coverage, width = unit(4, "cm"), ylim = range(50), axis_param = list(at= c(10,20,30,40,50))))

HEATC2B6<- Heatmap(Heat_Cyp2B6, show_row_dend = FALSE, show_column_dend = FALSE, right_annotation = Ha2, row_names_side= "left", show_heatmap_legend = FALSE, rect_gp = gpar(col= "gray44"), col= ColorMapping(col_fun= colorRamp2(c(0,50,100),c("blue","white","red"))))
