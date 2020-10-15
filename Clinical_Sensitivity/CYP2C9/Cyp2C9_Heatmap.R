library(ComplexHeatmap)
library(circlize)
###Creating Cyp2C9 data with appropriate genetic test terminology
setwd("~/Desktop/Clinical_Sensitivity/CYP2C9")
Cyp2C9_Sensitivity_Tests
Heat_Cyp2C9<- as.matrix(Cyp2C9_Sensitivity_Tests[,c(3:11)])
colnames(Heat_Cyp2C9)<- c("AAAC", "Amer", "CSA", "EA", "Euro", "Lat", "NE", "Oc", "SSA")
Cyp2C9_Coverage<- as.matrix(Cyp2C9_Sensitivity_Tests$Percent)
rownames(Cyp2C9_Coverage)<- rownames(Cyp2C9_Sensitivity_Tests)
Ha2<- rowAnnotation('Coverage %'= anno_barplot(Cyp2C9_Coverage, width = unit(4, "cm"), ylim = range(50), axis_param = list(at= c(10,20,30,40,50))))

HEATC2C9<- Heatmap(Heat_Cyp2C9, show_row_dend = FALSE, show_column_dend = FALSE, right_annotation = Ha2, row_names_side= "left", show_heatmap_legend = FALSE, rect_gp = gpar(col= "gray44"), col= ColorMapping(col_fun= colorRamp2(c(0,50,100),c("blue","white","red"))))
