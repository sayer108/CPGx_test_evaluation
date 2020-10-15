setwd("~/Desktop/Clinical_Sensitivity/Supplementary_Tables")
library(formattable)
Cyp2B6_Sensitivity_Tests
Cyp2C19_Sensitivity_Tests
Cyp2C9_Sensitivity_Tests
Cyp2D6_Sensitivity_Tests
Cyp3A5_Sensitivity_Tests

CYP2C9.Table<- formattable(Cyp2C9_Sensitivity_Tests, align= c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l"))
CYP2D6.Table<- formattable(Cyp2D6_Sensitivity_Tests, align= c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l"))
CYP2C19.Table<- formattable(Cyp2C19_Sensitivity_Tests, align= c("l", "l", "l", "l", "l", "l", "l", "l", "l", "l"))
CYP2B6.Table<- formattable(Cyp2B6_Sensitivity_Tests, align= c("l", "l", "l", "l", "l", "l", "l", "l", "l"))
CYP3A5.Table<- formattable(Cyp3A5_Sensitivity_Tests, align= c("l", "l", "l", "l", "l", "l", "l", "l"))





