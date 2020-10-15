###Creating Cyp2C19 data with appropriate genetic test terminology
setwd("~/Desktop/Clinical_Sensitivity/CYP2C19")
library(readxl)
library(tidyr)

#"Cyp2C19_Frequencies", this brings gene frequency data for each of the ethnic groups, values come from PharmGKB database
#Each column represents an ethnic group, each row represents a gene variant, values in the table are the gene frequency
#removes NA values replaces wtih 0, and names the rows after gene variant names
Cyp2C19_Frequencies<- read_excel("CYP2C19_Frequencies.xlsx")
Cyp2C19_Frequencies<- as.data.frame(Cyp2C19_Frequencies)
Cyp2C19_Frequencies[is.na(Cyp2C19_Frequencies)]<- 0
rownames(Cyp2C19_Frequencies)<- Cyp2C19_Frequencies$`CYP2C19 allele*`



#"Cyp2C19_Tests", this brings gene test coverage data collected in our study
#each column represents a specific genetic test, the rows represent each gene variant, "Yes" means gene is covered, "No" means it is not
#row names changed to be the name of each gene variant
Cyp2C19_Tests<- read_excel("Cyp2C19Tests.xlsx")
Cyp2C19_Tests<- as.data.frame(Cyp2C19_Tests)     
rownames(Cyp2C19_Tests)<- Cyp2C19_Frequencies$`CYP2C19 allele*`



#"Cyp2C19_Phenotypes", loads table with each potential diplotype and corresponding phenotype, from PharmGKB reference
#each gene of the diplotype has its own column (A1 and A2), the column "Phenotype" designates the metabolizing status of each diplotype,each diplotype is designated as heterozygous(2) or homozygous(1) in a column called multiply
Cyp2C19_Phenotypes<- read_excel("CYP2C19_Diplotypes.xlsx")
Cyp2C19_Phenotypes<- as.data.frame(Cyp2C19_Phenotypes)
Cyp2C19_Phenotypes<- separate(data= Cyp2C19_Phenotypes, col= 'CYP2C19 Diplotype', into= c("A1", "A2"), sep= "/")
colnames(Cyp2C19_Phenotypes)<- c("A1", "A2", "Phenotype", "EHR")
Cyp2C19_Phenotypes$Multiply<- ifelse(Cyp2C19_Phenotypes$A1 == Cyp2C19_Phenotypes$A2, 1, 2)



#takes the same Phenotype table "Cyp2C19_Phenotypes" described above with the same columns, creates a new table called "Cyp2C19_Phenotypes_Test" and makes additional adjustments
#takes the variant names listed in A1 and A2 in the phenotype table and replaces it with a "Yes" or a "No" depending on whether or not the gene test being assessed covers the gene
#For each calculation of clinical sensitivity, the Test Name has to be replaced in the column title of the Cyp2C19_Tests
Cyp2C19_Phenotypes_Test<- Cyp2C19_Phenotypes
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*1"]<- Cyp2C19_Tests["*1","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*2"]<- Cyp2C19_Tests["*2","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*3"]<- Cyp2C19_Tests["*3","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*4"]<- Cyp2C19_Tests["*4","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*5"]<- Cyp2C19_Tests["*5","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*6"]<- Cyp2C19_Tests["*6","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*7"]<- Cyp2C19_Tests["*7","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*8"]<- Cyp2C19_Tests["*8","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*9"]<- Cyp2C19_Tests["*9","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*10"]<- Cyp2C19_Tests["*10","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*11"]<- Cyp2C19_Tests["*11","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*12"]<- Cyp2C19_Tests["*12","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*13"]<- Cyp2C19_Tests["*13","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*14"]<- Cyp2C19_Tests["*14","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*15"]<- Cyp2C19_Tests["*15","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*16"]<- Cyp2C19_Tests["*16","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*17"]<- Cyp2C19_Tests["*17","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*18"]<- Cyp2C19_Tests["*18","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*19"]<- Cyp2C19_Tests["*19","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*22"]<- Cyp2C19_Tests["*22","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*23"]<- Cyp2C19_Tests["*23","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*24"]<- Cyp2C19_Tests["*24","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*25"]<- Cyp2C19_Tests["*25","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*26"]<- Cyp2C19_Tests["*26","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*28"]<- Cyp2C19_Tests["*28","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*29"]<- Cyp2C19_Tests["*29","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*30"]<- Cyp2C19_Tests["*30","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*31"]<- Cyp2C19_Tests["*31","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*32"]<- Cyp2C19_Tests["*32","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*33"]<- Cyp2C19_Tests["*33","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*34"]<- Cyp2C19_Tests["*34","Tier One"] 
Cyp2C19_Phenotypes_Test[Cyp2C19_Phenotypes_Test== "*35"]<- Cyp2C19_Tests["*35","Tier One"] 

#Creates an addtional column for "Cyp2C19_Phenotypes_Test" called "Test"
#if a phenotype has both genes covered, or "Yes" for both A1 and A2 columns, it is called "Observable", if only 1 of 2 or none say "Yes", it says "Not"
Cyp2C19_Phenotypes_Test$Test<- ifelse(Cyp2C19_Phenotypes_Test$A1 == "Yes"& Cyp2C19_Phenotypes_Test$A2== "Yes", "Observable", "Not")
colnames(Cyp2C19_Phenotypes_Test)<- c("Al.1", "A2.1", "Phenotype", "EHR", "Multiply", "Test")

Cyp2C19_Phenotypes_Test$A1<- Cyp2C19_Phenotypes$A1
Cyp2C19_Phenotypes_Test$A2<- Cyp2C19_Phenotypes$A2



###The next 9 subsections represent calculations for clinical sensitity for each ethnic group using the "Cyp2C19_Phenotypes_Test"
###replaces the allele name with the frequency it occurs in each table


###African
AfricanPhenotypes<- Cyp2C19_Phenotypes_Test
AfricanPhenotypes[AfricanPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","African-American/Afro-Caribbean Allele Frequency"] 
AfricanPhenotypes[AfricanPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","African-American/Afro-Caribbean Allele Frequency"] 

#Calculate Probabilities of each Diplotype by multiplying gene frequencies in each table
AfricanPhenotypes$A1<- as.numeric(AfricanPhenotypes$A1)
AfricanPhenotypes$A2<- as.numeric(AfricanPhenotypes$A2)
AfricanPhenotypes$Multiply<- as.numeric(AfricanPhenotypes$Multiply)
AfricanPhenotypes$Probability<- AfricanPhenotypes$A1*AfricanPhenotypes$A2
AfricanPhenotypes$Probability<- AfricanPhenotypes$Probability*AfricanPhenotypes$Multiply

#This portion calculation 

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AfricanAbnormalCorrect<- subset(AfricanPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_AfricanAbnormalCorrect<- sum(AfricanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
AfricanAbnormal<- subset(AfricanPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_AfricanAbnormal<- sum(AfricanAbnormal$Probability)

###Sensitivity
AfricanSensitivity<- (Frequency_AfricanAbnormalCorrect/(Frequency_AfricanAbnormal))*100



###American
AmericanPhenotypes<- Cyp2C19_Phenotypes_Test
AmericanPhenotypes[AmericanPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","American Allele Frequency"] 
AmericanPhenotypes[AmericanPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","American Allele Frequency"] 

#Calculate Probabilities of each Diplotype
AmericanPhenotypes$A1<- as.numeric(AmericanPhenotypes$A1)
AmericanPhenotypes$A2<- as.numeric(AmericanPhenotypes$A2)
AmericanPhenotypes$Multiply<- as.numeric(AmericanPhenotypes$Multiply)
AmericanPhenotypes$Probability<- AmericanPhenotypes$A1*AmericanPhenotypes$A2
AmericanPhenotypes$Probability<- AmericanPhenotypes$Probability*AmericanPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AmericanAbnormalCorrect<- subset(AmericanPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_AmericanAbnormalCorrect<- sum(AmericanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
AmericanAbnormal<- subset(AmericanPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_AmericanAbnormal<- sum(AmericanAbnormal$Probability)

###Sensitivity
AmericanSensitivity<- (Frequency_AmericanAbnormalCorrect/(Frequency_AmericanAbnormal))*100



###Central
CentralPhenotypes<- Cyp2C19_Phenotypes_Test
CentralPhenotypes[CentralPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","Central/South Asian Allele Frequency"] 
CentralPhenotypes[CentralPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","Central/South Asian Allele Frequency"] 

#Calculate Probabilities of each Diplotype
CentralPhenotypes$A1<- as.numeric(CentralPhenotypes$A1)
CentralPhenotypes$A2<- as.numeric(CentralPhenotypes$A2)
CentralPhenotypes$Multiply<- as.numeric(CentralPhenotypes$Multiply)
CentralPhenotypes$Probability<- CentralPhenotypes$A1*CentralPhenotypes$A2
CentralPhenotypes$Probability<- CentralPhenotypes$Probability*CentralPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
CentralAbnormalCorrect<- subset(CentralPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_CentralAbnormalCorrect<- sum(CentralAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
CentralAbnormal<- subset(CentralPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_CentralAbnormal<- sum(CentralAbnormal$Probability)

###Sensitivity
CentralSensitivity<- (Frequency_CentralAbnormalCorrect/(Frequency_CentralAbnormal))*100



###East
EastPhenotypes<- Cyp2C19_Phenotypes_Test
EastPhenotypes[EastPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","East Asian Allele Frequency"] 
EastPhenotypes[EastPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","East Asian Allele Frequency"] 

#Calculate Probabilities of each Diplotype
EastPhenotypes$A1<- as.numeric(EastPhenotypes$A1)
EastPhenotypes$A2<- as.numeric(EastPhenotypes$A2)
EastPhenotypes$Multiply<- as.numeric(EastPhenotypes$Multiply)
EastPhenotypes$Probability<- EastPhenotypes$A1*EastPhenotypes$A2
EastPhenotypes$Probability<- EastPhenotypes$Probability*EastPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
EastAbnormalCorrect<- subset(EastPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_EastAbnormalCorrect<- sum(EastAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
EastAbnormal<- subset(EastPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_EastAbnormal<- sum(EastAbnormal$Probability)

###Sensitivity
EastSensitivity<- (Frequency_EastAbnormalCorrect/(Frequency_EastAbnormal))*100



###European
EuropeanPhenotypes<- Cyp2C19_Phenotypes_Test
EuropeanPhenotypes[EuropeanPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","European Allele Frequency"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","European Allele Frequency"] 

#Calculate Probabilities of each Diplotype
EuropeanPhenotypes$A1<- as.numeric(EuropeanPhenotypes$A1)
EuropeanPhenotypes$A2<- as.numeric(EuropeanPhenotypes$A2)
EuropeanPhenotypes$Multiply<- as.numeric(EuropeanPhenotypes$Multiply)
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$A1*EuropeanPhenotypes$A2
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$Probability*EuropeanPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
EuropeanAbnormalCorrect<- subset(EuropeanPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_EuropeanAbnormalCorrect<- sum(EuropeanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
EuropeanAbnormal<- subset(EuropeanPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_EuropeanAbnormal<- sum(EuropeanAbnormal$Probability)

###Sensitivity
EuropeanSensitivity<- (Frequency_EuropeanAbnormalCorrect/(Frequency_EuropeanAbnormal))*100



###Latino
LatinoPhenotypes<- Cyp2C19_Phenotypes_Test
LatinoPhenotypes[LatinoPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","Latino Allele Frequency"] 
LatinoPhenotypes[LatinoPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","Latino Allele Frequency"] 

#Calculate Probabilities of each Diplotype
LatinoPhenotypes$A1<- as.numeric(LatinoPhenotypes$A1)
LatinoPhenotypes$A2<- as.numeric(LatinoPhenotypes$A2)
LatinoPhenotypes$Multiply<- as.numeric(LatinoPhenotypes$Multiply)
LatinoPhenotypes$Probability<- LatinoPhenotypes$A1*LatinoPhenotypes$A2
LatinoPhenotypes$Probability<- LatinoPhenotypes$Probability*LatinoPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
LatinoAbnormalCorrect<- subset(LatinoPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_LatinoAbnormalCorrect<- sum(LatinoAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
LatinoAbnormal<- subset(LatinoPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_LatinoAbnormal<- sum(LatinoAbnormal$Probability)

###Sensitivity
LatinoSensitivity<- (Frequency_LatinoAbnormalCorrect/(Frequency_LatinoAbnormal))*100



###Near
NearPhenotypes<- Cyp2C19_Phenotypes_Test
NearPhenotypes[NearPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","Near Eastern Allele Frequency"] 
NearPhenotypes[NearPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","Near Eastern Allele Frequency"] 

#Calculate Probabilities of each Diplotype
NearPhenotypes$A1<- as.numeric(NearPhenotypes$A1)
NearPhenotypes$A2<- as.numeric(NearPhenotypes$A2)
NearPhenotypes$Multiply<- as.numeric(NearPhenotypes$Multiply)
NearPhenotypes$Probability<- NearPhenotypes$A1*NearPhenotypes$A2
NearPhenotypes$Probability<- NearPhenotypes$Probability*NearPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
NearAbnormalCorrect<- subset(NearPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_NearAbnormalCorrect<- sum(NearAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
NearAbnormal<- subset(NearPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_NearAbnormal<- sum(NearAbnormal$Probability)

###Sensitivity
NearSensitivity<- (Frequency_NearAbnormalCorrect/(Frequency_NearAbnormal))*100



###Oceanian
OceanianPhenotypes<- Cyp2C19_Phenotypes_Test
OceanianPhenotypes[OceanianPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","Oceanian Allele Frequency"] 
OceanianPhenotypes[OceanianPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","Oceanian Allele Frequency"] 

#Calculate Probabilities of each Diplotype
OceanianPhenotypes$A1<- as.numeric(OceanianPhenotypes$A1)
OceanianPhenotypes$A2<- as.numeric(OceanianPhenotypes$A2)
OceanianPhenotypes$Multiply<- as.numeric(OceanianPhenotypes$Multiply)
OceanianPhenotypes$Probability<- OceanianPhenotypes$A1*OceanianPhenotypes$A2
OceanianPhenotypes$Probability<- OceanianPhenotypes$Probability*OceanianPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
OceanianAbnormalCorrect<- subset(OceanianPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_OceanianAbnormalCorrect<- sum(OceanianAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
OceanianAbnormal<- subset(OceanianPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer")
Frequency_OceanianAbnormal<- sum(OceanianAbnormal$Probability)

###Sensitivity
OceanianSensitivity<- (Frequency_OceanianAbnormalCorrect/(Frequency_OceanianAbnormal))*100



###Sub
SubPhenotypes<- Cyp2C19_Phenotypes_Test
SubPhenotypes[SubPhenotypes== "*1"]<- Cyp2C19_Frequencies["*1","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*2"]<- Cyp2C19_Frequencies["*2","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*3"]<- Cyp2C19_Frequencies["*3","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*4"]<- Cyp2C19_Frequencies["*4","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*5"]<- Cyp2C19_Frequencies["*5","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*6"]<- Cyp2C19_Frequencies["*6","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*7"]<- Cyp2C19_Frequencies["*7","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*8"]<- Cyp2C19_Frequencies["*8","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*9"]<- Cyp2C19_Frequencies["*9","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*10"]<- Cyp2C19_Frequencies["*10","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*11"]<- Cyp2C19_Frequencies["*11","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*12"]<- Cyp2C19_Frequencies["*12","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*13"]<- Cyp2C19_Frequencies["*13","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*14"]<- Cyp2C19_Frequencies["*14","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*15"]<- Cyp2C19_Frequencies["*15","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*16"]<- Cyp2C19_Frequencies["*16","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*17"]<- Cyp2C19_Frequencies["*17","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*18"]<- Cyp2C19_Frequencies["*18","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*19"]<- Cyp2C19_Frequencies["*19","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*22"]<- Cyp2C19_Frequencies["*22","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*23"]<- Cyp2C19_Frequencies["*23","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*24"]<- Cyp2C19_Frequencies["*24","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*25"]<- Cyp2C19_Frequencies["*25","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*26"]<- Cyp2C19_Frequencies["*26","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*28"]<- Cyp2C19_Frequencies["*28","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*29"]<- Cyp2C19_Frequencies["*29","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*30"]<- Cyp2C19_Frequencies["*30","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*31"]<- Cyp2C19_Frequencies["*31","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*32"]<- Cyp2C19_Frequencies["*32","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*33"]<- Cyp2C19_Frequencies["*33","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*34"]<- Cyp2C19_Frequencies["*34","Sub-Saharan Allele African Frequency"] 
SubPhenotypes[SubPhenotypes== "*35"]<- Cyp2C19_Frequencies["*35","Sub-Saharan Allele African Frequency"] 

#Calculate Probabilities of each Diplotype
SubPhenotypes$A1<- as.numeric(SubPhenotypes$A1)
SubPhenotypes$A2<- as.numeric(SubPhenotypes$A2)
SubPhenotypes$Multiply<- as.numeric(SubPhenotypes$Multiply)
SubPhenotypes$Probability<- SubPhenotypes$A1*SubPhenotypes$A2
SubPhenotypes$Probability<- SubPhenotypes$Probability*SubPhenotypes$Multiply

#Subset appropriate groups

###Population Frequency of Abnormal Phenotypes Correctly Detected 
SubAbnormalCorrect<- subset(SubPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Observable")
Frequency_SubAbnormalCorrect<- sum(SubAbnormalCorrect$Probability)

###Population Frequency of Abnormal Phenotypes Classified as normal (incorrectly detected)
SubAbnormalIncorrect<- subset(SubPhenotypes, Phenotype != "CYP2C19 Normal Metabolizer" & Test== "Not")
Frequency_SubAbnormalIncorrect<- sum(SubAbnormalIncorrect$Probability)

###Sensitivity
SubSensitivity<- (Frequency_SubAbnormalCorrect/(Frequency_SubAbnormalCorrect+Frequency_SubAbnormalIncorrect))*100



###Final Composite Results of Clinical Sensitivity for One PGx Test
TestSensitivity<- c(AfricanSensitivity, AmericanSensitivity, CentralSensitivity, EastSensitivity, EuropeanSensitivity, LatinoSensitivity, NearSensitivity, OceanianSensitivity, SubSensitivity)
TestSensitivity<- as.data.frame(TestSensitivity)
rownames(TestSensitivity)<- c("African-American/Afro-Caribbean", "American", "Central/South Asian", "East Asian", "European", "Latino", "Near Eastern", "Oceanian", "Sub-Saharan")

#"Cyp2C19_Counts", gives the total number of variants each PGx test selects, and the percent coverge of total variants targeted by each test from the total listed variants listed on PharmGKB
#Calcluations already performed, this is only exporting the values
Cyp2C19_Counts<- as.data.frame(read_excel("Cyp2C19Tests.xlsx", sheet= "Sheet2"))

Cyp2C19_Sensitivity_Tests

