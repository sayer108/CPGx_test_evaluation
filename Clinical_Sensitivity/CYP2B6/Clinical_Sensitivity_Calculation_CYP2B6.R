setwd("~/Desktop/Clinical_Sensitivity/CYP2B6")
library(readxl)
library(tidyr)

#"Cyp2B6_Frequencies", this brings gene frequency data for each of the ethnic groups, values come from PharmGKB database
#Each column represents an ethnic group, each row represents a gene variant, values in the table are the gene frequency
#removes NA values replaces wtih 0, and names the rows after gene variant names
Cyp2B6_Frequencies<- read_excel("CYP2B6_Frequencies.xlsx")
Cyp2B6_Frequencies<- as.data.frame(Cyp2B6_Frequencies)
Cyp2B6_Frequencies[is.na(Cyp2B6_Frequencies)]<- 0
rownames(Cyp2B6_Frequencies)<- Cyp2B6_Frequencies$`CYP2B6 allelec`



#"Cyp2B6_Tests", this brings gene test coverage data collected in our study
#each column represents a specific genetic test, the rows represent each gene variant, "Yes" means gene is covered, "No" means it is not
#row names changed to be the name of each gene variant
Cyp2B6_Tests<- read_excel("Cyp2B6Tests.xlsx")
Cyp2B6_Tests<- as.data.frame(Cyp2B6_Tests)
rownames(Cyp2B6_Tests)<- Cyp2B6_Frequencies$`CYP2B6 allelec`



#"Cyp2B6_Phenotypes", loads table with each potential diplotype and corresponding phenotype, from PharmGKB reference
#each gene of the diplotype has its own column (A1 and A2), the column "Phenotype" designates the metabolizing status of each diplotype,each diplotype is designated as heterozygous(2) or homozygous(1) in a column called multiply
Cyp2B6_Phenotypes<- read_excel("CYP2B6_Diplotypes.xlsx")
Cyp2B6_Phenotypes<- as.data.frame(Cyp2B6_Phenotypes)
Cyp2B6_Phenotypes<- separate(data= Cyp2B6_Phenotypes, col= 'CYP2B6 Diplotype', into= c("A1", "A2"), sep= "/")
colnames(Cyp2B6_Phenotypes)<- c("A1", "A2", "Phenotype", "EHR")
Cyp2B6_Phenotypes[Cyp2B6_Phenotypes== "Normal/Routine/Low Riskc"]<- "Normal/Routine/Low Risk"
Cyp2B6_Phenotypes$Multiply<- ifelse(Cyp2B6_Phenotypes$A1 == Cyp2B6_Phenotypes$A2, 1, 2)



#takes the same Phenotype table "Cyp2B6_Phenotypes" described above with the same columns, creates a new table called "Cyp2B6_Phenotypes_Test" and makes additional adjustments
#takes the variant names listed in A1 and A2 in the phenotype table and replaces it with a "Yes" or a "No" depending on whether or not the PGx test being assessed covers the gene
#For each calculation of clinical sensitivity, the Test Name has to be replaced in the column title of the Cyp2C19_Tests
Cyp2B6_Phenotypes_Test<- Cyp2B6_Phenotypes
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*1"]<- Cyp2B6_Tests["*1d","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*2"]<- Cyp2B6_Tests["*2","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*3"]<- Cyp2B6_Tests["*3","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*4"]<- Cyp2B6_Tests["*4","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*5"]<- Cyp2B6_Tests["*5","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*6"]<- Cyp2B6_Tests["*6","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*7"]<- Cyp2B6_Tests["*7","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*8"]<- Cyp2B6_Tests["*8","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*9"]<- Cyp2B6_Tests["*9","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*10"]<- Cyp2B6_Tests["*10","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*11"]<- Cyp2B6_Tests["*11","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*12"]<- Cyp2B6_Tests["*12","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*13"]<- Cyp2B6_Tests["*13","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*14"]<- Cyp2B6_Tests["*14","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*15"]<- Cyp2B6_Tests["*15","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*15c"]<- Cyp2B6_Tests["*15","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*16"]<- Cyp2B6_Tests["*16","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*17"]<- Cyp2B6_Tests["*17","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*18"]<- Cyp2B6_Tests["*18","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*19"]<- Cyp2B6_Tests["*19","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*20"]<- Cyp2B6_Tests["*20","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*21"]<- Cyp2B6_Tests["*21","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*22"]<- Cyp2B6_Tests["*22","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*23"]<- Cyp2B6_Tests["*23","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*24"]<- Cyp2B6_Tests["*24","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*25"]<- Cyp2B6_Tests["*25","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*26"]<- Cyp2B6_Tests["*26","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*27"]<- Cyp2B6_Tests["*27","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*28"]<- Cyp2B6_Tests["*28","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*29"]<- Cyp2B6_Tests["*29","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*30"]<- Cyp2B6_Tests["*30","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*31"]<- Cyp2B6_Tests["*31","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*32"]<- Cyp2B6_Tests["*32","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*33"]<- Cyp2B6_Tests["*33","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*34"]<- Cyp2B6_Tests["*34","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*35"]<- Cyp2B6_Tests["*35","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*36"]<- Cyp2B6_Tests["*36","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*37"]<- Cyp2B6_Tests["*37","GeneSight Psychotropic"] 
Cyp2B6_Phenotypes_Test[Cyp2B6_Phenotypes_Test== "*38"]<- Cyp2B6_Tests["*38","GeneSight Psychotropic"] 

#Creates an addtional column for "Cyp2C19_Phenotypes_Test" called "Test"
#if a phenotype has both genes covered, or "Yes" for both A1 and A2 columns, it is called "Observable", if only 1 of 2 or none say "Yes", it says "Not"
Cyp2B6_Phenotypes_Test$Test<- ifelse(Cyp2B6_Phenotypes_Test$A1 == "Yes"& Cyp2B6_Phenotypes_Test$A2== "Yes", "Observable", "Not")
colnames(Cyp2B6_Phenotypes_Test)<- c("Al.1", "A2.1", "Phenotype", "EHR", "Multiply", "Test")

Cyp2B6_Phenotypes_Test$A1<- Cyp2B6_Phenotypes$A1
Cyp2B6_Phenotypes_Test$A2<- Cyp2B6_Phenotypes$A2


###The next 9 subsections represent calculations for clinical sensitity for each ethnic group using the "Cyp2C19_Phenotypes_Test"
###replaces the allele name with the frequency it occurs in each table
###African Phenotypes
AfricanPhenotypes<- Cyp2B6_Phenotypes_Test
AfricanPhenotypes[AfricanPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","African American/Afro-Caribbean"] 

#Calculate Probabilities of each Diplotype
AfricanPhenotypes$A1<- as.numeric(AfricanPhenotypes$A1)
AfricanPhenotypes$A2<- as.numeric(AfricanPhenotypes$A2)
AfricanPhenotypes$Multiply<- as.numeric(AfricanPhenotypes$Multiply)
AfricanPhenotypes$Probability<- AfricanPhenotypes$A1*AfricanPhenotypes$A2
AfricanPhenotypes$Probability<- AfricanPhenotypes$Probability*AfricanPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AfricanAbnormalCorrect<- subset(AfricanPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_AfricanAbnormalCorrect<- sum(AfricanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
AfricanAbnormal<- subset(AfricanPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_AfricanAbnormal<- sum(AfricanAbnormal$Probability)

###Sensitivity
AfricanSensitivity<- (Frequency_AfricanAbnormalCorrect/(Frequency_AfricanAbnormal))*100



###American (No Published gene frequencies for American Populations, this is here for future use)
AmericanPhenotypes<- Cyp2B6_Phenotypes_Test
AmericanPhenotypes[AmericanPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","American"] 

#Calculate Probabilities of each Diplotype
AmericanPhenotypes$A1<- as.numeric(AmericanPhenotypes$A1)
AmericanPhenotypes$A2<- as.numeric(AmericanPhenotypes$A2)
AmericanPhenotypes$Multiply<- as.numeric(AmericanPhenotypes$Multiply)
AmericanPhenotypes$Probability<- AmericanPhenotypes$A1*AmericanPhenotypes$A2
AmericanPhenotypes$Probability<- AmericanPhenotypes$Probability*AmericanPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AmericanAbnormalCorrect<- subset(AmericanPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_AmericanAbnormalCorrect<- sum(AmericanAbnormalCorrect$Probability)

###Population Frequency of Abnormal Phenotypes Classified as normal (incorrectly detected)
AmericanAbnormal<- subset(AmericanPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_AmericanAbnormal<- sum(AmericanAbnormal$Probability)

###Sensitivity
AmericanSensitivity<- (Frequency_AmericanAbnormalCorrect/(Frequency_AmericanAbnormal))*100



#Central
CentralPhenotypes<- Cyp2B6_Phenotypes_Test
CentralPhenotypes[CentralPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","Central/South Asian"] 

#Calculate Probabilities of each Diplotype
CentralPhenotypes$A1<- as.numeric(CentralPhenotypes$A1)
CentralPhenotypes$A2<- as.numeric(CentralPhenotypes$A2)
CentralPhenotypes$Multiply<- as.numeric(CentralPhenotypes$Multiply)
CentralPhenotypes$Probability<- CentralPhenotypes$A1*CentralPhenotypes$A2
CentralPhenotypes$Probability<- CentralPhenotypes$Probability*CentralPhenotypes$Multiply


###Population Frequency of Abnormal Phenotypes Correctly Detected 
CentralAbnormalCorrect<- subset(CentralPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_CentralAbnormalCorrect<- sum(CentralAbnormalCorrect$Probability)

###Population Frequency of Abnormal Phenotypes Classified as normal (incorrectly detected)
CentralAbnormal<- subset(CentralPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_CentralAbnormal<- sum(CentralAbnormal$Probability)

###Sensitivity
CentralSensitivity<- (Frequency_CentralAbnormalCorrect/(Frequency_CentralAbnormal))*100



###East
EastPhenotypes<- Cyp2B6_Phenotypes_Test
EastPhenotypes[EastPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","East Asian"] 
EastPhenotypes[EastPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","East Asian"] 
EastPhenotypes[EastPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","East Asian"] 
EastPhenotypes[EastPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","East Asian"] 
EastPhenotypes[EastPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","East Asian"] 
EastPhenotypes[EastPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","East Asian"] 
EastPhenotypes[EastPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","East Asian"] 
EastPhenotypes[EastPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","East Asian"] 
EastPhenotypes[EastPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","East Asian"] 
EastPhenotypes[EastPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","East Asian"] 
EastPhenotypes[EastPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","East Asian"] 
EastPhenotypes[EastPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","East Asian"] 
EastPhenotypes[EastPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","East Asian"] 
EastPhenotypes[EastPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","East Asian"] 
EastPhenotypes[EastPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","East Asian"] 
EastPhenotypes[EastPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","East Asian"] 
EastPhenotypes[EastPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","East Asian"] 
EastPhenotypes[EastPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","East Asian"] 
EastPhenotypes[EastPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","East Asian"] 
EastPhenotypes[EastPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","East Asian"] 
EastPhenotypes[EastPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","East Asian"] 
EastPhenotypes[EastPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","East Asian"] 
EastPhenotypes[EastPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","East Asian"] 
EastPhenotypes[EastPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","East Asian"] 
EastPhenotypes[EastPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","East Asian"] 
EastPhenotypes[EastPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","East Asian"] 
EastPhenotypes[EastPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","East Asian"] 
EastPhenotypes[EastPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","East Asian"] 
EastPhenotypes[EastPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","East Asian"] 
EastPhenotypes[EastPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","East Asian"] 
EastPhenotypes[EastPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","East Asian"] 
EastPhenotypes[EastPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","East Asian"] 
EastPhenotypes[EastPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","East Asian"] 
EastPhenotypes[EastPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","East Asian"] 
EastPhenotypes[EastPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","East Asian"] 
EastPhenotypes[EastPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","East Asian"] 
EastPhenotypes[EastPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","East Asian"] 
EastPhenotypes[EastPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","East Asian"] 
EastPhenotypes[EastPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","East Asian"] 

#Calculate Probabilities of each Diplotype
EastPhenotypes$A1<- as.numeric(EastPhenotypes$A1)
EastPhenotypes$A2<- as.numeric(EastPhenotypes$A2)
EastPhenotypes$Multiply<- as.numeric(EastPhenotypes$Multiply)
EastPhenotypes$Probability<- EastPhenotypes$A1*EastPhenotypes$A2
EastPhenotypes$Probability<- EastPhenotypes$Probability*EastPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
EastAbnormalCorrect<- subset(EastPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_EastAbnormalCorrect<- sum(EastAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
EastAbnormal<- subset(EastPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_EastAbnormal<- sum(EastAbnormal$Probability)

###Sensitivity
EastSensitivity<- (Frequency_EastAbnormalCorrect/(Frequency_EastAbnormal))*100



###########European Allele Frequency
EuropeanPhenotypes<- Cyp2B6_Phenotypes_Test
EuropeanPhenotypes[EuropeanPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","European"] 

#Calculate Probabilities of each Diplotype
EuropeanPhenotypes$A1<- as.numeric(EuropeanPhenotypes$A1)
EuropeanPhenotypes$A2<- as.numeric(EuropeanPhenotypes$A2)
EuropeanPhenotypes$Multiply<- as.numeric(EuropeanPhenotypes$Multiply)
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$A1*EuropeanPhenotypes$A2
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$Probability*EuropeanPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
EuropeanAbnormalCorrect<- subset(EuropeanPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_EuropeanAbnormalCorrect<- sum(EuropeanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
EuropeanAbnormal<- subset(EuropeanPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_EuropeanAbnormal<- sum(EuropeanAbnormal$Probability)

###Sensitivity
EuropeanSensitivity<- (Frequency_EuropeanAbnormalCorrect/(Frequency_EuropeanAbnormal))*100



###Latino
LatinoPhenotypes<- Cyp2B6_Phenotypes_Test
LatinoPhenotypes[LatinoPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","Latino"] 

#Calculate Probabilities of each Diplotype
LatinoPhenotypes$A1<- as.numeric(LatinoPhenotypes$A1)
LatinoPhenotypes$A2<- as.numeric(LatinoPhenotypes$A2)
LatinoPhenotypes$Multiply<- as.numeric(LatinoPhenotypes$Multiply)
LatinoPhenotypes$Probability<- LatinoPhenotypes$A1*LatinoPhenotypes$A2
LatinoPhenotypes$Probability<- LatinoPhenotypes$Probability*LatinoPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
LatinoAbnormalCorrect<- subset(LatinoPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_LatinoAbnormalCorrect<- sum(LatinoAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
LatinoAbnormal<- subset(LatinoPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_LatinoAbnormal<- sum(LatinoAbnormal$Probability)

###Sensitivity
LatinoSensitivity<- (Frequency_LatinoAbnormalCorrect/(Frequency_LatinoAbnormal))*100



###############Near Eastern
NearPhenotypes<- Cyp2B6_Phenotypes_Test
NearPhenotypes[NearPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","Near Eastern"] 

#Calculate Probabilities of each Diplotype
NearPhenotypes$A1<- as.numeric(NearPhenotypes$A1)
NearPhenotypes$A2<- as.numeric(NearPhenotypes$A2)
NearPhenotypes$Multiply<- as.numeric(NearPhenotypes$Multiply)
NearPhenotypes$Probability<- NearPhenotypes$A1*NearPhenotypes$A2
NearPhenotypes$Probability<- NearPhenotypes$Probability*NearPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
NearAbnormalCorrect<- subset(NearPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_NearAbnormalCorrect<- sum(NearAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
NearAbnormal<- subset(NearPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_NearAbnormal<- sum(NearAbnormal$Probability)

###Sensitivity
NearSensitivity<- (Frequency_NearAbnormalCorrect/(Frequency_NearAbnormal))*100



#########Oceanian Allele Frequency
OceanianPhenotypes<- Cyp2B6_Phenotypes_Test
OceanianPhenotypes[OceanianPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","Oceanian"] 

#Calculate Probabilities of each Diplotype
OceanianPhenotypes$A1<- as.numeric(OceanianPhenotypes$A1)
OceanianPhenotypes$A2<- as.numeric(OceanianPhenotypes$A2)
OceanianPhenotypes$Multiply<- as.numeric(OceanianPhenotypes$Multiply)
OceanianPhenotypes$Probability<- OceanianPhenotypes$A1*OceanianPhenotypes$A2
OceanianPhenotypes$Probability<- OceanianPhenotypes$Probability*OceanianPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
OceanianAbnormalCorrect<- subset(OceanianPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_OceanianAbnormalCorrect<- sum(OceanianAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
OceanianAbnormal<- subset(OceanianPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_OceanianAbnormal<- sum(OceanianAbnormal$Probability)

###Sensitivity
OceanianSensitivity<- (Frequency_OceanianAbnormalCorrect/(Frequency_OceanianAbnormal))*100



#######Sub-Saharan Allele African Frequency
SubPhenotypes<- Cyp2B6_Phenotypes_Test
SubPhenotypes[SubPhenotypes== "*1"]<- Cyp2B6_Frequencies["*1d","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*2"]<- Cyp2B6_Frequencies["*2","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*3"]<- Cyp2B6_Frequencies["*3","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*4"]<- Cyp2B6_Frequencies["*4","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*5"]<- Cyp2B6_Frequencies["*5","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*6"]<- Cyp2B6_Frequencies["*6","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*7"]<- Cyp2B6_Frequencies["*7","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*8"]<- Cyp2B6_Frequencies["*8","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*9"]<- Cyp2B6_Frequencies["*9","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*10"]<- Cyp2B6_Frequencies["*10","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*11"]<- Cyp2B6_Frequencies["*11","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*12"]<- Cyp2B6_Frequencies["*12","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*13"]<- Cyp2B6_Frequencies["*13","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*14"]<- Cyp2B6_Frequencies["*14","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*15"]<- Cyp2B6_Frequencies["*15","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*15c"]<- Cyp2B6_Frequencies["*15","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*16"]<- Cyp2B6_Frequencies["*16","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*17"]<- Cyp2B6_Frequencies["*17","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*18"]<- Cyp2B6_Frequencies["*18","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*19"]<- Cyp2B6_Frequencies["*19","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*20"]<- Cyp2B6_Frequencies["*20","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*21"]<- Cyp2B6_Frequencies["*21","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*22"]<- Cyp2B6_Frequencies["*22","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*23"]<- Cyp2B6_Frequencies["*23","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*24"]<- Cyp2B6_Frequencies["*24","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*25"]<- Cyp2B6_Frequencies["*25","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*26"]<- Cyp2B6_Frequencies["*26","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*27"]<- Cyp2B6_Frequencies["*27","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*28"]<- Cyp2B6_Frequencies["*28","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*29"]<- Cyp2B6_Frequencies["*29","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*30"]<- Cyp2B6_Frequencies["*30","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*31"]<- Cyp2B6_Frequencies["*31","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*32"]<- Cyp2B6_Frequencies["*32","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*33"]<- Cyp2B6_Frequencies["*33","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*34"]<- Cyp2B6_Frequencies["*34","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*35"]<- Cyp2B6_Frequencies["*35","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*36"]<- Cyp2B6_Frequencies["*36","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*37"]<- Cyp2B6_Frequencies["*37","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*38"]<- Cyp2B6_Frequencies["*38","Sub-Saharan African"] 

#Calculate Probabilities of each Diplotype
SubPhenotypes$A1<- as.numeric(SubPhenotypes$A1)
SubPhenotypes$A2<- as.numeric(SubPhenotypes$A2)
SubPhenotypes$Multiply<- as.numeric(SubPhenotypes$Multiply)
SubPhenotypes$Probability<- SubPhenotypes$A1*SubPhenotypes$A2
SubPhenotypes$Probability<- SubPhenotypes$Probability*SubPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
SubAbnormalCorrect<- subset(SubPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer" & Test== "Observable")
Frequency_SubAbnormalCorrect<- sum(SubAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
SubAbnormal<- subset(SubPhenotypes, Phenotype != "CYP2B6 Normal Metabolizer")
Frequency_SubAbnormal<- sum(SubAbnormal$Probability)

###Sensitivity
SubSensitivity<- (Frequency_SubAbnormalCorrect/(Frequency_SubAbnormal))*100


#Aggregated Sensitivty Results for all ethnic groups for PGx test, There are no results for "American" because there are no published gene frequencies in PharmGKB
TestSensitivity<- c(AfricanSensitivity, AmericanSensitivity, CentralSensitivity, EastSensitivity, EuropeanSensitivity, LatinoSensitivity, NearSensitivity, OceanianSensitivity, SubSensitivity)
TestSensitivity<- as.data.frame(TestSensitivity)
rownames(TestSensitivity)<- c("African American/Afro-Caribbean", "American", "Central/South Asian", "East Asian", "European", "Latino", "Near Eastern", "Oceanian", "Sub-Saharan")



#"Cyp2B6_Counts", gives the total number of variants each PGx test selects, and the percent coverge of total variants targeted by each test from the total listed variants listed on PharmGKB
#Calcluations already performed, this is only exporting the values
Cyp2B6_Counts<- as.data.frame(read_excel("Cyp2B6Tests.xlsx", sheet= "Sheet2"))

###Table with results for all Sensitivity Results
Cyp2B6_Sensitivity_Tests




