setwd("~/Desktop/Clinical_Sensitivity/CYP3A5")
library(readxl)
library(tidyr)

#"Cyp3A5_Frequencies", this brings gene frequency data for each of the ethnic groups, values come from PharmGKB database
#Each column represents an ethnic group, each row represents a gene variant, values in the table are the gene frequency
#removes NA values replaces wtih 0, and names the rows after gene variant names
Cyp3A5_Frequencies<- read_excel("CYP3A5_Frequencies.xlsx")
Cyp3A5_Frequencies<- as.data.frame(Cyp3A5_Frequencies)
Cyp3A5_Frequencies[is.na(Cyp3A5_Frequencies)]<- 0
rownames(Cyp3A5_Frequencies)<- Cyp3A5_Frequencies$`CYP3A5 allelec`



#"Cyp3A5_Tests", this brings gene test coverage data collected in our study
#each column represents a specific genetic test, the rows represent each gene variant, "Yes" means gene is covered, "No" means it is not
#row names changed to be the name of each gene variant
Cyp3A5_Tests<- read_excel("Cyp3A5Tests.xlsx")
Cyp3A5_Tests<- as.data.frame(Cyp3A5_Tests)
rownames(Cyp3A5_Tests)<- Cyp3A5_Frequencies$`CYP3A5 allelec`



#"Cyp3A5_Phenotypes", loads table with each potential diplotype and corresponding phenotype, from PharmGKB reference
#each gene of the diplotype has its own column (A1 and A2), the column "Phenotype" designates the metabolizing status of each diplotype,each diplotype is designated as heterozygous(2) or homozygous(1) in a column called multiply
Cyp3A5_Phenotypes<- read_excel("Cyp3A5_Diplotypes.xlsx")
Cyp3A5_Phenotypes<- as.data.frame(Cyp3A5_Phenotypes)
Cyp3A5_Phenotypes<- separate(data= Cyp3A5_Phenotypes, col= 'CYP3A5 Diplotype', into= c("A1", "A2"), sep= "/")
colnames(Cyp3A5_Phenotypes)<- c("A1", "A2", "Phenotype", "EHR")
Cyp3A5_Phenotypes[Cyp3A5_Phenotypes== "CYP3A5 Poor metabolizer"]<- "CYP3A5 Poor Metabolizer"
Cyp3A5_Phenotypes[Cyp3A5_Phenotypes== "CYP3A5 Normal Metabolizerc"]<- "CYP3A5 Normal Metabolizer"
Cyp3A5_Phenotypes$Multiply<- ifelse(Cyp3A5_Phenotypes$A1 == Cyp3A5_Phenotypes$A2, 1, 2)



#takes the same Phenotype table "Cyp2B6_Phenotypes" described above with the same columns, creates a new table called "Cyp2B6_Phenotypes_Test" and makes additional adjustments
#takes the variant names listed in A1 and A2 in the phenotype table and replaces it with a "Yes" or a "No" depending on whether or not the PGx test being assessed covers the gene
#For each calculation of clinical sensitivity, the Test Name has to be replaced in the column title of the Cyp2C19_Tests
Cyp3A5_Phenotypes_Test<- Cyp3A5_Phenotypes
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*1"]<- Cyp3A5_Tests["*1","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*2"]<- Cyp3A5_Tests["*2","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*3"]<- Cyp3A5_Tests["*3","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*4"]<- Cyp3A5_Tests["*4","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*5"]<- Cyp3A5_Tests["*5","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*6"]<- Cyp3A5_Tests["*6","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*7"]<- Cyp3A5_Tests["*7","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*8"]<- Cyp3A5_Tests["*8","Mental Health DNA Insight"] 
Cyp3A5_Phenotypes_Test[Cyp3A5_Phenotypes_Test== "*9"]<- Cyp3A5_Tests["*9","Mental Health DNA Insight"] 

#Creates an addtional column for "Cyp2C19_Phenotypes_Test" called "Test"
#if a phenotype has both genes covered, or "Yes" for both A1 and A2 columns, it is called "Observable", if only 1 of 2 or none say "Yes", it says "Not"
Cyp3A5_Phenotypes_Test$Test<- ifelse(Cyp3A5_Phenotypes_Test$A1 == "Yes"& Cyp3A5_Phenotypes_Test$A2== "Yes", "Observable", "Not")
colnames(Cyp3A5_Phenotypes_Test)<- c("Al.1", "A2.1", "Phenotype", "EHR", "Multiply", "Test")

Cyp3A5_Phenotypes_Test$A1<- Cyp3A5_Phenotypes$A1
Cyp3A5_Phenotypes_Test$A2<- Cyp3A5_Phenotypes$A2



###The next 9 subsections represent calculations for clinical sensitity for each ethnic group using the "Cyp2C19_Phenotypes_Test"
###replaces the allele name with the frequency it occurs in each table
######African-American/Afro-Caribbean Allele Frequency
AfricanPhenotypes<- Cyp3A5_Phenotypes_Test
AfricanPhenotypes[AfricanPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","African American/Afro-Caribbean"] 

#Calculated Probabilities
AfricanPhenotypes$A1<- as.numeric(AfricanPhenotypes$A1)
AfricanPhenotypes$A2<- as.numeric(AfricanPhenotypes$A2)
AfricanPhenotypes$Probability<- AfricanPhenotypes$A1*AfricanPhenotypes$A2
AfricanPhenotypes$Probability<- AfricanPhenotypes$Probability*AfricanPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AfricanAbnormalCorrect<- subset(AfricanPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_AfricanAbnormalCorrect<- sum(AfricanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
AfricanAbnormal<- subset(AfricanPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_AfricanAbnormal<- sum(AfricanAbnormal$Probability)

###Sensitivity
AfricanSensitivity<- (Frequency_AfricanAbnormalCorrect)/(Frequency_AfricanAbnormal)*100



######American Allele Frequency
AmericanPhenotypes<- Cyp3A5_Phenotypes_Test
AmericanPhenotypes[AmericanPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","American"] 

#Calculated Probabilities
AmericanPhenotypes$A1<- as.numeric(AmericanPhenotypes$A1)
AmericanPhenotypes$A2<- as.numeric(AmericanPhenotypes$A2)
AmericanPhenotypes$Probability<- AmericanPhenotypes$A1*AmericanPhenotypes$A2
AmericanPhenotypes$Probability<- AmericanPhenotypes$Probability*AmericanPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AmericanAbnormalCorrect<- subset(AmericanPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_AmericanAbnormalCorrect<- sum(AmericanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
AmericanAbnormal<- subset(AmericanPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_AmericanAbnormal<- sum(AmericanAbnormal$Probability)

###Sensitivity
AmericanSensitivity<- (Frequency_AmericanAbnormalCorrect)/(Frequency_AmericanAbnormal)*100



#########Central/South Asian Allele Frequency
CentralPhenotypes<- Cyp3A5_Phenotypes_Test
CentralPhenotypes[CentralPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","Central/South Asian"] 

#Calculated Probabilities
CentralPhenotypes$A1<- as.numeric(CentralPhenotypes$A1)
CentralPhenotypes$A2<- as.numeric(CentralPhenotypes$A2)
CentralPhenotypes$Probability<- CentralPhenotypes$A1*CentralPhenotypes$A2
CentralPhenotypes$Probability<- CentralPhenotypes$Probability*CentralPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
CentralAbnormalCorrect<- subset(CentralPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_CentralAbnormalCorrect<- sum(CentralAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
CentralAbnormal<- subset(CentralPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_CentralAbnormal<- sum(CentralAbnormal$Probability)

###Sensitivity
CentralSensitivity<- (Frequency_CentralAbnormalCorrect)/(Frequency_CentralAbnormal)*100



#########East Asian Allele Frequency
EastPhenotypes<- Cyp3A5_Phenotypes_Test
EastPhenotypes[EastPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","East Asian"] 
EastPhenotypes[EastPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","East Asian"] 
EastPhenotypes[EastPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","East Asian"] 
EastPhenotypes[EastPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","East Asian"] 
EastPhenotypes[EastPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","East Asian"] 
EastPhenotypes[EastPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","East Asian"] 
EastPhenotypes[EastPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","East Asian"] 
EastPhenotypes[EastPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","East Asian"] 
EastPhenotypes[EastPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","East Asian"] 

#Calculated Probabilities
EastPhenotypes$A1<- as.numeric(EastPhenotypes$A1)
EastPhenotypes$A2<- as.numeric(EastPhenotypes$A2)
EastPhenotypes$Probability<- EastPhenotypes$A1*EastPhenotypes$A2
EastPhenotypes$Probability<- EastPhenotypes$Probability*EastPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
EastAbnormalCorrect<- subset(EastPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_EastAbnormalCorrect<- sum(EastAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
EastAbnormal<- subset(EastPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_EastAbnormal<- sum(EastAbnormal$Probability)

###Sensitivity
EastSensitivity<- (Frequency_EastAbnormalCorrect)/(Frequency_EastAbnormal)*100



########European Allele Frequency
EuropeanPhenotypes<- Cyp3A5_Phenotypes_Test
EuropeanPhenotypes[EuropeanPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","European"] 

#Calculated Probabilities
EuropeanPhenotypes$A1<- as.numeric(EuropeanPhenotypes$A1)
EuropeanPhenotypes$A2<- as.numeric(EuropeanPhenotypes$A2)
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$A1*EuropeanPhenotypes$A2
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$Probability*EuropeanPhenotypes$Multiply

###Population Frequency of Abnormal Phenotypes Correctly Detected 
EuropeanAbnormalCorrect<- subset(EuropeanPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_EuropeanAbnormalCorrect<- sum(EuropeanAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
EuropeanAbnormal<- subset(EuropeanPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_EuropeanAbnormal<- sum(EuropeanAbnormal$Probability)

###Sensitivity
EuropeanSensitivity<- (Frequency_EuropeanAbnormalCorrect)/(Frequency_EuropeanAbnormal)*100


############Latino Allele Frequency
LatinoPhenotypes<- Cyp3A5_Phenotypes_Test
LatinoPhenotypes[LatinoPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","Latino"] 

#Calculated Probabilities
LatinoPhenotypes$A1<- as.numeric(LatinoPhenotypes$A1)
LatinoPhenotypes$A2<- as.numeric(LatinoPhenotypes$A2)
LatinoPhenotypes$Probability<- LatinoPhenotypes$A1*LatinoPhenotypes$A2
LatinoPhenotypes$Probability<- LatinoPhenotypes$Probability*LatinoPhenotypes$Multiply

#Aggregation of Results
###Population Frequency of Abnormal Phenotypes Correctly Detected 
LatinoAbnormalCorrect<- subset(LatinoPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_LatinoAbnormalCorrect<- sum(LatinoAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
LatinoAbnormal<- subset(LatinoPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_LatinoAbnormal<- sum(LatinoAbnormal$Probability)

###Sensitivity
LatinoSensitivity<- (Frequency_LatinoAbnormalCorrect)/(Frequency_LatinoAbnormal)*100



#########Near Eastern Allele Frequency
NearPhenotypes<- Cyp3A5_Phenotypes_Test
NearPhenotypes[NearPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","Near Eastern"] 

#Calculated Probabilities
NearPhenotypes$A1<- as.numeric(NearPhenotypes$A1)
NearPhenotypes$A2<- as.numeric(NearPhenotypes$A2)
NearPhenotypes$Probability<- NearPhenotypes$A1*NearPhenotypes$A2
NearPhenotypes$Probability<- NearPhenotypes$Probability*NearPhenotypes$Multiply

#Aggregation of Results
###Population Frequency of Abnormal Phenotypes Correctly Detected 
NearAbnormalCorrect<- subset(NearPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_NearAbnormalCorrect<- sum(NearAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
NearAbnormal<- subset(NearPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_NearAbnormal<- sum(NearAbnormal$Probability)

###Sensitivity
NearSensitivity<- (Frequency_NearAbnormalCorrect)/(Frequency_NearAbnormal)*100



############Oceanian Allele Frequency
OceanianPhenotypes<- Cyp3A5_Phenotypes_Test
OceanianPhenotypes[OceanianPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","Oceanian"] 

#Calculated Probabilities
OceanianPhenotypes$A1<- as.numeric(OceanianPhenotypes$A1)
OceanianPhenotypes$A2<- as.numeric(OceanianPhenotypes$A2)
OceanianPhenotypes$Probability<- OceanianPhenotypes$A1*OceanianPhenotypes$A2
OceanianPhenotypes$Probability<- OceanianPhenotypes$Probability*OceanianPhenotypes$Multiply

#Aggregation of Results
###Population Frequency of Abnormal Phenotypes Correctly Detected 
OceanianAbnormalCorrect<- subset(OceanianPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_OceanianAbnormalCorrect<- sum(OceanianAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
OceanianAbnormal<- subset(OceanianPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_OceanianAbnormal<- sum(OceanianAbnormal$Probability)

###Sensitivity
OceanianSensitivity<- (Frequency_OceanianAbnormalCorrect)/(Frequency_OceanianAbnormal)*100



#########Sub-Saharan Allele African Frequency
SubPhenotypes<- Cyp3A5_Phenotypes_Test
SubPhenotypes[SubPhenotypes== "*1"]<- Cyp3A5_Frequencies["*1","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*2"]<- Cyp3A5_Frequencies["*2","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*3"]<- Cyp3A5_Frequencies["*3","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*4"]<- Cyp3A5_Frequencies["*4","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*5"]<- Cyp3A5_Frequencies["*5","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*6"]<- Cyp3A5_Frequencies["*6","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*7"]<- Cyp3A5_Frequencies["*7","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*8"]<- Cyp3A5_Frequencies["*8","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*9"]<- Cyp3A5_Frequencies["*9","Sub-Saharan African"] 

#Calculated Probabilities
SubPhenotypes$A1<- as.numeric(SubPhenotypes$A1)
SubPhenotypes$A2<- as.numeric(SubPhenotypes$A2)
SubPhenotypes$Probability<- SubPhenotypes$A1*SubPhenotypes$A2
SubPhenotypes$Probability<- SubPhenotypes$Probability*SubPhenotypes$Multiply

#Aggregation of Results
###Population Frequency of Abnormal Phenotypes Correctly Detected 
SubAbnormalCorrect<- subset(SubPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer" & Test== "Observable")
Frequency_SubAbnormalCorrect<- sum(SubAbnormalCorrect$Probability)

###Total Population Frequency of Abnormal Phenotypes
SubAbnormal<- subset(SubPhenotypes, Phenotype != "CYP3A5 Poor Metabolizer")
Frequency_SubAbnormal<- sum(SubAbnormal$Probability)

###Sensitivity
SubSensitivity<- (Frequency_SubAbnormalCorrect)/(Frequency_SubAbnormal)*100



#Aggregated Sensitivty Results for all ethnic groups for PGx test, There are no results for "American" or "Oceanian" because there are no published gene frequencies in PharmGKB
TestSensitivity<- c(AfricanSensitivity, AmericanSensitivity, CentralSensitivity, EastSensitivity, EuropeanSensitivity, LatinoSensitivity, NearSensitivity, OceanianSensitivity, SubSensitivity)
TestSensitivity<- as.data.frame(TestSensitivity)
rownames(TestSensitivity)<- c("African American/Afro-Caribbean", "American", "Central/South Asian", "East Asian", "European", "Latino", "Near Eastern", "Oceanian", "Sub-Saharan")



#"Cyp3A5_Counts", gives the total number of variants each PGx test selects, and the percent coverge of total variants targeted by each test from the total listed variants listed on PharmGKB
#Calcluations already performed, this is only exporting the values
Cyp3A5_Counts<- as.data.frame(read_excel("Cyp3A5Tests.xlsx", sheet= "Sheet2"))

#Complete Dataset for all PGx tests
Cyp3A5_Sensitivity_Tests




