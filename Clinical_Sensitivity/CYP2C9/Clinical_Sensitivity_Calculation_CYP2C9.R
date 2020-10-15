###Cyp2C9 Sensitivity
setwd("~/Desktop/Clinical_Sensitivity/CYP2C9")

#"Cyp2C9_Frequencies", this brings gene frequency data for each of the ethnic groups, values come from PharmGKB database
#Each column represents an ethnic group, each row represents a gene variant, values in the table are the gene frequency
#removes NA values replaces wtih 0, and names the rows after gene variant names
Cyp2C9_Frequencies<- read_excel("CYP2C9_Frequencies.xlsx")
Cyp2C9_Frequencies<- as.data.frame(Cyp2C9_Frequencies)
Cyp2C9_Frequencies[is.na(Cyp2C9_Frequencies)]<- 0
rownames(Cyp2C9_Frequencies)<- Cyp2C9_Frequencies$`CYP2C9 allele`



#"Cyp2C9_Tests", this brings gene test coverage data collected in our study
#each column represents a specific genetic test, the rows represent each gene variant, "Yes" means gene is covered, "No" means it is not
#row names changed to be the name of each gene variant
Cyp2C9_Tests<- read_excel("Cyp2C9Tests.xlsx")
Cyp2C9_Tests<- as.data.frame(Cyp2C9_Tests)
rownames(Cyp2C9_Tests)<- Cyp2C9_Frequencies$`CYP2C9 allele`



#"Cyp2C9_Phenotypes", loads table with each potential diplotype and corresponding phenotype, from PharmGKB reference
#each gene of the diplotype has its own column (A1 and A2), the column "Phenotype" designates the metabolizing status of each diplotype,each diplotype is designated as heterozygous(2) or homozygous(1) in a column called multiply
Cyp2C9_Phenotypes<- read_excel("Cyp2C9_Diplotypes.xlsx")
Cyp2C9_Phenotypes<- as.data.frame(Cyp2C9_Phenotypes)
Cyp2C9_Phenotypes<- separate(data= Cyp2C9_Phenotypes, col= 'CYP2C9 Diplotype', into= c("A1", "A2"), sep= "/")
colnames(Cyp2C9_Phenotypes)<- c("A1", "A2", "Score","Phenotype", "EHR")
Cyp2C9_Phenotypes[Cyp2C9_Phenotypes== "*2 "]<- "*2"
Cyp2C9_Phenotypes$Multiply<- ifelse(Cyp2C9_Phenotypes$A1 == Cyp2C9_Phenotypes$A2, 1, 2)



#takes the same Phenotype table "Cyp2C9_Phenotypes" described above with the same columns, creates a new table called "Cyp2C9_Phenotypes_Test" and makes additional adjustments
#takes the variant names listed in A1 and A2 in the phenotype table and replaces it with a "Yes" or a "No" depending on whether or not the PGx test being assessed covers the gene
#For each calculation of clinical sensitivity, the Test Name has to be replaced in the column title of the Cyp2C19_Tests
Cyp2C9_Phenotypes_Test<- Cyp2C9_Phenotypes
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*1"]<- Cyp2C9_Tests["*1","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*2"]<- Cyp2C9_Tests["*2","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*3"]<- Cyp2C9_Tests["*3","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*4"]<- Cyp2C9_Tests["*4","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*5"]<- Cyp2C9_Tests["*5","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*6"]<- Cyp2C9_Tests["*6","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*7"]<- Cyp2C9_Tests["*7","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*8"]<- Cyp2C9_Tests["*8","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*9"]<- Cyp2C9_Tests["*9","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*10"]<- Cyp2C9_Tests["*10","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*11"]<- Cyp2C9_Tests["*11","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*12"]<- Cyp2C9_Tests["*12","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*13"]<- Cyp2C9_Tests["*13","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*14"]<- Cyp2C9_Tests["*14","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*15"]<- Cyp2C9_Tests["*15","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*16"]<- Cyp2C9_Tests["*16","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*17"]<- Cyp2C9_Tests["*17","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*18"]<- Cyp2C9_Tests["*18","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*19"]<- Cyp2C9_Tests["*19","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*20"]<- Cyp2C9_Tests["*20","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*21"]<- Cyp2C9_Tests["*21","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*22"]<- Cyp2C9_Tests["*22","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*23"]<- Cyp2C9_Tests["*23","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*24"]<- Cyp2C9_Tests["*24","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*25"]<- Cyp2C9_Tests["*25","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*26"]<- Cyp2C9_Tests["*26","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*27"]<- Cyp2C9_Tests["*27","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*28"]<- Cyp2C9_Tests["*28","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*29"]<- Cyp2C9_Tests["*29","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*30"]<- Cyp2C9_Tests["*30","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*31"]<- Cyp2C9_Tests["*31","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*32"]<- Cyp2C9_Tests["*32","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*33"]<- Cyp2C9_Tests["*33","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*34"]<- Cyp2C9_Tests["*34","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*35"]<- Cyp2C9_Tests["*35","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*36"]<- Cyp2C9_Tests["*36","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*37"]<- Cyp2C9_Tests["*37","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*38"]<- Cyp2C9_Tests["*38","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*39"]<- Cyp2C9_Tests["*39","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*40"]<- Cyp2C9_Tests["*40","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*41"]<- Cyp2C9_Tests["*41","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*42"]<- Cyp2C9_Tests["*42","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*43"]<- Cyp2C9_Tests["*43","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*44"]<- Cyp2C9_Tests["*44","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*45"]<- Cyp2C9_Tests["*45","Tier Two"] 
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*46"]<- Cyp2C9_Tests["*46","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*47"]<- Cyp2C9_Tests["*47","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*48"]<- Cyp2C9_Tests["*48","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*49"]<- Cyp2C9_Tests["*49","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*50"]<- Cyp2C9_Tests["*50","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*51"]<- Cyp2C9_Tests["*51","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*52"]<- Cyp2C9_Tests["*52","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*53"]<- Cyp2C9_Tests["*53","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*54"]<- Cyp2C9_Tests["*54","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*55"]<- Cyp2C9_Tests["*55","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*56"]<- Cyp2C9_Tests["*56","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*57"]<- Cyp2C9_Tests["*57","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*58"]<- Cyp2C9_Tests["*58","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*59"]<- Cyp2C9_Tests["*59","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*60"]<- Cyp2C9_Tests["*60","Tier Two"]
Cyp2C9_Phenotypes_Test[Cyp2C9_Phenotypes_Test== "*61"]<- Cyp2C9_Tests["*61","Tier Two"]

#Creates an addtional column for "Cyp2C19_Phenotypes_Test" called "Test"
#if a phenotype has both genes covered, or "Yes" for both A1 and A2 columns, it is called "Observable", if only 1 of 2 or none say "Yes", it says "Not"
Cyp2C9_Phenotypes_Test$Test<- ifelse(Cyp2C9_Phenotypes_Test$A1 == "Yes"& Cyp2C9_Phenotypes_Test$A2== "Yes", "Observable", "Not")
colnames(Cyp2C9_Phenotypes_Test)<- c("Al.1", "A2.1", "Score", "Phenotype", "EHR", "Multiply", "Test")

Cyp2C9_Phenotypes_Test$A1<- Cyp2C9_Phenotypes$A1
Cyp2C9_Phenotypes_Test$A2<- Cyp2C9_Phenotypes$A2



###The next 9 subsections represent calculations for clinical sensitity for each ethnic group using the "Cyp2C19_Phenotypes_Test"
###replaces the allele name with the frequency it occurs in each table
######African Allele Frequency
AfricanPhenotypes<- Cyp2C9_Phenotypes_Test
AfricanPhenotypes[AfricanPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","African American/Afro-Caribbean"] 
AfricanPhenotypes[AfricanPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","African American/Afro-Caribbean"]
AfricanPhenotypes[AfricanPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","African American/Afro-Caribbean"]

#Probabilities of Each Diplotype
AfricanPhenotypes$A1<- as.numeric(AfricanPhenotypes$A1)
AfricanPhenotypes$A2<- as.numeric(AfricanPhenotypes$A2)
AfricanPhenotypes$Multiply<- as.numeric(AfricanPhenotypes$Multiply)
AfricanPhenotypes$Probability<- AfricanPhenotypes$A1*AfricanPhenotypes$A2
AfricanPhenotypes$Probability<- AfricanPhenotypes$Probability*AfricanPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
AfricanAbnormal<- subset(AfricanPhenotypes, Score <1 | Score>2)
SumAfricanAbnormal<- sum(AfricanAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AfricanAbnormalCorrect<- subset(AfricanAbnormal, Test== "Observable")
SumAfricanAbnormalCorrect<- sum(AfricanAbnormalCorrect$Probability)

###Sensitivity
AfricanSensitivity<- SumAfricanAbnormalCorrect/SumAfricanAbnormal*100



######American Allele Frequency
AmericanPhenotypes<- Cyp2C9_Phenotypes_Test
AmericanPhenotypes[AmericanPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","American"]
AmericanPhenotypes[AmericanPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","American"]
AmericanPhenotypes[AmericanPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","American"]
AmericanPhenotypes[AmericanPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","American"]
AmericanPhenotypes[AmericanPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","American"]
AmericanPhenotypes[AmericanPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","American"] 
AmericanPhenotypes[AmericanPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","American"]
AmericanPhenotypes[AmericanPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","American"]
AmericanPhenotypes[AmericanPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","American"]
AmericanPhenotypes[AmericanPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","American"]
AmericanPhenotypes[AmericanPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","American"]
AmericanPhenotypes[AmericanPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","American"]
AmericanPhenotypes[AmericanPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","American"]
AmericanPhenotypes[AmericanPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","American"]
AmericanPhenotypes[AmericanPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","American"]
AmericanPhenotypes[AmericanPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","American"]
AmericanPhenotypes[AmericanPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","American"]
AmericanPhenotypes[AmericanPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","American"]
AmericanPhenotypes[AmericanPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","American"]
AmericanPhenotypes[AmericanPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","American"]
AmericanPhenotypes[AmericanPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","American"]
AmericanPhenotypes[AmericanPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","American"]

#Probabilities of Each Diplotype
AmericanPhenotypes$A1<- as.numeric(AmericanPhenotypes$A1)
AmericanPhenotypes$A2<- as.numeric(AmericanPhenotypes$A2)
AmericanPhenotypes$Multiply<- as.numeric(AmericanPhenotypes$Multiply)
AmericanPhenotypes$Probability<- AmericanPhenotypes$A1*AmericanPhenotypes$A2
AmericanPhenotypes$Probability<- AmericanPhenotypes$Probability*AmericanPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
AmericanAbnormal<- subset(AmericanPhenotypes, Score <1 | Score>2)
SumAmericanAbnormal<- sum(AmericanAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected 
AmericanAbnormalCorrect<- subset(AmericanAbnormal, Test== "Observable")
SumAmericanAbnormalCorrect<- sum(AmericanAbnormalCorrect$Probability)

#Sensitivity
AmericanSensitivity<- SumAmericanAbnormalCorrect/SumAmericanAbnormal*100



########Central Allele Frequency
CentralPhenotypes<- Cyp2C9_Phenotypes_Test
CentralPhenotypes[CentralPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","Central/South Asian"] 
CentralPhenotypes[CentralPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","Central/South Asian"]
CentralPhenotypes[CentralPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","Central/South Asian"]

#Probabilities of Each Diplotype
CentralPhenotypes$A1<- as.numeric(CentralPhenotypes$A1)
CentralPhenotypes$A2<- as.numeric(CentralPhenotypes$A2)
CentralPhenotypes$Multiply<- as.numeric(CentralPhenotypes$Multiply)
CentralPhenotypes$Probability<- CentralPhenotypes$A1*CentralPhenotypes$A2
CentralPhenotypes$Probability<- CentralPhenotypes$Probability*CentralPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
CentralAbnormal<- subset(CentralPhenotypes, Score <1 | Score>2)
SumCentralAbnormal<- sum(CentralAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected 
CentralAbnormalCorrect<- subset(CentralAbnormal, Test== "Observable")
SumCentralAbnormalCorrect<- sum(CentralAbnormalCorrect$Probability)

#Sensitivity
CentralSensitivity<- SumCentralAbnormalCorrect/SumCentralAbnormal*100



#####East Allele Frequency
EastPhenotypes<- Cyp2C9_Phenotypes_Test
EastPhenotypes[EastPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","East Asian"]
EastPhenotypes[EastPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","East Asian"] 
EastPhenotypes[EastPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","East Asian"]
EastPhenotypes[EastPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","East Asian"] 
EastPhenotypes[EastPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","East Asian"] 
EastPhenotypes[EastPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","East Asian"] 
EastPhenotypes[EastPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","East Asian"] 
EastPhenotypes[EastPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","East Asian"] 
EastPhenotypes[EastPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","East Asian"] 
EastPhenotypes[EastPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","East Asian"] 
EastPhenotypes[EastPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","East Asian"]
EastPhenotypes[EastPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","East Asian"] 
EastPhenotypes[EastPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","East Asian"] 
EastPhenotypes[EastPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","East Asian"] 
EastPhenotypes[EastPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","East Asian"]
EastPhenotypes[EastPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","East Asian"] 
EastPhenotypes[EastPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","East Asian"] 
EastPhenotypes[EastPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","East Asian"] 
EastPhenotypes[EastPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","East Asian"] 
EastPhenotypes[EastPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","East Asian"] 
EastPhenotypes[EastPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","East Asian"]
EastPhenotypes[EastPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","East Asian"] 
EastPhenotypes[EastPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","East Asian"] 
EastPhenotypes[EastPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","East Asian"] 
EastPhenotypes[EastPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","East Asian"] 
EastPhenotypes[EastPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","East Asian"] 
EastPhenotypes[EastPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","East Asian"] 
EastPhenotypes[EastPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","East Asian"] 
EastPhenotypes[EastPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","East Asian"] 
EastPhenotypes[EastPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","East Asian"] 
EastPhenotypes[EastPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","East Asian"] 
EastPhenotypes[EastPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","East Asian"] 
EastPhenotypes[EastPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","East Asian"] 
EastPhenotypes[EastPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","East Asian"] 
EastPhenotypes[EastPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","East Asian"] 
EastPhenotypes[EastPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","East Asian"] 
EastPhenotypes[EastPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","East Asian"] 
EastPhenotypes[EastPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","East Asian"] 
EastPhenotypes[EastPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","East Asian"] 
EastPhenotypes[EastPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","East Asian"] 
EastPhenotypes[EastPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","East Asian"] 
EastPhenotypes[EastPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","East Asian"] 
EastPhenotypes[EastPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","East Asian"] 
EastPhenotypes[EastPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","East Asian"] 
EastPhenotypes[EastPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","East Asian"] 
EastPhenotypes[EastPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","East Asian"]
EastPhenotypes[EastPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","East Asian"]
EastPhenotypes[EastPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","East Asian"]
EastPhenotypes[EastPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","East Asian"]
EastPhenotypes[EastPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","East Asian"]
EastPhenotypes[EastPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","East Asian"]
EastPhenotypes[EastPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","East Asian"]
EastPhenotypes[EastPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","East Asian"]
EastPhenotypes[EastPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","East Asian"]
EastPhenotypes[EastPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","East Asian"]
EastPhenotypes[EastPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","East Asian"]
EastPhenotypes[EastPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","East Asian"]
EastPhenotypes[EastPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","East Asian"]
EastPhenotypes[EastPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","East Asian"]
EastPhenotypes[EastPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","East Asian"]
EastPhenotypes[EastPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","East Asian"]

#Probabilities of Each Diplotype
EastPhenotypes$A1<- as.numeric(EastPhenotypes$A1)
EastPhenotypes$A2<- as.numeric(EastPhenotypes$A2)
EastPhenotypes$Multiply<- as.numeric(EastPhenotypes$Multiply)
EastPhenotypes$Probability<- EastPhenotypes$A1*EastPhenotypes$A2
EastPhenotypes$Probability<- EastPhenotypes$Probability*EastPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
EastAbnormal<- subset(EastPhenotypes, Score <1 | Score>2)
SumEastAbnormal<- sum(EastAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected
EastAbnormalCorrect<- subset(EastAbnormal, Test== "Observable")
SumEastAbnormalCorrect<- sum(EastAbnormalCorrect$Probability)

#Sensitivity
EastSensitivity<- SumEastAbnormalCorrect/SumEastAbnormal*100



########European Allele Frequency
EuropeanPhenotypes<- Cyp2C9_Phenotypes_Test
EuropeanPhenotypes[EuropeanPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","European"] 
EuropeanPhenotypes[EuropeanPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","European"]
EuropeanPhenotypes[EuropeanPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","European"]

#Probabilities of Each Diplotype
EuropeanPhenotypes$A1<- as.numeric(EuropeanPhenotypes$A1)
EuropeanPhenotypes$A2<- as.numeric(EuropeanPhenotypes$A2)
EuropeanPhenotypes$Multiply<- as.numeric(EuropeanPhenotypes$Multiply)
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$A1*EuropeanPhenotypes$A2
EuropeanPhenotypes$Probability<- EuropeanPhenotypes$Probability*EuropeanPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
EuropeanAbnormal<- subset(EuropeanPhenotypes, Score <1 | Score>2)
SumEuropeanAbnormal<- sum(EuropeanAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected
EuropeanAbnormalCorrect<- subset(EuropeanAbnormal, Test== "Observable")
SumEuropeanAbnormalCorrect<- sum(EuropeanAbnormalCorrect$Probability)

#Sensitivity
EuropeanSensitivity<- SumEuropeanAbnormalCorrect/SumEuropeanAbnormal*100



#######Latino Allele Frequency
LatinoPhenotypes<- Cyp2C9_Phenotypes_Test
LatinoPhenotypes[LatinoPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","Latino"] 
LatinoPhenotypes[LatinoPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","Latino"]
LatinoPhenotypes[LatinoPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","Latino"]

#Probabilities of Each Diplotype
LatinoPhenotypes$A1<- as.numeric(LatinoPhenotypes$A1)
LatinoPhenotypes$A2<- as.numeric(LatinoPhenotypes$A2)
LatinoPhenotypes$Multiply<- as.numeric(LatinoPhenotypes$Multiply)
LatinoPhenotypes$Probability<- LatinoPhenotypes$A1*LatinoPhenotypes$A2
LatinoPhenotypes$Probability<- LatinoPhenotypes$Probability*LatinoPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
LatinoAbnormal<- subset(LatinoPhenotypes, Score <1 | Score>2)
SumLatinoAbnormal<- sum(LatinoAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected
LatinoAbnormalCorrect<- subset(LatinoAbnormal, Test== "Observable")
SumLatinoAbnormalCorrect<- sum(LatinoAbnormalCorrect$Probability)

#Sensitivity
LatinoSensitivity<- SumLatinoAbnormalCorrect/SumLatinoAbnormal*100



##############Near Allele Frequency
NearPhenotypes<- Cyp2C9_Phenotypes_Test
NearPhenotypes[NearPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","Near Eastern"] 
NearPhenotypes[NearPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","Near Eastern"]
NearPhenotypes[NearPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","Near Eastern"]

#Probabilities of Each Diplotype
NearPhenotypes$A1<- as.numeric(NearPhenotypes$A1)
NearPhenotypes$A2<- as.numeric(NearPhenotypes$A2)
NearPhenotypes$Multiply<- as.numeric(NearPhenotypes$Multiply)
NearPhenotypes$Probability<- NearPhenotypes$A1*NearPhenotypes$A2
NearPhenotypes$Probability<- NearPhenotypes$Probability*NearPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
NearAbnormal<- subset(NearPhenotypes, Score <1 | Score>2)
SumNearAbnormal<- sum(NearAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected
NearAbnormalCorrect<- subset(NearAbnormal, Test== "Observable")
SumNearAbnormalCorrect<- sum(NearAbnormalCorrect$Probability)

#Sensitivity
NearSensitivity<- SumNearAbnormalCorrect/SumNearAbnormal*100



#######Oceanian Allele Frequency
OceanianPhenotypes<- Cyp2C9_Phenotypes_Test
OceanianPhenotypes[OceanianPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","Oceanian"] 
OceanianPhenotypes[OceanianPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","Oceanian"]
OceanianPhenotypes[OceanianPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","Oceanian"]

#Probabilities of Each Diplotype
OceanianPhenotypes$A1<- as.numeric(OceanianPhenotypes$A1)
OceanianPhenotypes$A2<- as.numeric(OceanianPhenotypes$A2)
OceanianPhenotypes$Multiply<- as.numeric(OceanianPhenotypes$Multiply)
OceanianPhenotypes$Probability<- OceanianPhenotypes$A1*OceanianPhenotypes$A2
OceanianPhenotypes$Probability<- OceanianPhenotypes$Probability*OceanianPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
OceanianAbnormal<- subset(OceanianPhenotypes, Score <1 | Score>2)
SumOceanianAbnormal<- sum(OceanianAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected 
OceanianAbnormalCorrect<- subset(OceanianAbnormal, Test== "Observable")
SumOceanianAbnormalCorrect<- sum(OceanianAbnormalCorrect$Probability)

#Sensitivity
OceanianSensitivity<- SumOceanianAbnormalCorrect/SumOceanianAbnormal*100



###Sub-saharan African Frequency 
SubPhenotypes<- Cyp2C9_Phenotypes_Test
SubPhenotypes[SubPhenotypes== "*1"]<- Cyp2C9_Frequencies["*1","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*2"]<- Cyp2C9_Frequencies["*2","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*3"]<- Cyp2C9_Frequencies["*3","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*4"]<- Cyp2C9_Frequencies["*4","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*5"]<- Cyp2C9_Frequencies["*5","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*6"]<- Cyp2C9_Frequencies["*6","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*7"]<- Cyp2C9_Frequencies["*7","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*8"]<- Cyp2C9_Frequencies["*8","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*9"]<- Cyp2C9_Frequencies["*9","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*10"]<- Cyp2C9_Frequencies["*10","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*11"]<- Cyp2C9_Frequencies["*11","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*12"]<- Cyp2C9_Frequencies["*12","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*13"]<- Cyp2C9_Frequencies["*13","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*14"]<- Cyp2C9_Frequencies["*14","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*15"]<- Cyp2C9_Frequencies["*15","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*16"]<- Cyp2C9_Frequencies["*16","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*17"]<- Cyp2C9_Frequencies["*17","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*18"]<- Cyp2C9_Frequencies["*18","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*19"]<- Cyp2C9_Frequencies["*19","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*20"]<- Cyp2C9_Frequencies["*20","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*21"]<- Cyp2C9_Frequencies["*21","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*22"]<- Cyp2C9_Frequencies["*22","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*23"]<- Cyp2C9_Frequencies["*23","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*24"]<- Cyp2C9_Frequencies["*24","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*25"]<- Cyp2C9_Frequencies["*25","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*26"]<- Cyp2C9_Frequencies["*26","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*27"]<- Cyp2C9_Frequencies["*27","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*28"]<- Cyp2C9_Frequencies["*28","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*29"]<- Cyp2C9_Frequencies["*29","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*30"]<- Cyp2C9_Frequencies["*30","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*31"]<- Cyp2C9_Frequencies["*31","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*32"]<- Cyp2C9_Frequencies["*32","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*33"]<- Cyp2C9_Frequencies["*33","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*34"]<- Cyp2C9_Frequencies["*34","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*35"]<- Cyp2C9_Frequencies["*35","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*36"]<- Cyp2C9_Frequencies["*36","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*37"]<- Cyp2C9_Frequencies["*37","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*38"]<- Cyp2C9_Frequencies["*38","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*39"]<- Cyp2C9_Frequencies["*39","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*40"]<- Cyp2C9_Frequencies["*40","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*41"]<- Cyp2C9_Frequencies["*41","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*42"]<- Cyp2C9_Frequencies["*42","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*43"]<- Cyp2C9_Frequencies["*43","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*44"]<- Cyp2C9_Frequencies["*44","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*45"]<- Cyp2C9_Frequencies["*45","Sub-Saharan African"] 
SubPhenotypes[SubPhenotypes== "*46"]<- Cyp2C9_Frequencies["*46","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*47"]<- Cyp2C9_Frequencies["*47","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*48"]<- Cyp2C9_Frequencies["*48","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*49"]<- Cyp2C9_Frequencies["*49","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*50"]<- Cyp2C9_Frequencies["*50","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*51"]<- Cyp2C9_Frequencies["*51","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*52"]<- Cyp2C9_Frequencies["*52","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*53"]<- Cyp2C9_Frequencies["*53","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*54"]<- Cyp2C9_Frequencies["*54","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*55"]<- Cyp2C9_Frequencies["*55","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*56"]<- Cyp2C9_Frequencies["*56","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*57"]<- Cyp2C9_Frequencies["*57","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*58"]<- Cyp2C9_Frequencies["*58","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*59"]<- Cyp2C9_Frequencies["*59","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*60"]<- Cyp2C9_Frequencies["*60","Sub-Saharan African"]
SubPhenotypes[SubPhenotypes== "*61"]<- Cyp2C9_Frequencies["*61","Sub-Saharan African"]

#Probabilities of Each Diplotype
SubPhenotypes$A1<- as.numeric(SubPhenotypes$A1)
SubPhenotypes$A2<- as.numeric(SubPhenotypes$A2)
SubPhenotypes$Multiply<- as.numeric(SubPhenotypes$Multiply)
SubPhenotypes$Probability<- SubPhenotypes$A1*SubPhenotypes$A2
SubPhenotypes$Probability<- SubPhenotypes$Probability*SubPhenotypes$Multiply

###Total Population Frequency of Abnormal Phenotypes
SubAbnormal<- subset(SubPhenotypes, Score <1 | Score>2)
SumSubAbnormal<- sum(SubAbnormal$Probability)

###Population Frequency of Abnormal Phenotypes Correctly Detected 
SubAbnormalCorrect<- subset(SubAbnormal, Test== "Observable")
SumSubAbnormalCorrect<- sum(SubAbnormalCorrect$Probability)

#Sensitivity
SubSensitivity<- SumSubAbnormalCorrect/SumSubAbnormal*100



#Results
#Sensitivity
TestSensitivity<- c(AfricanSensitivity, AmericanSensitivity, CentralSensitivity, EastSensitivity, EuropeanSensitivity, LatinoSensitivity, NearSensitivity, OceanianSensitivity, SubSensitivity)
TestSensitivity<- as.data.frame(TestSensitivity)
rownames(TestSensitivity)<- c("African-American/Afro-Caribbean", "American", "Central/South Asian", "East Asian", "European", "Latino", "Near Eastern", "Oceanian", "Sub-Saharan")



#"Cyp2C19_Counts", gives the total number of variants each PGx test selects, and the percent coverge of total variants targeted by each test from the total listed variants listed on PharmGKB
#Calcluations already performed, this is only exporting the values
Cyp2C9_Counts<- as.data.frame(read_excel("Cyp2C9Tests.xlsx", sheet= "Sheet2"))

Cyp2C9_Sensitivity_Tests

