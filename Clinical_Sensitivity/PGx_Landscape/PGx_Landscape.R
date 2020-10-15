###PGxLandscape Figure
setwd("~/Desktop/Clinical_Sensitivity/PGx_Landscape")



###Single And Combinatorial Tests
SingComb<- as.data.frame(read_excel("SingleComb.xlsx"))
SingComb[SingComb== "Comb"]<- "Combinatorial"
SingComb[SingComb== "Single"]<- "Single"
SingComb<- SingComb %>% filter(Gene %in% c("CYP2D6", "CYP2C19", "CYP2C9", "CYP2B6", "CYP3A5"))
positions<- c("CYP2D6", "CYP2C19", "CYP2C9", "CYP2B6", "CYP3A5")

SingCombPlot<- SingComb %>% ggplot(aes(x= Gene, y= Count, fill= Gene, alpha= Type)) + geom_bar(stat= "identity") + scale_alpha_manual(values= c(.4,1,.4,1,.4,1,.4,1,.4))+
  scale_x_discrete(limits= positions) + ylab("Number of Tests") + guides(fill= FALSE)+
  theme_minimal() + theme(axis.title= element_text(face= "bold", size= 16), axis.title.x= element_blank(), axis.text= element_text(face= "bold", size=12), plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_bar_text(position= "stack")



###FDA and CPIC Documentation
CPIC<- as.data.frame(read_excel("CPIC.xlsx"))
CPIC<- CPIC %>% filter(Gene %in% c("CYP2A7P1", "CYP2B6", "CYP2C19", "CYP2C8", "CYP2C9", "CYP2D6", "CYP3A4", "CYP3A5", "CYP4F2", "TOP300"))
colnames(CPIC)<- c("Gene", "Drug", "EvidenceCPIC", "EvidenceGKB", "Label")
CPIC<- CPIC %>% filter(EvidenceCPIC %in% c("A", "A/B", "B", "C", "C/D"))
SummaryCPIC<- CPIC %>% group_by(Gene) %>% summarize(count= n()) %>% arrange(count) %>% as.data.frame()
SummaryCPIC$Source<- "CPIC"

FDA<- as.data.frame(read_excel("CYPFDA.xlsx"))
FDA<- FDA %>% filter(Biomarker %in% c("CYP2A7P1", "CYP2B6", "CYP2C19", "CYP2C8", "CYP2C9", "CYP2D6", "CYP3A4", "CYP3A5", "CYP4F2", "TOP300"))
SummaryFDA<- FDA %>% group_by(Biomarker) %>% summarize(count= n()) %>% arrange(count) %>% as.data.frame()
SummaryFDA$Source<- "FDA"
colnames(SummaryFDA)<- c("Gene", "count", "Source")
SummaryFDA<- SummaryFDA %>% filter( Gene != "TOP300")

FDACpic<- rbind(SummaryCPIC, SummaryFDA)

positions<- c("CYP2D6", "CYP2C19", "CYP2C9", "CYP2B6", "CYP3A5")

FDACpicPlot1<- FDACpic %>% ggplot(aes(x=Gene, y= count, fill= Gene, alpha= Source)) + geom_bar(stat= "identity") + scale_alpha_manual(values= c(.4,1,.4,1,.4,1,.4,1,.4)) + 
  scale_x_discrete(limits= positions) + ylab("Number of Medications") + 
  theme_minimal() + theme(axis.title= element_text(face= "bold", size= 16), axis.title.x= element_blank(), axis.text.y= element_text(face= "bold", size=12), axis.text.x = element_blank(), plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_bar_text(position= "stack")

a<- ggarrange(FDACpicPlot1, SingCombPlot, nrow = 2, labels= c("B", "C"),legend= "right", align= c("v"))


###Total number of genes selected by CPGx Tests
GENECOUNT<- as.data.frame(t(read_excel("~/Desktop/CypProject/TOTCYPPUB.xlsx")))
colnames(GENECOUNT)<- "Totals"
CYPtotals<- as.data.frame(GENECOUNT[4:9,])
colnames(CYPtotals)<- "Total"
Targetted<- c("Five", "Four", "Three", "Two", "One", "Zero")
CYPENZYMETOTALS<- cbind(CYPtotals, Targetted)
CYPENZYMETOTALS$Targetted<- factor(CYPENZYMETOTALS$Targetted, levels= c("Zero", "One", "Two", "Three", "Four", "Five"))

NumTargettedPlot<- CYPENZYMETOTALS %>% ggplot(aes(x= Targetted, y= Total)) + geom_bar(stat= "identity") +
  ylab("Number of Tests") + 
  theme_minimal() + theme(axis.title.y= element_text(face= "bold", size= 16), axis.title.x= element_blank(),axis.text= element_text(face= "bold", size= 12),plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_bar_text(position= "stack")

b<- ggarrange(NumTargettedPlot, nrow=1, labels= c("A"))


###Combine all Graphs
PGx_Landscape<- ggarrange(b, a, ncol=2, widths = c(1,2), align= c("hv"))

