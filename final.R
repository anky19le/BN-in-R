#Q1
load("~/cad1.RData")
library(bnlearn)
# create an empty graph
dag <- empty.graph(nodes = c("Sex", "Smoker", "Inherit", "Hyperchol", "SuffHeartF", "CAD"))
# dag
plot(dag)
#create edges
dag <- set.arc(dag, from = "Sex", to = "Smoker") # 
dag <- set.arc(dag, from = "Smoker", to = "Inherit") 
dag <- set.arc(dag, from = "Smoker", to = "Hyperchol") # ....
dag <- set.arc(dag, from = "SuffHeartF", to = "Hyperchol") # ....
dag <- set.arc(dag, from = "Hyperchol", to = "CAD") # ....
dag <- set.arc(dag, from = "Inherit", to = "CAD") 

# dag
# modelstring(dag)
# nodes(dag)
# arcs(dag)
plot(dag)
#create dataframe df as we are considering only specific variables from the data
df <- cad1[,c("Sex", "Smoker", "Inherit", "CAD", "SuffHeartF", "Hyperchol")]
head(df)
#check dimension
dim(df)
# Fit the DAG (MLE)
# Use of the Data only
bn.mle <- bn.fit(dag, data = df, method = "mle")
bn.mle

#Q1.2
#d-separations in the graph
dsep(dag, x = "Inherit", y = "SuffHeartF")
dsep(dag, x = "Sex", y = "SuffHeartF")
dsep(dag, x = "Smoker", y = "SuffHeartF")
# dsep given information in the graph
dsep(dag, x = "Inherit", y = "Hyperchol",z="Smoker")
dsep(dag, x = "Sex", y = "Hyperchol",z="Smoker")
dsep(dag, x = "Sex", y = "Inherit",z="Smoker")

#Q1.3(try for bayes)
junction <- compile(as.grain(bn.mle))
jsex_hyperchol <- setEvidence(junction, nodes = c("Sex", "Hyperchol"), states = c("Male", "Yes"))
suffheart_cad <- querygrain(jsex_hyperchol, nodes = c("SuffHeartF","CAD"),type="joint")
suffheart_cad

#Q1.4
#Bayesian Fit (with a prior, sample size of prior = 10)
bn.bayes1 <- bn.fit(dag, data = df, method = "bayes", iss = 10)
bn.bayes1

#Q1.5
# Bayesian Fit (with a prior, sample size of prior = 50)
bn.bayes2 <- bn.fit(dag, data = df, method = "bayes", iss = 50)
bn.bayes2

#Q1.6
# Bayesian Fit (with a prior, sample size of prior = 1000)
bn.bayes3 <- bn.fit(dag, data = df, method = "bayes", iss = 1000)
bn.bayes3

#Q1.7
jsmoker_cad <- setEvidence(junction, nodes = c("Smoker", "CAD"), states = c("Yes", "Yes"))
querygrain(jsmoker_cad)

#Q1.8
bn.bayes <- bn.fit(dag, data = df, method = "bayes")
junction2 <- compile(as.grain(bn.bayes))
jsmoker_cad2 <- setEvidence(junction2, nodes = c("Smoker", "CAD"), states = c("Yes", "Yes"))
querygrain(jsmoker_cad2)

#Q3.1
dag2 <- empty.graph(nodes = c("BirthAsphyxia", "Disease", "Age", "LVH", "DuctFlow", "CardiacMixing", "LungParench",
                              "LungFlow", "Sick", "HypDistrib", "HypoxiaInO2", "CO2", "ChestXray", "Grunting", "LVHreport", 
                              "LowerBodyO2", "RUQO2", "CO2Report", "XrayReport", "GruntingReport" ))
arc.set <- matrix(c("BirthAsphyxia", "Disease", 
                    "Disease", "Age", 
                    "Disease", "LVH",
                    "Disease", "DuctFlow",
                    "Disease", "CardiacMixing",
                    "Disease", "LungParench",
                    "Disease", "LungFlow",
                    "Disease", "Sick",
                    "LVH", "LVHreport",
                    "DuctFlow", "HypDistrib",
                    "CardiacMixing", "HypDistrib",
                    "CardiacMixing", "HypoxiaInO2",
                    "LungParench", "HypoxiaInO2",
                    "LungParench", "CO2",
                    "LungParench", "ChestXray",
                    "LungParench", "Grunting",
                    "LungFlow", "ChestXray",
                    "Sick", "Age",
                    "Sick", "Grunting",
                    "HypDistrib", "LowerBodyO2",
                    "HypoxiaInO2", "LowerBodyO2",
                    "HypoxiaInO2", "RUQO2",
                    "CO2", "CO2Report",
                    "ChestXray", "XrayReport",
                    "Grunting", "GruntingReport"), byrow = TRUE, 
                  ncol = 2, dimnames = list(NULL, c("from", "to")))
arc.set
arcs(dag2) <- arc.set
plot(dag2)

#Q3.1
modelstring(dag2)#joint distribution written in compact factored form
#Q3.2
survey <- read.csv("child_network.csv", header = TRUE)
head(survey)
dim(survey)
#Q3.2
# Bayesian Fit (without a prior)
bn.child <- bn.fit(dag2, data = survey, method = "bayes")
bn.child
#?bn.fit

#Q3.3
bn.child$BirthAsphyxia

#Q3.4
bn.child$LowerBodyO2

#Q3.5
junction_child <- compile(as.grain(bn.child))
jLBO2_Xrayreport <- setEvidence(junction_child, nodes = c("LowerBodyO2", "XrayReport"), states = c("<5", "Plethoric"))
querygrain(jLBO2_Xrayreport, nodes = "Disease")$Disease



graphviz.chart(bn.child, grid = TRUE, main = "Original BN")
graphviz.chart(as.bn.fit(jLBO2_Xrayreport, including.evidence = TRUE), grid = TRUE,
               bar.col = c(BirthAsphyxia = "black", Disease = "black", Age = "grey", LVH = "black", DuctFlow = "grey", CardiacMixing = "black", LungParench = "black", LungFlow = "black", Sick = "black", HypDistrib = "black", HypoxiaInO2 = "grey", CO2 = "black", ChestXray = "black", Grunting = "black", LVHreport = "black", LowerBodyO2 = "black", RUQO2 = "black", CO2Report = "black", XrayReport = "black", GruntingReport = "grey"),
               strip.bg = c(BirthAsphyxia = "transparent", Disease = "transparent", Age = "grey", LVH = "transparent", DuctFlow = "grey", CardiacMixing = "transparent", LungParench = "transparent", LungFlow = "transparent", Sick = "transparent", HypDistrib = "transparent", HypoxiaInO2 = "grey", CO2 = "transparent", ChestXray = "transparent", Grunting = "transparent", LVHreport = "transparent", LowerBodyO2 = "transparent", RUQO2 = "transparent", CO2Report = "transparent", XrayReport = "transparent", GruntingReport = "grey"),
               main = "BN with Evidence")

3.6
junction_child_1 <- compile(as.grain(bn.child))
jLBO2_Xrayreport_notgrunting <- setEvidence(junction_child_1, nodes = c("LowerBodyO2", "XrayReport","Grunting"),
                                                       states = c("<5", "Oligaemic","no"))
querygrain(jLBO2_Xrayreport_notgrunting, nodes = "Disease")$Disease
graphviz.chart(as.bn.fit(jLBO2_Xrayreport_notgrunting, including.evidence = TRUE), grid = TRUE,
               bar.col = c(BirthAsphyxia = "black", Disease = "black", Age = "grey", LVH = "black", DuctFlow = "grey", CardiacMixing = "black", LungParench = "black", LungFlow = "black", Sick = "black", HypDistrib = "black", HypoxiaInO2 = "grey", CO2 = "black", ChestXray = "black", Grunting = "black", LVHreport = "black", LowerBodyO2 = "black", RUQO2 = "black", CO2Report = "black", XrayReport = "black", GruntingReport = "grey"),
               strip.bg = c(BirthAsphyxia = "transparent", Disease = "red", Age = "grey", LVH = "transparent", DuctFlow = "grey", CardiacMixing = "transparent", LungParench = "transparent", LungFlow = "transparent", Sick = "transparent", HypDistrib = "transparent", HypoxiaInO2 = "grey", CO2 = "transparent", ChestXray = "transparent", Grunting = "transparent", LVHreport = "transparent", LowerBodyO2 = "transparent", RUQO2 = "transparent", CO2Report = "transparent", XrayReport = "transparent", GruntingReport = "grey"),
               main = "BN with Evidence")


3.7
junction_child_2 <- compile(as.grain(bn.child))
jgrunting_mildcardiacmixing <- setEvidence(junction_child_2, nodes = c("Grunting","CardiacMixing"), states = c("yes", "Mild"))
querygrain(jgrunting_mildcardiacmixing, nodes = "Disease")$Disease

junction_child_3 <- compile(as.grain(bn.child))
jnotgrunting_completecardiacmixing <- setEvidence(junction_child_3, nodes = c("Grunting","CardiacMixing"), states = c("no", "Complete"))
querygrain(jnotgrunting_completecardiacmixing, nodes = "Disease")$Disease
