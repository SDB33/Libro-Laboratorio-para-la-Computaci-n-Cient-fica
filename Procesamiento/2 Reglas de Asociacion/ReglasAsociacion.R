library(arules)
library(dplyr)
library(magrittr)
library(fcaR)

#Reglas que sean un poco mejor que tirar una moneda
reglas <- apriori(Emple01, parameter = list(supp = 0.1, conf = 0.5,   
                                            target = "rules")) 
length(reglas)


reglas <- reglas[which(is.significant(reglas))]
reglas <- reglas[which(!is.redundant(reglas))]

length(reglas)
inspect(reglas)

fc_Empleo <- FormalContext$new(I = Emple01)

fc_Empleo$implications$add(reglas)

fc_Empleo$implications$cardinality()


fc_Empleo$implications$apply_rules(rules = c("composition",
                                      "generalization",
                                      "simplification",
                                      "rsimplification"),
                            parallelize = FALSE)


reglas <- fc_Empleo$implications$to_arules()


inspect(reglas)
