library(arules)
library(dplyr)
library(magrittr)
library(fcaR)

#Reglas que sean un poco mejor que tirar una moneda

#Soporte = Frecuencia de la regla en el dataset
#Confianza = Indica cuán fiable es una regla
reglas <- apriori(Emple01, parameter = list(supp = 0.1, conf = 0.5,   
                                            target = "rules")) 


reglas <- reglas[which(is.significant(reglas))]
reglas <- reglas[which(!is.redundant(reglas))]



fc_Empleo <- FormalContext$new(Emple01)

fc_Empleo$implications$add(reglas)

fc_Empleo$implications$apply_rules(rules = c("composition","generalization",
                                           "simplification","rsimplification"),
                                   parallelize = FALSE)


reglas <- fc_Empleo$implications$to_arules()


inspect(reglas)


#Despues de la limpieza nos quedan 11 reglas
