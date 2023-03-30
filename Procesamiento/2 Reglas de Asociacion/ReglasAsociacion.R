library(arules)
library(dplyr)
library(magrittr)

reglas <- apriori(Emple01, parameter = list(supp = 0.5, conf = 0.9,   
                                            target = "rules")) 








