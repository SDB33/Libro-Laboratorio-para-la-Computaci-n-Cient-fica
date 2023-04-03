library(dplyr)
library(magrittr)
library(fcaR)


fc_Empleo <- FormalContext$new(Emple01)

fc_Empleo$plot()

fc_Empleo$clarify(TRUE)
fc_Empleo$reduce(TRUE)

#Tarda un poco
fc_Empleo$find_concepts()


fc_Empleo$concepts$support()[order(fc_Empleo$concepts$support(),decreasing = TRUE)]


fc_Empleo$concepts[which(fc_Empleo$concepts$support()>0.5)]



