library(dplyr)
library(magrittr)
library(fcaR)


fc_Empleo <- FormalContext$new(Emple01)

fc_Empleo$plot()

fc_Empleo$clarify(TRUE)
fc_Empleo$reduce(TRUE)

#Tarda un poco
fc_Empleo$find_concepts()


fc_Empleo$concepts[which(fc_Empleo$concepts$support()>0.5)]





fc_Empleo$find_implications()





fc_Empleo$implications$recommend()


