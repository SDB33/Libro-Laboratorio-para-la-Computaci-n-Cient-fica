library(dplyr)
library(magrittr)
library(fcaR)


correlationMatrix <- cor(Emple01)
print(correlationMatrix)
relacionado <- findCorrelation(correlationMatrix, cutoff=0.5)
print(relacionado)
Emple01[-relacionado]


fc_Empleo <- FormalContext$new(Emple01[-relacionado])

fc_Empleo$plot()

fc_Empleo$clarify(TRUE)
fc_Empleo$reduce(TRUE)

#Tarda un poco
fc_Empleo$find_concepts()


fc_Empleo$concepts[which(fc_Empleo$concepts$support()>0.5)]


fc_Empleo$find_implications()



fc_Empleo$implications[which(fc_Empleo$implications$support()>0.2)]



