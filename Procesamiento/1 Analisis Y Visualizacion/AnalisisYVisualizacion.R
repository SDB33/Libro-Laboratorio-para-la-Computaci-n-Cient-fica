library(dplyr)
library(magrittr)
library(ggplot2)
library(rmarkdown)
library(tidyr)


#¿Cuáles son las páginas más usadas para la búsqueda de empleo?

table(unlist(Empleo$Job.finder))[order(table(unlist(Empleo$Job.finder)),decreasing = TRUE)]

#SuperJueves

auxi <- Empleo %>%
  select(Gender,Studies.grouped.by.field) %>%
  filter(Gender == "Male") %>%
  group_by(Studies.grouped.by.field) %>%
  summarise(Hombres=n()) %>%
  arrange(Hombres) 


auxi[[2]][which(auxi == "Art")] <- auxi[[2]][which(auxi == "Art")] +1 
auxi[[2]][which(auxi == "Business")] <- auxi[[2]][which(auxi == "Business")] +1 
auxi <- auxi[-1,]


auxi <- inner_join(auxi, (Empleo %>%
                    select(Gender,Studies.grouped.by.field) %>%
                    filter(Gender == "Female") %>%
                    group_by(Studies.grouped.by.field) %>%
                    summarise(Mujeres=n()) %>%
                    arrange(Mujeres))
                          , by = "Studies.grouped.by.field")


#No hay mucha disparidad entre elecciones de campo de estudios por género
#hay que decir también que estos  datos son "de juguete" pueden estar más que
#perfectamente sesgados o simplemente ser tan pequeños que no muestran nada

   



