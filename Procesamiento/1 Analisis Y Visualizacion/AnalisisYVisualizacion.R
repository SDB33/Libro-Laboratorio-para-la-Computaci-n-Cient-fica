library(dplyr)
library(magrittr)
library(ggplot2)
library(rmarkdown)
library(tidyr)


#¿Cuáles son las páginas más usadas para la búsqueda de empleo?

auxi <- table(unlist(Empleo$Job.finder))
auxi <- auxi[order(auxi,decreasing = TRUE)]

ggplot(as.data.frame(auxi), aes(x=Var1, y = Freq)) + geom_bar(stat="identity")

ggplot(as.data.frame(auxi), aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#¿Hay diferencias entre lo que estudian los hombres y las mujeres?

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
for (x in c(1:nrow(auxi))) {
  auxi[nrow(auxi) + 1,] <- auxi %>% slice(x)
}

auxi <- auxi %>%
  mutate(Sexo = if_else(row_number()<=7,"Hombre", "Mujer"))

for (x in c(((nrow(auxi)/2)+1):nrow(auxi)  )   ) {
  print(x)
  auxi$Hombres[x] <- auxi$Mujeres[ (x - (nrow(auxi)/2) )    ]
}

auxi <- auxi %>%
  select(-3) %>%
  rename( "Contar" = Hombres)

#No hay mucha disparidad entre elecciones de campo de estudios por género
#hay que decir también que estos  datos son "de juguete" pueden estar más que
#perfectamente sesgados o simplemente ser tan pequeños que no muestran nada


ggplot(auxi, aes(x=unlist(auxi$Studies.grouped.by.field), y=Contar, fill = Sexo )) +
  geom_bar( stat='identity',position='dodge')




####Marital Status and interested in freelance

auxi <- Empleo %>% 
  select(Marital.status, Interested.in.freelance) %>%
  group_by(Marital.status, Interested.in.freelance) %>%
  summarise( numero = n()) 


auxi[[3]][which(auxi$Marital.status == "Married")] = 
  (auxi[[3]][which(auxi$Marital.status == "Married")])/sum(auxi[[3]][which(auxi$Marital.status == "Married")])


auxi[[3]][which(auxi$Marital.status == "Single")] = 
  (auxi[[3]][which(auxi$Marital.status == "Single")])/sum(auxi[[3]][which(auxi$Marital.status == "Single")])

auxi <- auxi[-4,]


ggplot(auxi, aes(x = unlist(Marital.status), y = numero, colour = Interested.in.freelance)) + 
   geom_point()

#Tampoco hay diferencias muy reseñables aunque sí que es cierto que, ligeramente,
#hay mayor porcentaje de personas casadas que están seguras de que no quieren
 #ser freelance y, por el contrario, hay más personas solteras que están pensando
 #si hacerlo.



#Y entre la edad y ser freelance?

auxi <- Empleo %>% 
  select(Age.group, Interested.in.freelance) %>%
  group_by(Age.group, Interested.in.freelance) %>%
  summarise( numero = n()) 


auxi[[3]][which(auxi$Age.group == "<25")] = 
  (auxi[[3]][which(auxi$Age.group == "<25")])/sum(auxi[[3]][which(auxi$Age.group == "<25")])


auxi[[3]][which(auxi$Age.group == "25-30")] = 
  (auxi[[3]][which(auxi$Age.group == "25-30")])/sum(auxi[[3]][which(auxi$Age.group == "25-30")])

auxi[[3]][which(auxi$Age.group == "31-40")] = 
  (auxi[[3]][which(auxi$Age.group == "31-40")])/sum(auxi[[3]][which(auxi$Age.group == "31-40")])



ggplot(auxi, aes(x = unlist(Age.group), y = numero, colour = Interested.in.freelance)) + 
  geom_point()

#Aquí ya sí podemos notar cierta separación que coincide más o menos con los datos esperados
#los más jóvenes son los más proclives a ser freelances (diferencia entre "Yes" y "No" mayor)
#Los de 25-30 años tienen tanto el mísmo interés en serlo como en no serlo y los de 31-40
#sorprendentemente tienen el porcentaje más alto de personas que quieren ser freelancers con casi un
# 60%
#reitero que todo esto considerando que son datos de juguete




##


