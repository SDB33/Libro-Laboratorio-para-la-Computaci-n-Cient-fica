library(cluster)
library(factoextra)


Emple01 <- Emple01 %>% mutate_all(as.numeric)

#¿Cuántos clusters?

fviz_nbclust(x = Emple01, 
             FUNcluster = pam, 
             method = "wss",
             diss = dist(Emple01, 
                         method = "manhattan"),
             k.max = 10)


kmedias <- kmeans(Emple01, centers=1, nstart=2,iter.max = 300)

fviz_cluster(kmedias,Emple01, geom = "point")

#Vemos que es un grupo muy homogéneo
fviz_cluster(kmedias,Emple01)


#Haider Abdullah, Shafeeque, Mary Rathna y Athulya Kp son

grep("Haider", rownames(Emple01))


triti <- Emple01 %>% 
  slice(32) %>%
  select(which(Emple01 %>% slice(32) == 1))

#¿Qué hace que Haider Abdullah sea un outlier?


auxi <- table(unlist(Empleo$Studies.grouped.by.field))
auxi <- auxi[order(auxi,decreasing = TRUE)]

ggplot(as.data.frame(auxi), aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  labs(fill = "Buscadores", x = " ", y = " ")

grep("Business",unlist(Empleo$Studies.grouped.by.field))

auxi <- unlist(Empleo$Studies.grouped.by.field)

length(grep("Business",auxi))/length(auxi)
length(grep("Art",auxi))/length(auxi)



#Sus estudios están entre los menos cursados



auxi <- unlist(Empleo$Skills)

length(grep("Other",auxi))/length(auxi)
length(grep("Design",auxi))/length(auxi)


auxi <- table( unlist(Empleo$Skills))
auxi <- auxi[order(auxi,decreasing = TRUE)]

ggplot(as.data.frame(auxi), aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  labs(fill = "Buscadores", x = " ", y = " ")
