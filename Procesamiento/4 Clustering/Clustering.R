library(cluster)
library(factoextra)
library(dendextend)   

Emple01 <- Emple01 %>% mutate_all(as.numeric)

#¿Cuántos clusters?

fviz_nbclust(x = Emple01, 
             FUNcluster = pam, 
             method = "wss",
             diss = dist(Emple01, 
                         method = "manhattan"),
             k.max = 10)


kmedias <- kmeans(Emple01, centers=1, nstart=1,iter.max = 300)

fviz_cluster(kmedias,Emple01, geom = "point")

#Vemos que es un grupo muy homogéneo
fviz_cluster(kmedias,Emple01)

Emple01 %>% 
  dist(method = "euclidean") %>%
  fviz_dist()


#Haider Abdullah, Shafeeque, Mary Rathna y Athulya Kp son outliers



auxi2 <- 0
for (x in c(1:length(rownames(Emple01)))) {
  
 auxi <-  (Emple01 %>%
    select(which(Emple01 %>% slice(x) == 1)) %>%
    sapply(sum))/length(rownames(Emple01))

  auxi2 <- c(auxi2,mean(auxi))
}




auxi <-  as.data.frame(list(
  Nombres = rownames(Emple01),
  Medias = auxi2[-1]
))

auxi %>%
  arrange(Medias) %>%
  slice(1:5)


#Los quitamos

Emple01 <-  Emple01 %>%
  slice(-c(grep("Haider Abdullah", rownames(Emple01)),
           grep("Shafeeque", rownames(Emple01)),
           grep("Mary Rathna K M", rownames(Emple01)),
           grep("Athulya kp", rownames(Emple01))))


#Dendogramas


#Representación del dendrograma
fviz_dend(x = hclust(d = dist(x = Emple01, method = "euclidean"),
                     method = "complete"), k = 10)



