library(dplyr)
library(magrittr)
library(ggplot2)
library(rmarkdown)
library(tidyr)

#Metemos los datos en una variable auxiliar
Empleo  <-  Survey

#Borramos los Na ya que son solo 3
Empleo <- drop_na(Empleo)

#Borramos la columna "Timestamp", "X" y "Nombre" porque no nos da ningún tipo
#de información
Empleo <- Empleo %>%
  select(-c(Timestamp,X,Name))

Empleo$Is.degree.must.4.job[Empleo$Is.degree.must.4.job == ""] <- "Maybe"

#Vamos a acortar el nombre de varias columnas 
Empleo <- Empleo %>%
           rename("Still.Studying" = Are.you.still.studying.,
                  "Is.degree.must.4.job" = Do.you.believe.degree.is.a.must.for.job.,
                  "Career.break"         = Did.you.have.a.career.break.,
                  "Career.switch"        = Did.you.switch.your.career.,
                  "Career.shift"         = Reason.for.shift.in.career.,
                  "Applying.4.job"       = Are.you.actively.applying.for.job.,
                  "Skills"               = Skills.that.you.are.confident,
                  "Personal.projects"    = Have.you.carried.out.any.independent.works.or.projects.,
                  "Job.finder"           = Where.do.you.search.for.Job.,
                  "Recruiter.response.you" = Are.you.receiving.response.from.recruiter.after.applying.,
                  "Job.search.problems"    = What.are.the.problems.faced.while.job.search.,
                  "Interested.in.freelance" = Are.you.interested.in.Freelancing.,
                  "Interested.in"           = Interested.area.of.work,
                  "Studies.grouped.by.level"    = Educational.Qualification,
                  "Dream.company.type"          = Dream.company.type.
                 )


#Duplicamos las tablas que nos interesan para guardar los valores originales
#a la par que trabajar con los agrupados
Empleo <- Empleo %>%
  mutate("Studies.grouped.by.field"    = Studies.grouped.by.level
  )

#Cambiamos el orden para que esrudios por nivel y campo estén juntos
Empleo <- Empleo %>%
        select(1:6, 24,7:23) 


#Vamos a discretizar la variable Work.Experience

##0-2 <- Fresher
Empleo[Empleo == "1-2years"]<- "Fresher"
Empleo[Empleo == "Doing Intership"]<- "Fresher"

##3-4 <- Junior
Empleo[Empleo == "3+ years"]<- "Junior"

##+5 <- Senior
#Considerando que no hay otro tipo de valores, todo lo que no sea Fresher
#o Junior, será Senior
Empleo$Work.experience[!(Empleo$Work.experience == "Fresher" | 
                           Empleo$Work.experience == "Junior") ] <- "Senior"


#Para poder trabajar mejor y sacar mejores resultados, 
#vamos a agrupar ciertos datos.


#Ahora agrupamos los datos mirando todos los valores diferentes que tiene
#cada columna y cambiándolos hasta que solo queden los mínimos representantes
#de cada clase

#Sacamos todos los datos diferentes
unique(unlist(Empleo$Job.search.problems))


for (x in c("No problems ","No problems",
            "Still not looking for jobs actively",
            "I haven't applied for any. ","Nothing ",
            " interviewer not handsome ", "Good oppurtunities")
    ) {
  Empleo$Job.search.problems <- gsub(x,"Nothing",
                                             Empleo$Job.search.problems)
}



for (x in c("Lack of experience","Fear of English"," network slow at home")) {
  Empleo$Job.search.problems <- gsub(x,"My thing",
                                             Empleo$Job.search.problems)
}


unique(unlist(Empleo$Dream.company.type))


for (x in c("Google","Multinationals ","multinational companies",
            "Apple","Amazon","MNC")) {
  Empleo$Dream.company.type <- gsub(x,"Multinational",
                                             Empleo$Dream.company.type)
}

for (x in c("Both","Both ","All types ")) {
  Empleo$Dream.company.type <- gsub(x,"Both",
                                            Empleo$Dream.company.type)
}

for (x in c("Product based","Interior design ")) {
  Empleo$Dream.company.type <- gsub(x,"Specific",
                                            Empleo$Dream.company.type)
}

unique(unlist(Empleo$Job.finder))

for (x in c("Other ")) {
  Empleo$Job.finder <- gsub(x,"Other",Empleo$Job.finder)
}


unique(unlist(Empleo$Studies.grouped.by.level))

for (x in c("BSC physics","Bsc Physics ","Bsc physics ")) {
  Empleo$Studies.grouped.by.level <- gsub(x,"Bachelor of Science in Physics",
                                 Empleo$Studies.grouped.by.level)
}

for (x in c("M.tech","Mtech","M. tech")) {
  Empleo$Studies.grouped.by.level <- gsub(x,"Master of Technology",
                                 Empleo$Studies.grouped.by.level)
}


for (x in c("BVOC IN SOFTWARE DEVELOPMENT ","bvoc in software development",
            "Bvoc in software development","B.voc Software development ",
            "BVOC SOFTWARE DEVELOPMENT ","BVOC in software development ",
            "BVOC IT IN SOFTWARE DEVELOPMENT ","Bvoc IT")) {
  Empleo$Studies.grouped.by.level<- gsub(x,"Bachelor of Vocational Studies in Software Development",
                                 Empleo$Studies.grouped.by.level)
}

for (x in c("Msc","MSc")) {
  Empleo$Studies.grouped.by.level <- gsub(x,"Master of Science",Empleo$Studies.grouped.by.level)
}

for (x in c("Diploma ","diploma","Diploma")) {
  Empleo$Studies.grouped.by.level <- gsub(x,"Diploma",Empleo$Studies.grouped.by.level)
}

Empleo$Studies.grouped.by.level <- gsub("MA","Master of Art",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("ITI","Industrial training institute",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("M Eng","Master of Engineering", Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("Bds","Bachelor of Dental Surgery",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("Pharm D","Doctor of Pharmacy",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("BE EEE","Bachelor of Electrical and Electronics Engineering",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("BE","Bachelor of Engineering",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("Bsc","Bachelor of Science",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("MBA","Master of Business Administration",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("Btech","Bachelor of Technology",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("BCA","Bachelor of Computer Applications",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("MSW","Master of Social work",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("MCA","Master in Computer Application",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("Bcom","Bachelor of Commerce",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("BBA","Bachelor of Business Administration",Empleo$Studies.grouped.by.level)
Empleo$Studies.grouped.by.level <- gsub("Msc.physics","Master of Science in Physics",Empleo$Studies.grouped.by.level)


unique(unlist(Empleo$Studies.grouped.by.level))

Empleo$Studies.grouped.by.field[grep("Diploma", Empleo$Studies.grouped.by.level)] <- "Other"

Empleo$Studies.grouped.by.field[grep("Computer", Empleo$Studies.grouped.by.level)] <- "Computer"
Empleo$Studies.grouped.by.field[grep("Software", Empleo$Studies.grouped.by.level)] <- "Computer"

Empleo$Studies.grouped.by.field[grep("Science", Empleo$Studies.grouped.by.level)] <- "Science"
Empleo$Studies.grouped.by.field[grep("Technology", Empleo$Studies.grouped.by.level)] <- "Technology"
Empleo$Studies.grouped.by.field[grep("Engineering", Empleo$Studies.grouped.by.level)] <- "Engineering"

Empleo$Studies.grouped.by.field[grep("Dental", Empleo$Studies.grouped.by.level)] <- "Medical"
Empleo$Studies.grouped.by.field[grep("Pharmacy", Empleo$Studies.grouped.by.level)] <- "Medical"

Empleo$Studies.grouped.by.field[grep("Commerce", Empleo$Studies.grouped.by.level)] <- "Commerce"
Empleo$Studies.grouped.by.field[grep("Business", Empleo$Studies.grouped.by.level)] <- "Business"

Empleo$Studies.grouped.by.field[grep("Art", Empleo$Studies.grouped.by.level)] <- "Art"
Empleo$Studies.grouped.by.field[grep("Interior designing", Empleo$Studies.grouped.by.level)] <- "Art"

Empleo$Studies.grouped.by.field[grep("Social work", Empleo$Studies.grouped.by.level)] <- "Other"
Empleo$Studies.grouped.by.field[grep("Industrial training institute", Empleo$Studies.grouped.by.level)] <- "Other"


Empleo$Studies.grouped.by.field[Empleo$Studies.grouped.by.level==
                                  "Diplomain multimedia & bba (distance education third year)"] <- "Art;Business"

Empleo$Studies.grouped.by.level[grep("Bachelor", Empleo$Studies.grouped.by.level)] <- "Bachelor"
Empleo$Studies.grouped.by.level[grep("Master", Empleo$Studies.grouped.by.level)] <- "Master"
Empleo$Studies.grouped.by.level[grep("Diploma", Empleo$Studies.grouped.by.level)] <- "Diploma"
Empleo$Studies.grouped.by.level[grep("Doctor", Empleo$Studies.grouped.by.level)] <- "Doctor"

Empleo$Studies.grouped.by.level[!(Empleo$Studies.grouped.by.level=="Bachelor" |
                                  Empleo$Studies.grouped.by.level=="Master" |
                                  Empleo$Studies.grouped.by.level=="Diploma" |
                            Empleo$Studies.grouped.by.level=="Doctor")]<- "Other"




unique(unlist(Empleo$Interested.in))

Empleo$Interested.in[grep("Python", Empleo$Interested.in)] <- "Python"

for (x in c("Medical","Medicine","Nursing")) {
  Empleo$Interested.in <- gsub(x,"Medical",Empleo$Interested.in)
}

for (x in c("Accounts and Art Field","Admin","Aerospace engineering ",
            "Construction field","Embedded programming ","Engineering Design ",
            "Govt services","HR","Interior Designing","IT","Life science",
            "Love without work","Mechanical Design","Mechanical Engineering ",
            "QA- testing ","Interior designing 'autocad designing","Other ")) {
  Empleo$Interested.in <- gsub(x,"Other",Empleo$Interested.in)
}


unique(unlist(Empleo$Skills))

for (x in c("Wordpress","Css"," html","web designing ","html"," css","Rivet",
            " bootstrap","Angular","front-end development","Dotnet")) {
  Empleo$Skills <- gsub(x,"WebPages",Empleo$Skills)
}

for (x in c("cad","Autocad ","Design","CREO Design Software","autocad",
            "CAD/CAM","AutoDesign ","autoDesign")) {
  Empleo$Skills <- gsub(x,"Design",Empleo$Skills)
}

for (x in c(" MATLAB","Tableau ","Power BI","Powerbi")) {
  Empleo$Skills <- gsub(x,"Data Analysis",Empleo$Skills)
}


for (x in c("Nursing","Driving "," editing "," electronics","Digital marketing",
            "Tally prime ")) {
  Empleo$Skills <- gsub(x,"Other",Empleo$Skills)
}

for (x in c("Aspen","HTRI")) {
  Empleo$Skills <- gsub(x,"Simulator",Empleo$Skills)
}

for (x in c("Excel","Sql","Database")) {
  Empleo$Skills <- gsub(x,"Database",Empleo$Skills)
}

for (x in c("Python","C","c++"," embedded systems"," embedded c")) {
  Empleo$Skills <- gsub(x,"Programming Languages",Empleo$Skills,fixed=TRUE)
}



##Vamos a convertir las columnas que tienen variables con un string con varias
#opciones por un vector
Empleo <- Empleo %>%
  mutate(Job.search.problems = strsplit(Empleo$Job.search.problems, ";|,"),
         Job.finder          = strsplit(Empleo$Job.finder , ";|,"),
         Job.preference      = strsplit(Empleo$Job.preference , ";|,"),
         Skills              = strsplit(Empleo$Skills , ";|,"),
         Dream.company.type = strsplit(Empleo$Dream.company.type, ";|,"),
         Studies.grouped.by.field   = strsplit(Empleo$Studies.grouped.by.field , ";|,"),
         Interested.in              = strsplit(Empleo$Interested.in , '/'),
        )
