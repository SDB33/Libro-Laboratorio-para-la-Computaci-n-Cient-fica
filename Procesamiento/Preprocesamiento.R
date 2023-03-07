library(dplyr)
library(magrittr)
library(ggplot2)
library(rmarkdown)
library(tidyr)

#Metemos los datos en una variable auxiliar
Empleo  <-  Survey.on.Employment.Trends..2

#Borramos los Na ya que son solo 3
Empleo <- drop_na(Empleo)

#Borramos la columna "Timestamp" y "X" porque no nos da ningún tipo
#de información
Empleo <- Empleo %>%
  select(-c(Timestamp,X))

#Vamos a acortar el nombre de varias columnas 
Empleo <- Empleo %>%
           rename("Still.Studying" = Are.you.still.studying.,
                  "Studies"        = Educational.Qualification,
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
                  "Interested.in"           = Interested.area.of.work
                 )

#Duplicamos las tablas que nos interesan para guardar los valores originales
#a la par que trabajar con los agrupados
Empleo <- Empleo %>%
  mutate("Job.search.problems.grouped" = Job.search.problems,
         "Dream.company.type.grouped"  = Dream.company.type.,
         "Skills.grouped"              = Skills,
         "Studies.grouped.by.level"    = Studies,
         "Studies.grouped.by.field"    = Studies,
         "Interested.in.grouped"       = Interested.in
  )

#Cambiamos el orden para que cada columna esté al lado de su agrupada
Empleo <- Empleo %>%
  select(Name, Age.group, Gender, Where.are.you.from.,
         Marital.status, Work.experience, Studies,Studies.grouped.by.level,
         Studies.grouped.by.field,
         Still.Studying, Is.degree.must.4.job, Interested.in,
         Interested.in.grouped, Currently.employed, Career.break,
         Career.switch, Career.shift,Applying.4.job, Skills,
         Skills.grouped, Job.preference, Personal.projects,
         Job.finder, Recruiter.response.you,
         Interested.in.freelance, Dream.company.type.,
         Dream.company.type.grouped, Preference.of.work, 
         Job.search.problems, Job.search.problems.grouped
  )




#Vamos a discretizar la variable Work.Experience

##0-2 <- Fresher
Empleo[Empleo == "1-2years" & !is.na(Empleo)]<- "Fresher"
Empleo[Empleo == "Doing Intership" & !is.na(Empleo)]<- "Fresher"

##3-4 <- Junior
Empleo[Empleo == "3+ years" & !is.na(Empleo)]<- "Junior"

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
unique(unlist(Empleo$Job.search.problems.grouped))

Auxi <- c("No problems ","No problems",
          "Still not looking for jobs actively",
          "I haven't applied for any. ","Nothing ",
          " interviewer not handsome ", "Good oppurtunities")

for (x in Auxi) {
  Empleo$Job.search.problems.grouped <- gsub(x,"Nothing",
                                             Empleo$Job.search.problems.grouped)
}

Auxi <- c("Lack of experience","Fear of English"," network slow at home")

for (x in Auxi) {
  Empleo$Job.search.problems.grouped <- gsub(x,"My thing",
                                             Empleo$Job.search.problems.grouped)
}


unique(unlist(Empleo$Dream.company.type.grouped))

Auxi <- c("Google","Multinationals ","multinational companies",
          "Apple","Amazon","MNC")

for (x in Auxi) {
  Empleo$Dream.company.type.grouped <- gsub(x,"Multinational",
                                             Empleo$Dream.company.type.grouped)
}

Auxi <- c("Both","Both ","All types ")

for (x in Auxi) {
  Empleo$Dream.company.type.grouped <- gsub(x,"Both",
                                            Empleo$Dream.company.type.grouped)
}

Auxi <- c("Product based","Interior design ")

for (x in Auxi) {
  Empleo$Dream.company.type.grouped <- gsub(x,"Specific",
                                            Empleo$Dream.company.type.grouped)
}

unique(unlist(Empleo$Job.finder))

Auxi <- c("Other ")

for (x in Auxi) {
  Empleo$Job.finder <- gsub(x,"Other",Empleo$Job.finder)
}

unique(unlist(Empleo$Studies))



Auxi <- c("BSC physics","Bsc Physics ","Bsc physics ")

for (x in Auxi) {
  Empleo$Studies <- gsub(x,"Bachelor of Science in Physics",
                                 Empleo$Studies)
}

Auxi <- c("M.tech","Mtech","M. tech")

for (x in Auxi) {
  Empleo$Studies <- gsub(x,"Master of Technology",
                                 Empleo$Studies)
}

Auxi <- c("BVOC IN SOFTWARE DEVELOPMENT ","bvoc in software development",
          "Bvoc in software development","B.voc Software development ",
          "BVOC SOFTWARE DEVELOPMENT ","BVOC in software development ",
          "BVOC IT IN SOFTWARE DEVELOPMENT ","Bvoc IT")

for (x in Auxi) {
  Empleo$Studies<- gsub(x,"Bachelor of Vocational Studies in Software Development",
                                 Empleo$Studies)
}

Auxi <- c("Msc","MSc")

for (x in Auxi) {
  Empleo$Studies <- gsub(x,"Master of Science",Empleo$Studies)
}

Auxi <- c("Diploma ","diploma","Diploma")

for (x in Auxi) {
  Empleo$Studies <- gsub(x,"Diploma",Empleo$Studies)
}

Empleo$Studies <- gsub("MA","Master of Art",Empleo$Studies)
Empleo$Studies <- gsub("ITI","Industrial training institute",Empleo$Studies)
Empleo$Studies <- gsub("M Eng","Master of Engineering", Empleo$Studies)
Empleo$Studies <- gsub("Bds","Bachelor of Dental Surgery",Empleo$Studies)
Empleo$Studies <- gsub("Pharm D","Doctor of Pharmacy",Empleo$Studies)
Empleo$Studies <- gsub("BE EEE","Bachelor of Electrical and Electronics Engineering",Empleo$Studies)
Empleo$Studies <- gsub("BE","Bachelor of Engineering",Empleo$Studies)
Empleo$Studies <- gsub("Bsc","Bachelor of Science",Empleo$Studies)
Empleo$Studies <- gsub("MBA","Master of Business Administration",Empleo$Studies)
Empleo$Studies <- gsub("Btech","Bachelor of Technology",Empleo$Studies)
Empleo$Studies <- gsub("BCA","Bachelor of Computer Applications",Empleo$Studies)
Empleo$Studies <- gsub("MSW","Master of Social work",Empleo$Studies)
Empleo$Studies <- gsub("MCA","Master in Computer Application",Empleo$Studies)
Empleo$Studies <- gsub("Bcom","Bachelor of Commerce",Empleo$Studies)
Empleo$Studies <- gsub("BBA","Bachelor of Business Administration",Empleo$Studies)
Empleo$Studies <- gsub("Msc.physics","Master of Science in Physics",Empleo$Studies)


unique(unlist(Empleo$Studies))


Empleo$Studies.grouped.by.level[grep("Bachelor", Empleo$Studies)] <- "Bachelor"
Empleo$Studies.grouped.by.level[grep("Master", Empleo$Studies)] <- "Master"
Empleo$Studies.grouped.by.level[grep("Diploma", Empleo$Studies)] <- "Diploma"
Empleo$Studies.grouped.by.level[grep("Doctor", Empleo$Studies)] <- "Doctor"

Empleo$Studies.grouped.by.level[!(Empleo$Studies.grouped.by.level=="Bachelor" |
                                  Empleo$Studies.grouped.by.level=="Master" |
                                  Empleo$Studies.grouped.by.level=="Diploma" |
                            Empleo$Studies.grouped.by.level=="Doctor")]<- "Other"

Empleo$Studies.grouped.by.field[grep("Diploma", Empleo$Studies)] <- "Other"

Empleo$Studies.grouped.by.field[grep("Computer", Empleo$Studies)] <- "Computer"
Empleo$Studies.grouped.by.field[grep("Software", Empleo$Studies)] <- "Computer"

Empleo$Studies.grouped.by.field[grep("Science", Empleo$Studies)] <- "Science"
Empleo$Studies.grouped.by.field[grep("Technology", Empleo$Studies)] <- "Technology"
Empleo$Studies.grouped.by.field[grep("Engineering", Empleo$Studies)] <- "Engineering"

Empleo$Studies.grouped.by.field[grep("Dental", Empleo$Studies)] <- "Medical"
Empleo$Studies.grouped.by.field[grep("Pharmacy", Empleo$Studies)] <- "Medical"

Empleo$Studies.grouped.by.field[grep("Commerce", Empleo$Studies)] <- "Commerce"
Empleo$Studies.grouped.by.field[grep("Business", Empleo$Studies)] <- "Business"

Empleo$Studies.grouped.by.field[grep("Art", Empleo$Studies)] <- "Art"
Empleo$Studies.grouped.by.field[grep("Interior designing", Empleo$Studies)] <- "Art"

Empleo$Studies.grouped.by.field[grep("Social work", Empleo$Studies)] <- "Other"
Empleo$Studies.grouped.by.field[grep("Industrial training institute", Empleo$Studies)] <- "Other"


Empleo$Studies.grouped.by.field[Empleo$Studies==
    "Diplomain multimedia & bba (distance education third year)"] <- "Art;Business"


unique(unlist(Empleo$Interested.in.grouped))

Empleo$Interested.in.grouped[grep("Python", Empleo$Interested.in.grouped)] <- "Python"

Auxi <- c("Medical","Medicine","Nursing")

for (x in Auxi) {
  Empleo$Interested.in.grouped <- gsub(x,"Medical",Empleo$Interested.in.grouped)
}

Auxi <- c("Accounts and Art Field","Admin","Aerospace engineering ",
          "Construction field","Embedded programming ","Engineering Design ",
          "Govt services","HR","Interior Designing","IT","Life science",
          "Love without work","Mechanical Design","Mechanical Engineering ",
          "QA- testing ","Interior designing 'autocad designing","Other ")

for (x in Auxi) {
  Empleo$Interested.in.grouped <- gsub(x,"Other",Empleo$Interested.in.grouped)
}



unique(unlist(Empleo$Skills.grouped))

Auxi <- c("Wordpress","Css"," html","web designing ","html"," css","Rivet",
          " bootstrap","Angular","front-end development","Dotnet")

for (x in Auxi) {
  Empleo$Skills.grouped <- gsub(x,"WebPages",Empleo$Skills.grouped)
}

Auxi <- c("cad","Autocad ","Design","CREO Design Software","autocad",
          "CAD/CAM","AutoDesign ","autoDesign")

for (x in Auxi) {
  Empleo$Skills.grouped <- gsub(x,"Design",Empleo$Skills.grouped)
}

Auxi <- c(" MATLAB","Tableau ","Power BI","Powerbi")

for (x in Auxi) {
  Empleo$Skills.grouped <- gsub(x,"Data Analysis",Empleo$Skills.grouped)
}

Auxi <- c("Nursing","Driving "," editing "," electronics","Digital marketing",
          "Tally prime ")

for (x in Auxi) {
  Empleo$Skills.grouped <- gsub(x,"Other",Empleo$Skills.grouped)
}

Auxi <- c("Aspen","HTRI")

for (x in Auxi) {
  Empleo$Skills.grouped <- gsub(x,"Simulator",Empleo$Skills.grouped)
}

Auxi <- c("Excel","Sql","Database")

for (x in Auxi) {
  Empleo$Skills.grouped <- gsub(x,"Database",Empleo$Skills.grouped)
}

Auxi <- c("Python","C","c++"," embedded systems"," embedded c")

for (x in Auxi) {
  Empleo$Skills.grouped <- gsub(x,"Programming Languages",Empleo$Skills.grouped,fixed=TRUE)
}



##Vamos a convertir las columnas que tienen variables con un string con varias
#opciones por un vector
Empleo <- Empleo %>%
  mutate(Job.search.problems = strsplit(Empleo$Job.search.problems, ";|,"),
         Job.search.problems.grouped = strsplit(Empleo$Job.search.problems.grouped, ";|,"),
         Job.finder          = strsplit(Empleo$Job.finder , ";|,"),
         Job.preference      = strsplit(Empleo$Job.preference , ";|,"),
         Skills              = strsplit(Empleo$Skills , ";|,"),
         Skills.grouped              = strsplit(Empleo$Skills.grouped , ";|,"),
         Dream.company.type. = strsplit(Empleo$Dream.company.type., ";|,"),
         Dream.company.type.grouped = strsplit(Empleo$ Dream.company.type.grouped, ";|,"),
         Studies.grouped.by.field   = strsplit(Empleo$Studies.grouped.by.field , ";|,"),
         Interested.in              = strsplit(Empleo$Interested.in , '/'),
         Interested.in.grouped      = strsplit(Empleo$Interested.in.grouped , '/')
        )



unique(unlist(Empleo$Job.search.problems.grouped))





