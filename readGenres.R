library(data.table)
library(tidyverse)
library(stringi)
genres <- read.csv("C:\\Users\\robert\\Desktop\\R\\IMDB\\genres.csv",sep = "\t")
#genres <- head(genres,100000) 

genresDT <- genres %>% as.data.table()
genresDT[,lp := seq.int(nrow(genresDT))]

genresDT[,gen1 := as.character(stri_extract_last_words(type2))]
genresDT[,gen1 := as.character(ifelse(is.na(gen1),"a",gen1)),by = lp]

genresDT[,gen2 := as.character(stri_extract_last_words(TYpe))]
genresDT[,gen2 := as.character(ifelse(is.na(gen2),as.character("a"),gen2)),by = lp]

genresDT[,gen3 := stri_extract_last_words(Coment)]
genresDT[,gen3 := ifelse(is.na(gen3),"a",gen3),by = lp]
str(genresDT)

genresDT[,genres := as.character(ifelse(gen1!="a",gen1,NA)),by = lp]
#genresDT[,genres := as.character(ifelse(length(gen1)>1,gen1,NA)),by = lp]#nie dziala, dlaczego?
genresDT[is.na(genresDT$genres),genres :=as.character(ifelse(gen2!="a",gen2,NA)),by = lp]
genresDT[is.na(genresDT$genres),genres := as.character(ifelse(gen3!="a",gen3,NA)),by = lp]

genresDT[,':='(gen1=NULL, gen2=NULL,gen3=NULL,Coment=NULL, TYpe=NULL, type2=NULL )]
gen <- genresDT$genres %>% unique() %>% as.vector()
gen <- gen[1:27]
genresDT <- genresDT[genres %in% gen,]
write.csv(genresDT,file = "genres2.csv")
