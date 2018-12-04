#--------------------------------------------
# reading genres.csv (orginal) and save to data.table:
#  Name, Year, lp, genres - genres2.csv, n_obs = 2.657.655
#--------------------------------------------
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
genresDT[is.na(genresDT$genres),genres :=as.character(ifelse(gen2!="a",gen2,NA)),by = lp]
genresDT[is.na(genresDT$genres),genres := as.character(ifelse(gen3!="a",gen3,NA)),by = lp]

genresDT[,':='(gen1=NULL, gen2=NULL,gen3=NULL,Coment=NULL, TYpe=NULL, type2=NULL )]
gen <- genresDT$genres %>% unique() %>% as.vector()
gen <- gen[1:27]
genresDT <- genresDT[genres %in% gen,]
write.csv(genresDT,file = "genres2.csv")


#--------------------------------------------
# reading plot.list (orginal) and save to data.table:
#  Name, year, No, genres - plots3.csv, n_obs = 596.428
#--------------------------------------------
library(data.table)
library(tidyverse)
library(stringi)
plots <- fread("C:\\Users\\robert\\Desktop\\R\\IMDB\\plot.list",sep = "\n")

wek <- plots$`-------------------------------------------------------------------------------` 

name <- ""
text <- ""
data <-data.table("d","d","d")
j=1
ileMV=0
names(data) <-c("name","plot","No")

for(i in 1:length(wek)){
  row <- wek[i]
  first <- stri_extract_first_words(row)
  first <- ifelse(is.na(first),"isNA",first)
  if (first=="MV") {
    name <- row
    ileMV = ileMV +1
  } else if(first=="PL"){
    loc=str_locate(row, "PL")[2]
    text = paste(text,"",str_sub(row,start = loc+2, end = -1))
  }
  else if(name!="" & text!=""){
    data <- rbind(data,list(name = name,plot = text,No=j))
    text <- ""
    name <- ""
    j=j+1
  }
  
}

data[2:nrow(data),]
data <- data[,year := str_extract(name, "[1-2][0-9]{3}")]
data2 <- data[,`:=`(pos = str_locate(name, year)[1],first = str_locate(name, "MV")[2]),
              by = No][,name := str_sub(name,start = first+3, end = pos-3),by = No]
write.csv(data,file ="plots2.csv")

#-------------------------------------
#exporation: 

#data2[name %like% "Avatar", .(name,year)]
#dim(data)

#data$year
#data$plot %>% head(5)
#data$plot %>% as.vector() %>% head()

#data$name
#dim(data)
#-------------------------------------

#--------------------------------------------
# merge plot2.csv & genres2.csv into one table
#--------------------------------------------

library(data.table)
library(tidyverse)
library(stringi)

plots <- fread("plots2.csv")
names <- fread("genres2.csv")

names[,Year := as.integer(str_extract(Year, "[1-2][0-9]{3}"))]

plots[,dots := str_count(plot, "\\.|\\?|!"),by = No]
names <- names[,':='(V1 = NULL, lp = NULL)]
plots <- plots[,':='(V1 = NULL, lp = NULL, pos = NULL, first = NULL)]

plots<- plots[order(name, dots),head(.SD, 1),by = name] #choose the longest plot for movie

names[,':='(name = Name, year = Year)] #change the name of columns
names[,':='(Name = NULL, Year = NULL)]

keycols = c("name","year")
setkeyv(names,keycols)
setkeyv(plots,keycols)
DTmerge <- plots[names]
DT_final<-DTmerge[!is.na(plot),.(name,year,plot,genres,No,dots)]
DT_final<-DT_final[nchar(name)>2,]

write.csv(DT_final,file ="DT_final.csv")

#--------------------------------------------
# EDA of DT_final, consolidate genres by title
#--------------------------------------------

library(data.table)
library(tidyverse)

data <- fread("DT_final.csv")
data[,':='(V1=NULL,No=NULL)]
data <- data %>% unique() 

str(data)
summary(data)
anyNA(data)


# number of observations in genres
ggplot(data, aes(genres)) + geom_histogram(stat = "count")
ggplot(data[dots>9], aes(genres)) + geom_histogram(stat = "count")

# production year 
ggplot(data[dots>9], aes(year)) + geom_histogram(stat = "count")

# number of categories for every movie 
ggplot(data[,.N,by = name], aes(N)) + geom_histogram(stat = "bin")
data[,.N,by = name] %>% summary()

# number of dots in plot
data[,.(dots)] %>% unlist() %>% boxplot()
data[,.(dots)] %>% unlist() %>% summary()

# consolidate movies by title (all genres gum together)
data <- data[, .(genres = paste(genres, collapse = " "),dots,year,plot), by = name]
data[,.(name, genres)]

write.csv(data,file ="DT_final2.csv")

