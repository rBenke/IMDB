library(data.table)
library(tidyverse)
library(stringi)
plots <- fread("C:\\Users\\robert\\Desktop\\R\\IMDB\\plot.list",sep = "\n")
#plots <- plots[1:1000,]
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


data2[name %like% "Avatar", .(name,year)]
#dim(data)
data[,year := str_extract(name, "[1-2][0-9]{3}")]
#data$year
#data$plot %>% head(5)
#data$plot %>% as.vector() %>% head()
data2 <- data[,`:=`(pos = str_locate(name, year)[1],first = str_locate(name, "MV")[2]),
               by = No][,name := str_sub(name,start = first+3, end = pos-3),by = No]
#data <- data[,`:=`(pos = str_locate(name, "\(")[1]),
 #              by = No][,name := str_sub(name,start = 0, end = pos-1),by = No]

#data$name
#dim(data)
write.csv(data,file ="plots3.csv")

