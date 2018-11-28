library(data.table)
library(tidyverse)
library(stringi)
plots <- fread("C:\\Users\\robert\\Desktop\\R\\IMDB\\plot.list",sep = "\n")
plots <- plots[1:100000,]
wek <- plots$`-------------------------------------------------------------------------------` 

name <- ""
text <- ""
data <-data.table("d","d")

names(data) <-c("name","plot")

for(i in 1:length(wek)){
  row <- wek[i]
  first <- stri_extract_first_words(row)
  first <- ifelse(is.na(first),"isNA",first)
  if (first=="MV") {
    name <- row
  } else if(first=="PL"){
    text = paste(text," ",row)
  }
  else if(name!="" & text!=""){
    data <- rbind(data,list(name = name,plot = text))
    text <- ""
    name <- ""
  }
  
}

data %>% head
