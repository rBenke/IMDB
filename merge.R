library(data.table)
library(tidyverse)
library(stringi)

plots <- fread("plots3.csv")
names <- fread("genres2.csv")


names[,Year := as.integer(str_extract(Year, "[1-2][0-9]{3}"))]
#plots[,Year := as.integer(str_extract(Year, "[1-2][0-9]{3}"))]
str(names)
str(plots)

plots[,dots := str_count(plot, "\\.|\\?|!"),by = No]
names <- names[,':='(V1 = NULL, lp = NULL)]
plots <- plots[,':='(V1 = NULL, lp = NULL, pos = NULL, first = NULL)]

sum(plots$dots>9) #83370

plots<- plots[order(name, dots),head(.SD, 1),by = name]

sum(plots$dots>9) #41251
plots$name %>% unique() %>% length() #312994
dim(plots) #312994
plots[,nameL := nchar(name), by= No]
#plots[,.(name,nameL)]
sum(plots$nameL<3,na.rm = T) 257
namePlots <- plots$name %>% unique() #312 994
data <- names[Name %in% namePlots,]  #1 002 687
# o te 41 tysiecy mozna powalczyc
data$Name %>% unique() %>% length()  #271 229

names[,':='(name = Name, year = Year)]
names[,':='(Name = NULL, Year = NULL)]

keycols = c("name","year")
setkeyv(names,keycols)
setkeyv(plots,keycols)
DTmerge <- plots[names] # okolo 1k straty bo roznia sie latami
DT_final<-DTmerge[!is.na(plot),.(name,year,plot,genres,No,dots)]
DT_final<-DT_final[nchar(name)>2,]

write.csv(DT_final,file ="DT_final.csv")

