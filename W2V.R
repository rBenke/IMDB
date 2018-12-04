# proba klasyfikacji: Comedy/ Drama
library(tidyverse)
library(data.table)
library(stringr)
library(qdap)
library(RTextTools)
data<- fread("DT_final2.csv")
data<- data[,head(.SD,1),by = name]
#Wyodrebnienie danych z kategorii Drama i Comedy
data <- data[str_detect(genres,"Drama")==TRUE|str_detect(genres,"Comedy")==TRUE,][,
              ':='(isDrama = str_detect(genres,"Drama"),isComedy = str_detect(genres,"Comedy") )]

data[isDrama == T& isComedy == F,] %>% dim() 
data[isDrama == F& isComedy == T,] %>% dim()
data[isDrama == T& isComedy == T,] %>% dim()

data[isDrama == T& isComedy == F& dots>5,] %>% dim()
data[isDrama == F& isComedy == T& dots>5,] %>% dim()
data[isDrama == T& isComedy == T& dots>5,] %>% dim()


data$V1 %>% unique() %>%  length()
data$V1 %>%  length() # tyle samo co wyzej => V1 jest unique

# usuwanie liczb, znakow specjalnych i stop words 
data <- data[,plot := tolower(plot)]

stopwords_regex = paste(Top25Words, collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
data <- data[,plot := stringr::str_replace_all(plot, stopwords_regex, '')]

data <- data[,plot := gsub("[#,':(){}\"$@<>`]","",plot,perl = T)]
data <- data[,plot := gsub('[0-9]',"",plot,perl = T)]
data <- data[,plot := gsub('[.]'," . ",plot,perl = T)]
data <- data[,plot := gsub('[!]'," ! ",plot,perl = T)]
data <- data[,plot := gsub('[?]'," ? ",plot,perl = T)]

#zamiana slow do podstawowej formy
data <- data[,plot := plot %>% strsplit(" ") %>% 
               unlist() %>%  wordStem() %>% paste(collapse = " "), by = V1]

# test & train sample
idx_Drama <- data[isDrama == T& isComedy == F& dots>5,.(V1)] %>% 
  as.vector() %>% dplyr::sample_n(3000)
idx_Comedy <- data[isDrama == F& isComedy == T& dots>5,.(V1)] %>%
  as.vector() %>% dplyr::sample_n(3000)

test <- data[V1 %in% idx_Drama$V1|V1 %in% idx_Comedy$V1,]
train <- data[!(V1 %in% test$V1), ] 

aa <- train$plot %>% unlist() %>% paste(collapse = " ")
write.csv(aa,file = "training.csv")
write.csv(test,file = "testDT.csv")
write.csv(train,file = "trainDT.csv")
# word to vector
#1) mozna liczyc tylko dystans?? x.x
#library(tmcn.word2vec)
#word2vec("training.csv","TMCNoutput.csv")
#2)
library(text2vec)


#4)
library(fastTextR)
#3)
#devtools::install_github("bmschmidt/wordVectors")
library(wordVectors)
library(magrittr)
#train_word2vec("training.csv","wordVecoutput.bin",threads = 4)
model = read.vectors("wordVecoutput.bin")
model %>% closest_to("women")
some_fish = closest_to(model,model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],150)
fishy = model[[some_fish$word,average=F]]
model %>% closest_to("fasdhjfs")
model %>% closest_to(~"good"+"bad")
model %>% closest_to(~"good"-"bad")
ls("package:wordVectors")
aaaa =model[[c("lady","woman","man","he","she","guy","man"), average=T]]
aaaa %>%  as.vector()

# stworzenie zbioru do klasyfikacji
learningDT <- train[!(isDrama == T& isComedy == T) & dots>5,]
learningDT <- learningDT[V1 %in% sample(learningDT$V1,10000),]

data <-data.table("d","d","d","d")
names(data) <-c("eigen1","eigen2","eigen3","isDrama")

library(foreach)
library(doSNOW)
cl<-makeCluster(4) #change the 2 to your number of CPU cores
registerDoSNOW(cl)
foreach(i = 1:nrow(learningDT)) %do% {
  row = learningDT[i,]
  eigen = tryCatch({plot2eigen(row$plot)}, finally = {
    data <- rbind(data,list(eigen1=eigen[1],eigen2 = eigen[2],
                            eigen3 = eigen[3],isDrama =row$isDrama ))
  }) # END tryCatch)
 
}
data <- data[2:nrow(data)]
data <- data %>% unique() 



plot2eigen <- function(plot){
  sentences <- strsplit(plot, "[!?.]") %>% unlist()
  sentences <- lapply(sentences,function(a) {ifelse(nchar(a)>5,a,NA)})
  sentences <- sentences[!is.na(sentences)] %>%  unlist()
  sentencesVectors <- lapply(sentences, function(senten){
    tryCatch({
    splited <- senten %>% strsplit(" ") %>% unlist() 
    transformed <- lapply(splited,function(trans) model[[trans]])
    if(anyNA(transformed)==FALSE){
    dt <- as.data.frame(data.table::transpose(transformed))
    colnames(dt) <- paste0('Col', 1:100)
    means <- sapply(dt, mean, na.rm = T)
    means
    }
    })
    })
  points <- as.data.frame(data.table::transpose(sentencesVectors))
  colnames(points) <- paste0('Col', 1:100)
  pca <- prcomp(points, center = F,scale. = F)
  c(pca$sdev[1],pca$sdev[2],pca$sdev[3])
}


#model 
data2 <- data[,':='(eigen1 = as.numeric(eigen1),eigen2 = as.numeric(eigen2),eigen3 = as.numeric(eigen3))]
summary(data[isDrama==T])
summary(data[isDrama==F])

library(rpart)
model <- ?rpart(isDrama~.,data2)
