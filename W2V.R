#--------------------------------------------
# Problem !: classify movie: Drama/Comedy
#--------------------------------------------

library(tidyverse)
library(data.table)
library(stringr)
library(qdap)
library(RTextTools)
MinNum <- 9
data<- fread("DT_final2.csv")
data<- data[,head(.SD,1),by = name]
#choosing only Drama and Comedy
data <- data[str_detect(genres,"Drama")==TRUE|str_detect(genres,"Comedy")==TRUE,][,
              ':='(isDrama = str_detect(genres,"Drama"),isComedy = str_detect(genres,"Comedy") )]

data[isDrama == T& isComedy == F,] %>% dim() 
data[isDrama == F& isComedy == T,] %>% dim()
data[isDrama == T& isComedy == T,] %>% dim()

data[isDrama == T& isComedy == F& dots>MinNum,] %>% dim()
data[isDrama == F& isComedy == T& dots>MinNum,] %>% dim()

# deleting numbers, special signs and stopwords 
data <- data[,plot := tolower(plot)]

stopwords_regex = paste(Top100Words, collapse = '\\b|\\b')
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
idx_Drama <- data[isDrama == T& isComedy == F& dots>MinNum,.(V1)] %>% 
  as.vector() %>% dplyr::sample_n(1000)
idx_Comedy <- data[isDrama == F& isComedy == T& dots>MinNum,.(V1)] %>%
  as.vector() %>% dplyr::sample_n(1000)

test <- data[V1 %in% idx_Drama$V1|V1 %in% idx_Comedy$V1,]
train <- data[!(V1 %in% test$V1), ] 

aa <- train$plot %>% unlist() %>% paste(collapse = " ")
write.csv(aa,file = "training.csv")
write.csv(test,file = "testDT.csv")
write.csv(train,file = "trainDT.csv")

library(wordVectors)
library(magrittr)
train_word2vec("training.csv","wordVecoutput2.bin",threads = 4,vectors = 300)
model = read.vectors("wordVecoutput2.bin")
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
learningDT <- train[!(isDrama == T& isComedy == T) & dots>MinNum,]
learningDT <- learningDT[V1 %in% sample(learningDT$V1,10000),]

data <-data.table("eigen1","eigen2","eigen3","eigen4","eigen5","eigen6",
                  "eigen7","eigen8","eigen9","eigen11","eigen10","eigen12",
                  "eigen13","eigen14","eigen15","eigen16","eigen17","eigen18",
                  "eigen19","MeanDistance","DistanceVariance","isDrama")
names(data) <-c("eigen1","eigen2","eigen3","eigen4","eigen5","eigen6",
                "eigen7","eigen8","eigen9","eigen11","eigen10","eigen12",
                "eigen13","eigen14","eigen15","eigen16","eigen17","eigen18",
                "eigen19","MeanDistance","DistanceVariance","isDrama")

library(foreach)
library(doSNOW)
cl<-makeCluster(4) #change the 2 to your number of CPU cores
registerDoSNOW(cl)
foreach(i = 1:nrow(learningDT)) %do% {
  row = learningDT[i,]
  eigen = plot2eigen(row$plot)
    data <- rbind(data,eigen)
}
data <- data[2:nrow(data)]
data <- data %>% unique() 



plot2eigen <- function(plot){
  plot = "after reach mutual new york sever financi servic compani robert j . smith becam total perman disabl result auto accid . unabl work smith interview reach industri . sport champion rock star busi other reach status interview . thank everi industri learn develop habit strategi reach top chosen profess . becom disabl chang career reason learn best yet ."
  senten = sentences[3]
  sentences[[1]][2] <-"w"
  
  sentences <- strsplit(plot, "[!?.]") #%>% unlist()
  sentences <- lapply(sentences[[1]],function(a) {ifelse(nchar(a)>5,a,NA)})
  sentences <- sentences[!is.na(sentences)] %>%  unlist()
  
  if(length(sentences)>MinNum){
  sentencesVectors <- lapply(sentences, function(senten){
    splited <- senten %>% strsplit(" ") 
    transformed <- lapply(splited[[1]],function(trans) model[[trans]])
    if(anyNA(transformed)==FALSE){
    dt <- as.data.frame(data.table::transpose(transformed))
    colnames(dt) <- paste0('Col', 1:300)
    means <- sapply(dt, mean, na.rm = T)
    means
    }
    })
  
  points <- as.data.table(data.table::transpose(sentencesVectors))
  points <- points[,':='(a=lapply(1:1,function(i) eval(parse(text = cat("Col",i,sep = "")))-eval(parse(text = cat("lag(Col",i,")",sep = "")))))]
  colnames(points) <- paste0('Col', 1:300)
  pca <- prcomp(points, center = F,scale. = F)
  c(pca$sdev[1],pca$sdev[2],pca$sdev[3])
  }
}


#model 
data2 <- data[,':='(eigen1 = as.numeric(eigen1),eigen2 = as.numeric(eigen2),eigen3 = as.numeric(eigen3))]
summary(data[isDrama==T])
summary(data[isDrama==F])

library(rpart)
model <- rpart(isDrama~.,data2)


# word to vector
#1) mozna liczyc tylko dystans?? x.x
#library(tmcn.word2vec)
#word2vec("training.csv","TMCNoutput.csv")
#2)
#library(text2vec)


#4)
#library(fastTextR)
#3)
#devtools::install_github("bmschmidt/wordVectors")