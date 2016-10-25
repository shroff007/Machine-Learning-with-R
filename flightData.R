getwd()
setwd('C:/Users/Shemal/Documents/Machine Learning - JKurata')
origData <- read.csv2('flight_data.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
airports <- c('ATL','LAX','ORD','DFW','JFK','SFO','CLT','LAS','PHX')
origData <- subset(origData, DEST %in% airports & ORIGIN %in% airports)
head(origData,2)
origData$UNIQUE_CARRIER <- NULL
cor(origData[c("ORIGIN_AIRPORT_SEQ_ID","ORIGIN_AIRPORT_ID")]) #Finding Correlation
mismatched <- origData[origData$CARRIER != origData$UNIQUE_CARRIER,]  #Filtering the rows where carrier and unique_carrier are different
nrow(mismatched)
onTimeData <- origData[!is.na(origData$ARR_DEL15) & origData$ARR_DEL15!="" & !is.na(origData$DEP_DEL15) & origData$DEP_DEL15!="",]
onTimeData$DIVERTED <- as.integer(onTimeData$DIVERTED)
onTimeData$CARRIER <- as.factor(onTimeData$CARRIER)   #can be changed back using as.numeric or as.string
tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15, length)
install.packages('caret')
library(caret)
set.seed(122515)
featureCols <- c("ARR_DEL15","DAY_OF_WEEK","CARRIER","DEST","ORIGIN","DEP_TIME_BLK")
onTimeDataFiltered <- onTimeData[,featureCols]
inTrainRows <- createDataPartition(onTimeDataFiltered$ARR_DEL15, p = 0.70, list = FALSE)
head(inTrainRows,10)
trainDataFiltered <- onTimeDataFiltered[inTrainRows,]
testDataFiltered <- onTimeDataFiltered[-inTrainRows,]
nrow(trainDataFiltered) / (nrow(trainDataFiltered) + nrow(testDataFiltered))
nrow(testDataFiltered) / (nrow(trainDataFiltered) + nrow(testDataFiltered))
install.packages('e1071', dependencies=TRUE)
library(e1071)
logisticRegModel <- train(ARR_DEL15 ~ ., data = trainDataFiltered, method = "glm", family = "binomial")
logisticRegModel
logRegPrediction <- predict(logisticRegModel, testDataFiltered)
logRegConfMat <- confusionMatrix(logRegPrediction, testDataFiltered[,"ARR_DEL15"])
logRegConfMat
