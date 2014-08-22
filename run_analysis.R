zipFile <- "UCI HAR Dataset.zip"
dataDir <- "./UCI HAR Dataset"

#checks if the zip data exists otherwise execution is stopped
if(!file.exists(zipFile)) {
    print(zipFile + "doesn't exists")
    return
}

#unzip data file
unzip(zipFile)
  
#1-Merges the training and the test sets to create one data set.

#helper function
getFilePath <- function (dataDir="./UCI HAR Dataset", type="train", prefix="X") {
    fileName <- paste(prefix, paste(type, ".txt", sep=""), sep="_")
    paste(dataDir, type, fileName,  sep="/")
}

#get train data
trainData <- read.table(getFilePath(prefix="X"))
subjectId <- read.table(getFilePath(prefix="subject"), col.names="subjectId")
activityId <-  read.table(getFilePath(prefix="y"), col.names="activityId")
trainData <- cbind(trainData, subjectId, activityId)
 
#get test data
testData <- read.table(getFilePath(type="test",prefix="X"))
subjectId <- read.table(getFilePath(type="test", prefix="subject"), col.names="subjectId")
activityId <-  read.table(getFilePath(type="test", prefix="y"), col.names="activityId")
testData <- cbind(testData, subjectId, activityId)

#append sets
allData <- rbind(trainData, testData)


#2-Extracts only the measurements on the mean and standard deviation for each measurement.

#get columns info
columnInfoFile <- paste(dataDir, "features.txt", sep="/")
columnInfo <- read.table(columnInfoFile)

#filter by mean or std column variables
filterColIndex <- grep("mean()|std()", columnInfo$V2);
allDataTrimmed <- allData[, filterColIndex]

#add subjecptId, activityId and type columns
allDataTrimmed <- cbind(allDataTrimmed, allData[,c("subjectId", "activityId")])


#3-Uses descriptive activity names to name the activities in the data set

#get activity labels
activityLabelFile <- paste(dataDir, "activity_labels.txt", sep="/")
activityLabel <- read.table(activityLabelFile, col.names=c("activityId","activityName"))

#add activity labels to data set
allDataTrimmed <- merge(allDataTrimmed, activityLabel, by="activityId", all.x=T)


#4-Appropriately labels the data set with descriptive variable names. 

#change activityId position to last column because merge set it first in order
allDataTrimmed <- allDataTrimmed[,c(2:(length(allDataTrimmed)),1)]

measureColumns <- as.character(columnInfo[filterColIndex,2])
nonFeatureColumns <- c("subjectId", "activityName","activityId")
newColNames <- c(measureColumns, nonFeatureColumns)
colnames(allDataTrimmed) <- newColNames


#5-Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

library(reshape2)
#melt data setting subject (type + subjectId) and activityName as dimensions and as measures the accelerometer values
dataMelt <- melt(allDataTrimmed, id=c("subjectId", "activityName"), measure.vars=measureColumns)

#Please note that the subject is identified by type group ("test" or "train") and the subject id
tidyData <- dcast(dataMelt, subjectId + activityName ~ variable, mean)

#writes tidy data to tidyData.txt
write.table(tidyData, file="tidyData.txt", row.name=F)





