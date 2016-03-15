#Reading Supporting Metadata
featureNames<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/features.txt")
activityLabels<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/activity_labels.txt", header=FALSE)

#Reading Training Data
subjectTrain<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/train/subject_train.txt", header=FALSE)
activityTrain<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/train/y_train.txt", header=FALSE)
featuresTrain<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/train/x_train.txt", header=FALSE)

#Reading Test Data
subjectTest<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/test/subject_test.txt", header=FALSE)
activityTest<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/test/y_test.txt", header=FALSE)
featuresTest<-read.table("C:/Users/Carrae/Documents/R_Data_Science/UCI HAR Dataset/test/x_test.txt", header=FALSE)

#Merge training and test sets into one data set
subject<-rbind(subjectTrain, subjectTest)
activity<-rbind(activityTrain, activityTest)
features<-rbind(featuresTrain, featuresTest)

#Name columns
colnames(features)<-t(featureNames[2])

#Merge Data
colnames(activity)<-"Activity"
colnames(subject)<-"Subject"
AllData<-cbind(features, activity, subject)

#Extract only Measurements Needed for the Mean and Std
columnswithMeanSTD<-grep(".*Mean.*|.*Std.*", names(AllData), ignore.case=TRUE)
requiredColumns<-c(columnswithMeanSTD, 562,563)
dim(AllData)
extractedData<-AllData[, requiredColumns]
dim(extractedData)

#Descriptive Activity Names
extractedData$Activity<-as.character(extractedData$Activity)
   for (i in 1:6) { 
    extractedData$Activity[extractedData$Activity==i]<-as.character(activityLabels[i,2])
    }
extractedData$Activity<-as.factor(extractedData$Activity)

#Change Acronymns
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean ()", "Mean", names(extractedData),ignore.case = TRUE)
names(extractedData)<-gsub("-std ()", "STD", names(extractedData),ignore.case = TRUE)
names(extractedData)<-gsub("-freq ()", "Freq", names(extractedData),ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#Create Tidy Data Set
extractedData$Subject<-as.factor(extractedData$Subject)
extractedData<-data.table(extractedData)
tidyData<-aggregate(. ~Subject + Activity, extractedData, mean)
tidayData<-tidyData[order(tidyData$Subject, tidyData$Activity),]
write.table(tidyData, file="C:/Users/Carrae/Documents/R_Data_Science/TidyData.txt", 
row.names=FALSE)
