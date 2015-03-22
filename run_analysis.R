#We'll use this function later to translate activity integers 
#to the activity names
Activitytransform <- function(activityinteger) {
    if(activityinteger==1) "WALKING"
    else if(activityinteger==2) "WALKING_UPSTAIRS"
    else if(activityinteger==3) "WALKING_DOWNSTAIRS"
    else if(activityinteger==4) "SITTING"
    else if(activityinteger==5) "STANDING"
    else if(activityinteger==6) "LAYING"
}

library(plyr)
library(reshape2)
#Read in the files
subject.train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x.train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject.test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x.test <- read.table("./UCI HAR Dataset/test/x_test.txt")
y.test <- read.table("./UCI HAR Dataset/test/y_test.txt")

#Merge them together - train/test into one table each
train <- cbind(subject.train,y.train,x.train)
test <- cbind(subject.test,y.test,x.test)

#Get the 561 feature names
names <- read.table("./UCI HAR Dataset/features.txt")
namevec <- as.character(names[,2])

#Apply namevec to the train and test data
names(train) <- c("Subject","Activity",namevec)
names(test) <- c("Subject","Activity",namevec)

#Make one dataframe with train and test on top of each other
data <- rbind(train, test)

#Make a new dataframe with only the subject, activity, and mean/std columns.  
#Was going to use grep() but there was a thread on the forums suggesting the explicit column number would make more sense realistically.
datasmaller <- data[,c(1,2,3,4,5,43,44,45,83,84,85,123,124,125,163,164,165,203,216,229,242,255,268,269,270,347,348,349,426,427,428,505,518,531,544,6,7,8,46,47,48,86,87,88,126,127,128,166,167,168,204,217,230,243,256,271,272,273,350,351,352,429,430,431,506,519,532,545)]

#Name activities using the activitytransform function from above
datasmaller$Activity <- sapply(datasmaller$Activity, Activitytransform)

#Melt it
datamelt <- melt(datasmaller,id.vars=c("Subject","Activity"))

#Cast it - Subject + Activity on the left puts them together, then mean is applied for each pair and each variable
finaldata <- dcast(datamelt,Subject + Activity ~ variable,mean)

#Write it
write.table(finaldata,file="finaldata.txt",row.names=FALSE)