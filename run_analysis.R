run_analysis <- function () {
        ##- Step 1 ---- Merge the training and test data
        ##   Download file and unzip
        if (!file.exists("data")) {
                dir.create("data")
        }
        filezip <- "./dataset.zip"
        if (!file.exists(filezip)) {
                url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                
                download.file(url, filezip)
                fname = unzip(filezip, list=TRUE)$Name
                unzip(zipfile = filezip, files= fname, exdir="./data", overwrite =TRUE)
        }
        
        
        
        ## -Read features and activity descriptions
        features <- read.table (file="./data/UCI HAR Dataset/features.txt",header= FALSE, sep="")
        actlist <- read.table (file="./data/UCI HAR Dataset/activity_labels.txt",header= FALSE, sep="")
        
        names(actlist) <- c("activitycode","activitydescription")
        ## -Read test data set
        xtest <- read.table (file="./data/UCI HAR Dataset/test/x_test.txt",header= FALSE, sep="")
        ytest <- read.table (file="./data/UCI HAR Dataset/test/y_test.txt",header= FALSE, sep="")
        subjecttest <- read.table (file="./data/UCI HAR Dataset/test/subject_test.txt",header= FALSE, sep="")
        
        ## -Read train data set
        xtrain <- read.table (file="./data/UCI HAR Dataset/train/x_train.txt",header= FALSE, sep="")
        ytrain <- read.table (file="./data/UCI HAR Dataset/train/y_train.txt",header= FALSE, sep="")
        subjecttrain <- read.table (file="./data/UCI HAR Dataset/train/subject_train.txt",header= FALSE, sep="")
        
        
        
        # set feature column names 
        colnames(xtest) <- features[,2]
        colnames(xtrain) <- features[,2]
        ## Step 2 - Extract only mean() and std() columns  
        ## select columns names containng mean() and std()
        selectcolumns  = c(grep("mean()",colnames(xtest), fixed= TRUE), grep("std()",colnames(xtest), fixed= TRUE))
        
        ## grep("mean()",colnames(xtest), fixed= TRUE)
        ## there are 33 mean() and 33 std(0 named columns)
        testresult <- xtest[, selectcolumns]
        trainresult <- xtrain[, selectcolumns]
        ## add subjectnumber and activtycode columns
        testresult <- cbind(ytest, testresult)
        trainresult <- cbind(ytrain,trainresult)
        names(testresult)[1] <- "activitycode"
        names(trainresult)[1] <- "activitycode"
        
        testresult <- cbind(subjecttest, testresult )
        trainresult <- cbind(subjecttrain, trainresult )
        names(testresult)[1] <- "subjectnumber"
        names(trainresult)[1] <- "subjectnumber"
        
        
        ## combine two sets vertically
        f1 <- rbind(testresult, trainresult)
        
        ## -- merge completes with only mean and standard deviation measures
        ## and subjectnumber and activitycode columns added
        
        ## Order data set by subjectnumber and activitycode
        f2 <- f1[order(f1$subjectnumber,f1$activitycode),]
        
        ## Step 3 - Add activity description
        ## library(plyr)
        f2 <- merge(f2,actlist, by.x="activitycode", by.y="activitycode", all = TRUE)
        
        ## set descriptive names
        c1 <- names(f2)
        c1 <- gsub("?mean?\\(\\)","Mean",c1)
        c1 <- gsub("?std?\\(\\)","STD",c1)
        c1 <- gsub("tBody","TimeBody",c1)
        c1 <- gsub("tGravity","TimeGravity",c1)
        c1 <- gsub("fBody","FFTBody",c1)
        c1 <- gsub("fGravity","FFTGravity",c1)
        c1 <- gsub("Acc","Acceleration",c1)
        c1 <- gsub("Mag","Magnitude",c1)
        c1 <- gsub("\\-","",c1)
        names(f2) <- c1
        ## create final tidy data frame with mead of Mean() and std() measures by subject and activit type
        
        
        tidydataset <- ddply (f2, c ("subjectnumber","activitydescription"),numcolwise(mean))
        ## Finally write the tidy data set to a file
        
        write.table(tidydataset, file="tidydataset.txt", row.names = FALSE)
        
} 
