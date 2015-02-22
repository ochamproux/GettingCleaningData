
run_analysis <- function(){
        
        dataset <- NULL
        
        testX <- read.table("test//X_test.txt")
        trainX <- read.table("train//X_train.txt")
        
        testY <- read.table("test//y_test.txt")
        trainY <- read.table("train//y_train.txt")
        colnames(testY) <- "ActivityCode"
        colnames(trainY) <- "ActivityCode"
        
        testSubjects <- read.table("test//subject_test.txt")
        trainSubjects <- read.table("train//subject_train.txt")
        colnames(testSubjects) <- "Subject"
        colnames(trainSubjects) <- "Subject"
  
        
        ## 1. Merges the training and the test sets to create one data set.
        testData <- cbind(testX,testY,testSubjects)
        trainData <- cbind(trainX, trainY, trainSubjects)
        
        if(ncol(testData)==ncol(trainData)){
                dataset <- rbind(testData,trainData)
                
        }
        else{
                print("test and train dataset don't have same ncol()")
                return(dataset)
        }
        
        ## 2. Extracts only the measurements on the mean and standard deviation 
        ## for each measurement.
        features <- read.table("features.txt")
        ##filter columns from their descriptive name in "features"
        colFilter <- filter(features, 
                            grepl("mean()",V2, fixed=T) | grepl("std()",V2, fixed=T))
        colFilter <- as.vector(colFilter[,1])
        ## keep also activityCode and subject cols
        lastcol <- ncol(dataset)
        colFilterFull <- c(colFilter,lastcol-1,lastcol)
        
        dataset <- dataset[,colFilterFull]
                

        ## 3. Uses descriptive activity names to name the activities in the data set
        al <- read.table("activity_labels.txt")
        dataset$ActivityName <- al[dataset$ActivityCode,2]
        
        
        ## 4. Appropriately labels the data set with descriptive variable names
        
        ## let's take names from the filtered column index used in step 2.
        newcolnames <- as.vector(features[colFilter,2])
        colnames <- colnames(dataset)
        ## let's rename only the "first" data columns
        for(i in 1:length(newcolnames)){
                colnames[i] <- newcolnames[i]
        }
        colnames(dataset) <- colnames
     
        
        ## 5. From the data set in step 4, creates a second, independent tidy 
        ## data set with the average of each variable for each activity and each subject
    
        ## step 5 still not done ...
        
        
        dataset
        
        
}