# Merges the training and the test sets to create one data set.
loadAndMerge <- function() {
    message("Loading data")  
    dfTrain <- read.table("train/X_train.txt", header=F, sep="", nrows=nrows)
    dfTest <- read.table("test/X_test.txt", header=F, sep="", nrows=nrows)
    rbind(dfTrain, dfTest)
}

# Extracts only the measurements on the mean and standard deviation for each measurement. 
extractColumns <- function(df) {
    message("Exracting the mean and std columns")  
    dfColumns <- read.table("features.txt", header=F, sep="")
    meanCols <- grepl("-mean\\(", dfColumns[,2])
    stdCols <- grepl("-std\\(", dfColumns[,2])
    colFilter <- meanCols | stdCols
    
    message("Columns to extract: ", sum(colFilter))
    colIndex <- dfColumns[colFilter, 1]
    result <- df[, colIndex]
    
    colNames <- dfColumns[colFilter, 2]
    colNames <- gsub("[()]", "", colNames)

    message("Adding column names")
    colnames(result) <- colNames
    result
}

# Uses descriptive activity names to name the activities in the data set
addSubject <- function(df) {
    message("Adding subject")
    subject <- 
        rbind(
            read.table("train/subject_train.txt", header=F, sep="", nrows=nrows),
            read.table("test/subject_test.txt", header=F, sep="", nrows=nrows)
        )
    colnames(subject) <- c("subject")
    cbind(subject, df)
}

addActivities <- function(df) {
    message("Adding Activities")
    activity <- 
        rbind(
            read.table("train/y_train.txt", header=F, sep="", nrows=nrows),
            read.table("test/y_test.txt", header=F, sep="", nrows=nrows)
        )
    
    activityNames <- read.table("activity_labels.txt", header=F, sep="", nrows=nrows)
    
    labeledActivity <- merge(activity, activityNames)[,2]
    result <- cbind(labeledActivity, df)
    colnames(result)[1] <- "activity"
    result
}


# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
createAverageDataFrame <-function(df) {
    message("creating average dataset")
    result <- aggregate(df[, 3:ncol(df)], by=list(df$activity, df$subject), FUN=mean)
    colnames(result)[1] <- "activity"
    colnames(result)[2] <- "subject"
    result
}

# Appropriately labels the data set with descriptive variable names. 
meltAndLabelVariables <- function(adf) {
    d <- melt(adf, id=c("activity", "subject"))
    message("Setting sensor")
    d$sensor <- sapply(d$variable, getSensor, USE.NAMES=F)
    message("Setting domain")
    d$component  <- sapply(d$variable, getDomain, USE.NAMES=F)
    message("Setting var")
    d$domain  <- sapply(d$variable, getDomain, USE.NAMES=F)
    d$var <- sapply(d$variable, getVar, USE.NAMES=F)
    message("Setting var component")
    d$component  <- sapply(d$variable, getVarComponent, USE.NAMES=F)
    message("Setting aggregation type")
    d$aggregation_type  <- sapply(d$variable, getAggregation, USE.NAMES=F)
    result <- data.frame(d[, c("subject", "activity", "sensor", "component", "domain", "var", "aggregation_type", "value")])
}

getSensor <- function(x) {
    if (grepl("Acc", x)) {
        result <- "Accelerometer"
    }  else if (grepl("Gyro", x)) {
        result <- "Gyroscope"
    } else {
        stop("Column must have Acc or Gyro")
    }
    result
}


getAggregation <- function(x) {
    if (grepl("-mean", x)) {
        result <- "mean"
    } else if (grepl("-std", x)) {
        result <-"std"
    }  else {
        stop("Column name must have -mean or -std")
    }
    result
}


getDomain <- function(x) {
    if (grepl("^t", x)) {
        result <- "Time"
    } else if (grepl("^f", x)) {
        result <- "Frequency"
    }  else {
        stop("Column name must start with t or f")
    }
}

getVar <- function(x) {
    result <- ""
    vars <- list("JerkMag", "Jerk", "Mag")
    for (v in vars) {
        if (grepl(v, x)) {
            result <-v
            break
        }
    }
    vars <- list("-X", "-Y", "-Z")
    for (v in vars) {
        if (grepl(v, x)) {
            y <- gsub("-", "", v)
            if (nchar(result) > 0) {
                result <- paste(result, y, sep="_")
            } else {
                result <- y
            }
            break
        }
    }
    
    result
}

getVarComponent <- function(x) {
    result <- ""
    if (grepl("Body", x)) {
        result <- "Body"
    } else if (grepl("Gravity", x)) {
        result <- "Gravity"
    }  else {
        stop("Column name must have Body or Gravity")
    }
    result
}


writeDataSet <- function(adf, fileName) {
    message("Writing dataset")
    write.table(adf, fileName, sep=",", row.names=F, col.names=T, quote=F)
}


#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------

#This variable limits the number of rows to process from input files. If set to -1 it will process all rows
nrows <- -1

library("reshape2")

df <- loadAndMerge()
df <- extractColumns(df)
df <- addSubject(df)
df <- addActivities(df)
adf <- createAverageDataFrame(df)
result <- meltAndLabelVariables(adf)
writeDataSet(result, "result.txt")

