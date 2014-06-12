run_analysis <- function()  {

# Read the list of all features and the activity names

  features<-read.table("features.txt",colClasses=c("integer","character"),col.names=c("fid","fname"))
  activity<-read.table("activity_labels.txt",colClasses=c("integer","character"),col.names=c("aid","aname"))

# Read the training data set

  X_train<-read.table("train/X_train.txt")
  y_train<-read.table("train/y_train.txt",col.names="activity")
  subject_train<-read.table("train/subject_train.txt",col.names="subject")

# Read the test data set

  X_test<-read.table("test/X_test.txt")
  y_test<-read.table("test/y_test.txt",col.names="activity")
  subject_test<-read.table("test/subject_test.txt",col.names="subject")

# Merge the training and the test sets
 
  X<-rbind(X_train,X_test)
  y<-rbind(y_train,y_test)
  subject<-rbind(subject_train,subject_test)

# Extract only the measurements on the mean and standard deviation

  msfeatures<-rbind(features[grep("mean",features[,2]),],features[grep("std",features[,2]),])
  Xms<-X[,msfeatures[,1]]
  names(Xms)<-msfeatures[,2]

# Name the activity in the data set

  ynamed<-sapply(y,function(y) activity[y,2])

# Create a tidy data set with average of each variable for each acitivy and each subject

  xys<-cbind(Xms,subject,ynamed)
  as<-split(xys,interaction(subject[,1],ynamed[,1]))
  tidy<-as.data.frame(sapply(as,function(x) colMeans(xys[,-c(80,81)])))

# Write a tidy data set to a file "tidy.txt"

  write.csv(tidy,"tidy.txt")

}