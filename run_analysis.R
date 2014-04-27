## Peer assessment for Getting and Cleaning Data.
##
## You should create one R script called run_analysis.R that does the 
## following: 
## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation 
##    for each measurement. 
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive activity names. 
## 5) Creates a second, independent tidy data set with the average of each 
##    variable for each activity and each subject. 

setwd('C:/Users/japadros/Documents/cuentas/coursera/Getting and Cleaning Data/peerAssessment/getdata-projectfiles-UCI HAR Dataset/');

getVar<-function(filename1, filename2, varName) {
  # Read datasets in, using a long col.names vector to accomodate 
  # varying measurement sizes
  ds1<-read.table(filename1,sep=' ',header=F,fill=T,
                  col.names=paste0('v',1:1000));
  ds2<-read.delim(filename2,sep=' ',header=F,fill=T,
                  col.names=paste0('v',1:1000));
  
  # Join both datasets
  ds<-rbind(ds1,ds2)
  
  # Calculate means and deviations per row
  ds.m<-rowMeans(ds,na.rm=T)
  ds.d<-apply(ds,1,sd,na.rm=T)
  
  res<-data.frame(ds.m, ds.d)
  colnames(res)<-paste0(varName,c(".mean",".sd"))
  return(res);
}

body_acc_x <- getVar('UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt','body_acc_x');
body_acc_y <- getVar('UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt','body_acc_y');
body_acc_z <- getVar('UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt','body_acc_z');
body_gyro_x <- getVar('UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt','body_gyro_x');
body_gyro_y <- getVar('UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt','body_gyro_y');
body_gyro_z <- getVar('UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt','body_gyro_z');
total_acc_x <- getVar('UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt','total_acc_x');
total_acc_y <- getVar('UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt','total_acc_y');
total_acc_z <- getVar('UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt',
                     'UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt','total_acc_z');
x           <- getVar('UCI HAR Dataset/train/X_train.txt',
                     'UCI HAR Dataset/test/X_test.txt','x');
subject     <- rbind(read.delim('UCI HAR Dataset/train/subject_train.txt',header=F),
                     read.delim('UCI HAR Dataset/test/subject_test.txt',header=F));
colnames(subject)<-'subject';
y           <- rbind(read.delim('UCI HAR Dataset/train/y_train.txt',header=F),
                     read.delim('UCI HAR Dataset/test/y_test.txt',header=F));
colnames(y)<-'y';
y$y<-factor(y$y);
levels(y$y)<-c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYING');

ds<-data.frame(subject,y,x,body_acc_x,body_acc_y,body_acc_z,body_gyro_x,body_gyro_y,body_gyro_z,total_acc_x,total_acc_y,total_acc_z);

ds.by<-data.frame(subject=rep(levels(factor(ds$subject)),each=length(levels(factor(ds$y)))),y=levels(factor(ds$y)))
ds.split<-t(simplify2array(by(ds[,-c(1,2)],ds[,c('subject','y')],colMeans)))
ds.summ<-data.frame(ds.by,ds.split)

write.csv(ds.summ,'ds.summ.csv')

