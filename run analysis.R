
test1<-read.table("test/subject_test.txt", header=FALSE, sep=" ")
test2<-read.table("test/X_test.txt", header=FALSE, sep=" ")
test3<-read.table("test/Y_test.txt", header=FALSE, sep=" ")
testgyrox<-read.table("test/Inertial Signals/body_gyro_x_test.txt", header=FALSE )
testgyroy<-read.table("test/Inertial Signals/body_gyro_y_test.txt", header=FALSE )
testgyroz<-read.table("test/Inertial Signals/body_gyro_z_test.txt", header=FALSE )
testtotalx<-read.table("test/Inertial Signals/total_acc_x_test.txt", header=FALSE )
testtotaly<-read.table("test/Inertial Signals/total_acc_y_test.txt", header=FALSE )
testtotalz<-read.table("test/Inertial Signals/total_acc_z_test.txt", header=FALSE )
testsub<-read.table("test/subject_test.txt", header=FALSE )
testsubX<-read.table("test/X_test.txt", header=FALSE )
testsubY<-read.table("test/Y_test.txt", header=FALSE )
trainY<-read.table("train/Y_train.txt", header=FALSE )
trainX<-read.table("train/X_train.txt", header=FALSE )
features<-read.table("features.txt", header=FALSE )

for (i in 1:561){
    colnames(trainX)[i]<-features[i,2]
}
colnames(trainX)[562]<-"subj"
colnames(trainX)[563]<-"label"
trainfeat[,1]=features[,1]
trainX$test_train<-"train"
testsubX$test_train<-"test"
for (i in 1:561){
    colnames(testsubX)[i]<-features[i,2]
}
colnames(trainX)[562]<-"subj"
colnames(trainX)[563]<-"val"
newdataset<-trainX


for (i in 1:2947) {
    newdataset[7352+i,]=testsubX[i,]
}

newdataset<-rbind(trainX,testsubX)

newdatasetfinal<-newdataset[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,294:296,345:350,373:375,424:429,452:454,503:504,513,516:517,526,529:530,539,542:543,552,555,557:561)]
newdatasetfinal$subj<-newdataset$subj
newdatasetfinal$label<-newdataset$val
newdatasetfinal$test_train<-newdataset$test_train
newdatasetfinal3<-newdatasetfinal

df_summary <- newdatasetfinal3 %>%
    group_by(subj, label) %>%
    summarise(
        Avg_Var1 = mean(`fBodyAcc-mean()-X`, na.rm = TRUE),
        Avg_Var2 = mean(`fBodyAcc-mean()-Y`, na.rm = TRUE),
        Avg_Var3 = mean(`fBodyAcc-mean()-Z`, na.rm = TRUE),
        Avg_Var4 = mean(`tBodyAcc-std()-X`, na.rm = TRUE),
        Avg_Var5 = mean(`tBodyAcc-std()-Y`, na.rm = TRUE),
        Avg_Var6 = mean(`tBodyAcc-std()-Z`, na.rm = TRUE),
        Avg_Var7 = mean(`tGravityAcc-mean()-X`, na.rm = TRUE),
        Avg_Var8 = mean(`tGravityAcc-mean()-Y`, na.rm = TRUE),
        Avg_Var9 = mean(`tGravityAcc-mean()-Z`, na.rm = TRUE),
        Avg_Var10 = mean(`tGravityAcc-std()-X`, na.rm = TRUE),
        Avg_Var11 = mean(`tGravityAcc-std()-Y`, na.rm = TRUE),
        Avg_Var12 = mean(`tGravityAcc-std()-Z`, na.rm = TRUE),
        Avg_Var13 = mean(`tBodyAccJerk-mean()-X`, na.rm = TRUE),
        Avg_Var14 = mean(`tBodyAccJerk-mean()-Y`, na.rm = TRUE),
        Avg_Var15 = mean(`tBodyAccJerk-mean()-Z`, na.rm = TRUE),
        Avg_Var16 = mean(`tBodyAccJerk-std()-X`, na.rm = TRUE),
        Avg_Var17 = mean(`tBodyAccJerk-std()-Y`, na.rm = TRUE),
        Avg_Var18 = mean(`tBodyAccJerk-std()-Z`, na.rm = TRUE),
        Avg_Var19 = mean(`tBodyGyro-mean()-X`, na.rm = TRUE),
        Avg_Var20 = mean(`tBodyGyro-mean()-Y`, na.rm = TRUE),
        Avg_Var21 = mean(`tBodyGyro-mean()-Z`, na.rm = TRUE),      
        Avg_Var22 = mean(`tBodyGyro-std()-X`, na.rm = TRUE),
        Avg_Var23 = mean(`tBodyGyro-std()-Y`, na.rm = TRUE),
        Avg_Var24 = mean(`tBodyGyro-std()-Z`, na.rm = TRUE),
        Avg_Var25 = mean(`tBodyGyroJerk-mean()-X`, na.rm = TRUE),
        Avg_Var26 = mean(`tBodyGyroJerk-mean()-Y`, na.rm = TRUE),
        Avg_Var27 = mean(`tBodyGyroJerk-mean()-Z`, na.rm = TRUE),
        Avg_Var28 = mean(`tBodyGyroJerk-std()-X`, na.rm = TRUE),
        Avg_Var29 = mean(`tBodyGyroJerk-std()-Y`, na.rm = TRUE),
        Avg_Var30 = mean(`tBodyGyroJerk-std()-Z`, na.rm = TRUE),      
        Avg_Var31 = mean(`tBodyAccMag-mean()`, na.rm = TRUE),
        Avg_Var32 = mean(`tBodyAccMag-std()`, na.rm = TRUE),
        Avg_Var33 = mean(`tGravityAccMag-mean()`, na.rm = TRUE),
        Avg_Var34 = mean(`tGravityAccMag-std()`, na.rm = TRUE),
        Avg_Var35 = mean(`tBodyAccJerkMag-mean()`, na.rm = TRUE),
        Avg_Var36 = mean(`tBodyAccJerkMag-std()`, na.rm = TRUE),
        Avg_Var37 = mean(`tBodyGyroMag-mean()`, na.rm = TRUE),
        Avg_Var38 = mean(`tBodyGyroMag-std()`, na.rm = TRUE),
        Avg_Var39 = mean(`tBodyGyroJerkMag-mean()`, na.rm = TRUE),    
        Avg_Var40 = mean(`tBodyGyroJerkMag-std()`, na.rm = TRUE),
        Avg_Var41 = mean(`fBodyAcc-mean()-X`, na.rm = TRUE),
        Avg_Var42 = mean(`fBodyAcc-mean()-Y`, na.rm = TRUE),
        Avg_Var43 = mean(`fBodyAcc-mean()-Z`, na.rm = TRUE),
        Avg_Var44 = mean(`fBodyAcc-std()-X`, na.rm = TRUE),
        Avg_Var45 = mean(`fBodyAcc-std()-Y`, na.rm = TRUE),
        Avg_Var46 = mean(`fBodyAcc-std()-Z`, na.rm = TRUE),
        Avg_Var47 = mean(`fBodyAcc-meanFreq()-X`, na.rm = TRUE),
        Avg_Var48 = mean(`fBodyAcc-meanFreq()-Y`, na.rm = TRUE),
        Avg_Var49 = mean(`fBodyAcc-meanFreq()-Z`, na.rm = TRUE),
        Avg_Var50 = mean(`fBodyAccJerk-mean()-X`, na.rm = TRUE),
        Avg_Var51 = mean(`fBodyAccJerk-mean()-Y`, na.rm = TRUE),      
        Avg_Var52 = mean(`fBodyAccJerk-mean()-Z`, na.rm = TRUE),
        Avg_Var53 = mean(`fBodyAccJerk-std()-X`, na.rm = TRUE),
        Avg_Var54 = mean(`fBodyAccJerk-std()-Y`, na.rm = TRUE),
        Avg_Var55 = mean(`fBodyAccJerk-std()-Z`, na.rm = TRUE),
        Avg_Var56 = mean(`fBodyAccJerk-meanFreq()-X`, na.rm = TRUE),
        Avg_Var57 = mean(`fBodyAccJerk-meanFreq()-Y`, na.rm = TRUE),
        Avg_Var58 = mean(`fBodyAccJerk-meanFreq()-Z`, na.rm = TRUE),
        Avg_Var59 = mean(`fBodyGyro-mean()-X`, na.rm = TRUE),
        Avg_Var60 = mean(`fBodyGyro-mean()-Y`, na.rm = TRUE),      
        Avg_Var61 = mean(`fBodyGyro-mean()-Z`, na.rm = TRUE),
        Avg_Var62 = mean(`fBodyGyro-std()-X`, na.rm = TRUE),
        Avg_Var63 = mean(`fBodyGyro-std()-Y`, na.rm = TRUE),
        Avg_Var64 = mean(`fBodyGyro-std()-Z`, na.rm = TRUE),
        Avg_Var65 = mean(`fBodyGyro-meanFreq()-X`, na.rm = TRUE),
        Avg_Var66 = mean(`fBodyGyro-meanFreq()-Y`, na.rm = TRUE),
        Avg_Var67 = mean(`fBodyGyro-meanFreq()-Z`, na.rm = TRUE),
        Avg_Var68 = mean(`fBodyAccMag-mean()`, na.rm = TRUE),
        Avg_Var69 = mean(`fBodyAccMag-std()`, na.rm = TRUE), 
        Avg_Var70 = mean(`fBodyAccMag-meanFreq()`, na.rm = TRUE),
        Avg_Var71 = mean(`fBodyBodyAccJerkMag-mean()`, na.rm = TRUE),      
        Avg_Var72 = mean(`fBodyBodyAccJerkMag-std()`, na.rm = TRUE),
        Avg_Var73 = mean(`fBodyBodyAccJerkMag-meanFreq()`, na.rm = TRUE),
        Avg_Var74 = mean(`fBodyBodyGyroMag-mean()`, na.rm = TRUE),
        Avg_Var75 = mean(`fBodyBodyGyroMag-std()`, na.rm = TRUE),
        Avg_Var76 = mean(`fBodyBodyGyroMag-meanFreq()`, na.rm = TRUE),
        Avg_Var77 = mean(`fBodyBodyGyroJerkMag-mean()`, na.rm = TRUE),
        Avg_Var78 = mean(`fBodyBodyGyroJerkMag-std()`, na.rm = TRUE),
        Avg_Var79 = mean(`fBodyBodyGyroJerkMag-meanFreq()`, na.rm = TRUE),
        Avg_Var80 = mean(`angle(tBodyAccMean,gravity)`, na.rm = TRUE),      
        Avg_Var81 = mean(`angle(tBodyGyroMean,gravityMean)`, na.rm = TRUE),
        Avg_Var82 = mean(`angle(tBodyGyroJerkMean,gravityMean)`, na.rm = TRUE),
        Avg_Var83 = mean(`angle(X,gravityMean)`, na.rm = TRUE),
        Avg_Var84 = mean(`angle(Y,gravityMean)`, na.rm = TRUE),
        Avg_Var85 = mean(`angle(Z,gravityMean)`, na.rm = TRUE),
        
        
        # Add more lines for each variable you want to average
        #.groups = 'drop' # This removes the grouping after summarization
    )