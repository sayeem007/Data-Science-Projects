install.packages("dplyr")
library(dplyr)

MidData<- read.csv("V:/MidProject.csv",header=TRUE ,sep=",")
MidData

MidData$pressurehight<-gsub("-","",MidData$pressurehight)
MidData

MidData[MidData == ""] <- NA
MidData

summary(MidData)
MidData$pressurehight<-as.integer(MidData$pressurehight)
summary(MidData)

str(MidData)

is.na(MidData)

colSums(is.na(MidData))

which(is.na(MidData$age))

RemovedMidData<-na.omit(MidData)
RemovedMidData

MidData$impluse[MidData$impluse > 190] <- NA
MidData


Mean_age<-mean(MidData$age,na.rm = TRUE)
Mean_age
MidData$age[is.na(MidData$age)]<-Mean_age
MidData

Median_impulse<-median(MidData$impluse,na.rm = TRUE)
Median_impulse
MidData$impluse[is.na(MidData$impluse)]<-Median_impulse
MidData


custom_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode_pressurehight <-custom_mode(MidData$pressurehight)
Mode_pressurehight
MidData$pressurehight[is.na(MidData$pressurehight)]<-Mode_pressurehight
MidData

MidData <- MidData[!is.na(MidData$gender),]
MidData

age<-MidData$age
sd(age)
impluse<-MidData$impluse 
sd(impluse )
pressurehight<-MidData$pressurehight  
sd(pressurehight)
pressurelow<-MidData$pressurelow  
sd(pressurelow)
glucose<-MidData$glucose      
sd(glucose)

MidData$gender<-factor(MidData$gender,levels = c("male","female"),labels = c(1,2))
MidData
MidData$class<-factor(MidData$class,levels = c("positive","negative"),labels = c(1,2))
MidData

install.packages("ggplot2")
library(ggplot2)

hist(age)
hist(impluse)
hist(pressurehight)
hist(pressurelow)
hist(glucose)

boxplot(MidData$age, MidData$gender ,MidData$impluse, MidData$pressurehight ,
        MidData$pressurelow ,MidData$glucose , MidData$class ,
        main="Box Plots for MidData",
        names=c("Age", "Gender","Impulse","Pressurehight","Pressurelow","Glucose","Class"))
