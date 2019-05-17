rm(list=ls())

#### Prepare Data Sets####
#setwd("~/Box Sync/Shared - Shimin and Hyojung/Data")
setwd("C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/Data")
source("function.R")
data <- read.csv("used_18.csv",na.strings ="NA",stringsAsFactors=FALSE) # imported used data (2018)
#establish time column
data$time <- paste(data$Actual.Surgery.Date,data$Patient.In)
data$actual.Surgery.Date <- as.Date(data$Actual.Surgery.Date,format='%m/%d/%Y')
data$week <- weekdays(data$actual.Surgery.Date)
# insert prime time
simple <- read.csv("primetable2018.csv",na.strings ="NA",stringsAsFactors=FALSE)
simple$test <- paste(simple$OR,simple$week,simple$Prime.Time.Start,simple$Prime.Time.Stop)
simple <- simple[!duplicated(simple$test),]
simple <- subset(simple,select=-test)
data <- merge(simple,data,by=c("OR","week"))
data <- data[which(data$Scheduled.Start.Time!=""),]
data$count <- rep(1,nrow(data))

data <- data[which(data$Actual.Surgery.Date!="7/4/2017"),]
data <- data[which(data$Actual.Surgery.Date!="9/4/2017"),]
data <- data[which(data$Actual.Surgery.Date!="11/23/2017"),]
data <- data[which(data$Actual.Surgery.Date!="11/24/2017"),]
data <- data[which(data$Actual.Surgery.Date!="12/25/2017"),]
data <- data[which(data$Actual.Surgery.Date!="12/26/2017"),]
data <- data[which(data$Actual.Surgery.Date!="12/29/2017"),]
data <- data[which(data$Actual.Surgery.Date!="1/1/2018"),]
data <- data[which(data$OR!="MAIN WAIT ROOM-01"),]
sum(which(data$week=="Sunday"|data$week=="Saturday")) #exam if there is any weekends
data <- data[which(data$week!="Sunday"& data$week!="Saturday"),] # delete weekends

#order data.used according to date, OR, patient in
data <- (data[order(data$actual.Surgery.Date,data$OR,change(as.character(data$Patient.In))),])
data[which(complete.cases(data)==FALSE),]

#remove duplicated value by case number
data <- data[!duplicated(data$Case.Number),]

#double check duplicated
data$test <- paste(data$Actual.Surgery.Date,data$OR,data$Case.Class,data$Facility,data$Primary.Surgeon,data$Patient.In)
data <- data[!duplicated(data$test),]

# Establiash turnover time column
data$Turnover_new <- rep(0,nrow(data))
for (i in 2:nrow(data)) {
  if((data[i,]$actual.Surgery.Date==data[i-1,]$actual.Surgery.Date)&(data$OR[i]==data$OR[i-1]))
  {data$Turnover_new[i-1] <- change(as.character(data$Patient.In[i]))-change(as.character(data$Patient.Out[i-1]))
  i=i+1}
  else{i=i+1}
}
data$Pt_ID[which(data$Turnover_new<0)]
data[which(data$Turnover_new<0),]
data$Turnover_new[which(data$Turnover_new<0)]=0
turn <- data$Turnover_new[data$Turnover_new!=0]
#write.csv(turn,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/turnover.csv',na="")


# establish primetime duration
data$Prime.Time.Start <- change(as.character(data$Prime.Time.Start))
data$Prime.Time.Stop <- change(as.character(data$Prime.Time.Stop))
data$prime_duration <- data$Prime.Time.Stop-data$Prime.Time.Start
#establish actual duration column
data$Patient.In <- change(as.character(data$Patient.In))
data$Patient.Out <- change(as.character(data$Patient.Out))
data$Patient.Out2 <- data$Patient.Out
data$Patient.Out2[which((data$Patient.Out- data$Patient.In)<0)] <- data$Patient.Out2[which((data$Patient.Out- data$Patient.In)<0)]+24
data$act_duration <- data$Patient.Out2- data$Patient.In

#establish estimated duration column
data$Scheduled.Start.Time <- change(as.character(data$Scheduled.Start.Time))
data$Scheduled.Stop.Time <- change(as.character(data$Scheduled.Stop.Time))
data$Scheduled.Stop.Time2 <- data$Scheduled.Stop.Time
data$Scheduled.Stop.Time2[which((data$Scheduled.Stop.Time- data$Scheduled.Start.Time)<0)] <- data$Scheduled.Stop.Time2[which((data$Scheduled.Stop.Time- data$Scheduled.Start.Time)<0)]+24
data$est_duration <- data$Scheduled.Stop.Time2- data$Scheduled.Start.Time
#with in prime time
data$esthours <- rep(0,length(data$Patient.Out)) # esthours: overlap between total prime time and scheduled time (for both elective and add on)
data$acthours<- rep(0,length(data$Patient.Out)) # acthours: overlap between total prime time and actual time (for both elective and add on)
data$esthours <- inter(data$Scheduled.Start.Time,data$Scheduled.Stop.Time2,data$Prime.Time.Start,data$Prime.Time.Stop,data$esthours) #esthours: overlap between total prime time and scheduled time (for both elective and add on)
data$acthours <- inter(data$Patient.In,data$Patient.Out2,data$Prime.Time.Start,data$Prime.Time.Stop,data$acthours)

#eStabliash start count column
data$start <- rep(0,nrow(data))
data$start[1] <- 1
for (i in 2:nrow(data)) {
  if((data[i,]$actual.Surgery.Date!=data[i-1,]$actual.Surgery.Date)|(data$OR[i]!=data$OR[i-1]))
  {data$start[i] <- 1
  i=i+1}
  else{i=i+1}
}
#eStabliash end count column
data$end <- rep(0,nrow(data))
data$end[nrow(data)] <- 1
for (i in 2:nrow(data)) {
  if((data[i-1,]$actual.Surgery.Date!=data[i,]$actual.Surgery.Date)|(data$OR[i-1]!=data$OR[i]))
  {data$end[i-1] <- 1
  }
  else{}
}

#establish patient in offset
data$offset <- data$Patient.In-data$Scheduled.Start.Time
data$offset[which(data$offset<(-12))] <- data$offset[which(data$offset<(-12))]+24
which(data$offset<(-12))
#write.csv(data$offset,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/offset.csv',na="")


#establish overutilization time column
data$act_over_time <- data$act_duration- data$acthours
data$est_over_time <- data$est_duration- data$esthours
## save(data, file="OR_variability_2018_data_processed")

# establish main data
data.main <- data
data.main.elective <- data.main[which(data.main$Case.Class=="ELECTIVE"),]
data.main.addson <- data.main[which(data.main$Case.Class!="ELECTIVE"),]
#write.csv(data.main.elective$offset,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/elective.offset.csv',na="")
#write.csv(data.main.addson$offset,file = 'C:/Users/50362/Box Sync/Shared - Shimin and Hyojung/DES program/Rdata/addson.offset.csv',na="")

save(data.main, file="OR_variability_2018")
save(data.main.elective, file="OR_variability_2018_Elective")
save(data.main.addson, file="OR_variability_2018_Addon")