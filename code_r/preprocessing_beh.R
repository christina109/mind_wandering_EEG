preprocessing.beh<-function(subnum, sessionnum, nback = 5){
  
#1-"session1" 2-"session2" 3-"session1+2"

library(readr)
library(dplyr)
library(data.table)

setwd('c:\\topic_mind wandering\\3data')


#IMPORT DATA, DISCREMENATE SART FROM VS
column_names<-c("code", "correct_keyboard_resp" , "correct_keyboard_response1", 
                "correct_keyboard_response2","correct_keyboard_response3","probe_on"  , 
                "response_probe_content",  "response_probe_orientation" , 
                "response_probe_stickness", "response_probe_valence",
                "response_time_keyboard_resp" , "response_time_keyboard_response1", 
                "response_time_keyboard_response2", "response_time_keyboard_response3" , 
                "stimulus", "task")
if (sessionnum<3){
  data<-read_csv(paste0("raw\\beh\\subject-",subnum,"_s", sessionnum, ".csv"))
  data<-data[,column_names]
  } else{
  data<-read_csv(paste0("raw\\beh\\subject-",subnum,"_s1.csv"))
  data_2<-read_csv(paste0("raw\\beh\\subject-",subnum,"_s2.csv"))
  data<-rbind(data[,column_names],data_2[,column_names])
  }

#add trial number
data$trial_num<-c(1:nrow(data))

#correct probe_on
if (sum(is.na(data$probe_on))>0){data[is.na(data$probe_on),]$probe_on <- 0}
data$probe_on <- as.logical(data$probe_on)

#mark tasks
data[data$task=='sart' & data$stimulus=='T','T/S']<-1
data[data$task=='sart' & data$stimulus!='T','T/S']<-2
data[data$task=='vs','T/S']<-3

#process probes
probe.num <- data[data$probe_on,]$trial_num
data$response_probe_content     <- as.numeric(data$response_probe_content)
data$response_probe_orientation <- as.numeric(data$response_probe_orientation)
data$response_probe_stickness   <- as.numeric(data$response_probe_stickness)
data$response_probe_valence     <- as.numeric(data$response_probe_valence)
data$FA1 <- 0  # for orientation, stickness, valence
data$FA0 <- 0  # for eventnum
for (i in probe.num){
  
  #check for invalid input
  if (data[i,'response_probe_content'] %in% 1:6) {
    
    data[i, 'FA0'] <- 1
    for (j in (-nback:-1)+i) {
      if (j<1) {next}
      data[j,'FA0']<-1  
      data[j,'response_probe_content']    <-data[i,'response_probe_content']
      data[j,'response_probe_orientation']<-data[i,'response_probe_orientation']
      data[j,'response_probe_stickness']  <-data[i,'response_probe_stickness']
      data[j,'response_probe_valence']    <-data[i,'response_probe_valence']
    }
    
    if (data[i,'response_probe_orientation'] %in% 1:3 &&
        data[i,'response_probe_stickness']   %in% 1:5 &&
        data[i,'response_probe_valence']     %in% 1:7) {
      data[i, 'FA1'] <- 1
      for (j in (-nback:-1)+i) {
        if (j<1) {next}
        data[j,'FA1']<-1  
      }
    }
  }
}
data$FA0 <- as.logical(data$FA0)
data$FA1 <- as.logical(data$FA1)
data[, c('orientation','stickiness','emotion')]<-0
data[data$FA1, 'orientation']<-as.integer(data[data$FA1,]$response_probe_orientation)+data[data$FA1,]$`T/S`*10
data[data$FA1, 'stickiness'] <-as.integer(data[data$FA1,]$response_probe_stickness)+data[data$FA1,]$`T/S`*10
data[data$FA1, 'emotion']    <-as.integer(data[data$FA1,]$response_probe_valence)+data[data$FA1,]$`T/S`*10


#subsetting by task
sart<-subset(data,subset=(task=="sart"))
vs  <-subset(data,subset=(task=="vs"))


#PRERPROCESSING SART:
#intialize
sart$rt     <- -1
sart$correct<- 999
for (i in 1:nrow(sart)){
  #rt: no response=0
  temp.rts <- c(sart[i,]$response_time_keyboard_response1, sart[i,]$response_time_keyboard_response2, sart[i,]$response_time_keyboard_response3)
  if (temp.rts[1] < 295){
    sum.rt <- temp.rts[1]
  } else if(temp.rts[2] < 895){
    sum.rt <- sum(temp.rts[1:2])
  } else if(temp.rts[3] < 2995){
    sum.rt <- sum(temp.rts)
  } else {
    sum.rt <- 0
  }
  sart[i,]$rt <- sum.rt
  
  #acc
  temp.keyres <- c(sart[i,]$correct_keyboard_response1, sart[i,]$correct_keyboard_response2, sart[i,]$correct_keyboard_response3)
  if (sart[i,]$stimulus =="T"){
    sart[i,]$correct <- ifelse(sum(temp.keyres) == 3, 1, 0)
  } else{ 
    sart[i,]$correct <- ifelse(sum(temp.keyres) > 0, 1, 0)
  }
}
sart$correct <- as.logical(sart$correct)
rm_sart_cols<-c("code", "correct_keyboard_resp" , "correct_keyboard_response1", 
"correct_keyboard_response2","correct_keyboard_response3","response_time_keyboard_resp" ,
"response_time_keyboard_response1", "response_time_keyboard_response2", "response_time_keyboard_response3")
sart[,rm_sart_cols]<-NULL

#adding eventnum(triggers)
#t1.c=10, t1.inc=11, t2.c=20, t2.inc=21, t3.c=30, t3.inc=31, t4.c=40, t4.inc=41, t5.c=50, t5.inc=51, t6.c=60, t6.inc=61
#nt1.c=15, nt1.inc=16, nt2.c=25, nt2.iinc=26, nt3.c=35, nt3.inc=36, nt4.c=45, nt4.inc=46, nt5.c=55, nt5.inc=56, nt6.c=65, nt6.inc=66
#not analyzed: nt.c=78, nt.inc=79, t.c=76, t.inc=77

sart$eventnum <- 999  #initialze 
sart[ sart$FA0 &  sart$probe_on,]$eventnum <- sart[sart$FA0 &  sart$probe_on,]$response_probe_content*10 + 0 + as.numeric(!sart[sart$FA0 &  sart$probe_on,]$correct)
sart[ sart$FA0 & !sart$probe_on,]$eventnum <- sart[sart$FA0 & !sart$probe_on,]$response_probe_content*10 + 5 + as.numeric(!sart[sart$FA0 & !sart$probe_on,]$correct)
sart[!sart$FA0 &  sart$stimulus == 'T',]$eventnum  <- as.numeric(!sart[!sart$FA0 &  sart$stimulus == 'T',]$correct) + 76
sart[!sart$FA0 & !sart$stimulus == 'T',]$eventnum  <- as.numeric(!sart[!sart$FA0 & !sart$stimulus == 'T',]$correct) + 78

#PREPROCESSING VS 
setnames(vs,"correct_keyboard_resp","correct")
vs$correct <- as.logical(vs$correct)
setnames(vs,"response_time_keyboard_resp","rt")
rm_vs_cols<-c("correct_keyboard_response1", "correct_keyboard_response2",
                "correct_keyboard_response3","response_time_keyboard_response1", 
                "response_time_keyboard_response2", "response_time_keyboard_response3","stimulus")
vs[,rm_vs_cols]<-NULL

#adding eventnum(triggers)
#t1.c=13, t1.inc=14, t2.c=23, t2.inc=24, t3.c=33, t3.inc=34, t4.c=43, t4.inc=44, t5.c=53, t5.inc=54, t6.c=63, t6.inc=64
#not analyzed: c=88, inc=89

vs$eventnum <- 999  #initialze
vs[ vs$FA0,]$eventnum <- vs[vs$FA0,]$response_probe_content*10 + 3 + as.numeric(!vs[vs$FA0,]$correct)
vs[!vs$FA0,]$eventnum <- as.numeric(!vs[!vs$FA0,]$correct) + 88


#CREATE MODIFIED EVENT SEQUENCE
#the colname in .txt need to be removed mannual
ev_column_names <- c("trial_num","eventnum","rt","correct","orientation","stickiness","emotion")
ev <- rbind(sart[,ev_column_names],vs[,ev_column_names])
ev <- ev[order(ev$trial_num),]
 
#SAVE OBJS
if (sessionnum<3){
    write.table(ev,file=paste0("preprocessing\\ev_ed\\ev_ed",subnum,"_s",sessionnum,".csv"),quote=FALSE,sep=",",row.names=FALSE)
  } else {
    save(data,sart,vs,ev,file=paste0("beh_rfile\\", subnum,".RData"))
}

} # func