analyze.beh<-function(subnum){

#set path
setwd('c:\\topic_mind wandering\\3data\\')

#load data
load(paste0("beh_rfile\\",subnum,".RData"))

#load output vars
load("beh reports.RData") #for sart.t, sart.nt, vs.t
#load("beh modelling.RData") #for lr

#process sart.t
#for (i in 1:6){
  
#  temp<-filter(sart,eventnum==i*10)
#  temp2<-filter(sart,eventnum==i*10+1)
#  sart.t[subnum,paste0('type',i,'.acc')]<-nrow(temp)/(nrow(temp)+nrow(temp2))
#  sart.t[subnum,paste0('type',i,'.n.c')]<-nrow(temp)
#  sart.t[subnum,paste0('type',i,'.n.inc')]<-nrow(temp2)
  
#}

temp<-filter(sart,eventnum %in% c(10,20,15,25)) #acc for both t, nt
temp2<-filter(sart,eventnum %in% c(11,21,16,26))
sart.df[subnum,'ot.acc']<-nrow(temp)/(nrow(temp)+nrow(temp2))
sart.df[subnum,'ot.n.c']<-nrow(temp)
sart.df[subnum,'ot.n.inc']<-nrow(temp2)
temp<-filter(sart,eventnum %in% c(15,25)) #rt for nt only
temp2<-filter(sart,eventnum %in% c(16,26))
sart.df[subnum,'ot.median.c']<-median(temp$rt)
sart.df[subnum,'ot.median.inc']<-median(temp2$rt)
sart.df[subnum,'ot.mean.c']<-mean(temp$rt)
sart.df[subnum,'ot.mean.inc']<-mean(temp2$rt)
sart.df[subnum,'ot.sd.c']<-sd(temp$rt)
sart.df[subnum,'ot.sd.inc']<-sd(temp2$rt)



#process sart.nt
#for (i in 1:6){
#  
#  temp<-filter(sart,eventnum==i*10+5)
#  temp2<-filter(sart,eventnum==i*10+6)
#  sart.nt[subnum,paste0('type',i,'.acc')]<-nrow(temp)/(nrow(temp)+nrow(temp2))
#  sart.nt[subnum,paste0('type',i,'.n.c')]<-nrow(temp)
#  sart.nt[subnum,paste0('type',i,'.n.inc')]<-nrow(temp2)
#  sart.nt[subnum,paste0('type',i,'.median.c')]<-median(temp$rt)
#  sart.nt[subnum,paste0('type',i,'.median.inc')]<-median(temp2$rt)
#  sart.nt[subnum,paste0('type',i,'.mean.c')]<-mean(temp$rt)
#  sart.nt[subnum,paste0('type',i,'.mean.inc')]<-mean(temp2$rt)
#  sart.nt[subnum,paste0('type',i,'.sd.c')]<-sd(temp$rt)
#  sart.nt[subnum,paste0('type',i,'.sd.inc')]<-sd(temp2$rt)
  
#}

temp<-filter(sart,eventnum %in% c(30,50,60,35,55,65)) #acc for both t, nt
temp2<-filter(sart,eventnum %in% c(31,51,61,36,56,66))
sart.df[subnum,'mw.acc']<-nrow(temp)/(nrow(temp)+nrow(temp2))
sart.df[subnum,'mw.n.c']<-nrow(temp)
sart.df[subnum,'mw.n.inc']<-nrow(temp2)
temp<-filter(sart,eventnum %in% c(35,55,65)) #rt for nt only
temp2<-filter(sart,eventnum %in% c(36,56,66))
sart.df[subnum,'mw.median.c']<-median(temp$rt)
sart.df[subnum,'mw.median.inc']<-median(temp2$rt)
sart.df[subnum,'mw.mean.c']<-mean(temp$rt)
sart.df[subnum,'mw.mean.inc']<-mean(temp2$rt)
sart.df[subnum,'mw.sd.c']<-sd(temp$rt)
sart.df[subnum,'mw.sd.inc']<-sd(temp2$rt)


#process vs
#for (i in 1:6){
#  
#  temp<-filter(vs,eventnum==i*10+3)
#  temp2<-filter(vs,eventnum==i*10+4)
#  vs.t[subnum,paste0('type',i,'.acc')]<-nrow(temp)/(nrow(temp)+nrow(temp2))
#  vs.t[subnum,paste0('type',i,'.n.c')]<-nrow(temp)
#  vs.t[subnum,paste0('type',i,'.n.inc')]<-nrow(temp2)
#  vs.t[subnum,paste0('type',i,'.median.c')]<-median(temp$rt)
#  vs.t[subnum,paste0('type',i,'.median.inc')]<-median(temp2$rt)
#  vs.t[subnum,paste0('type',i,'.mean.c')]<-mean(temp$rt)
#  vs.t[subnum,paste0('type',i,'.mean.inc')]<-mean(temp2$rt)
#  vs.t[subnum,paste0('type',i,'.sd.c')]<-sd(temp$rt)
#  vs.t[subnum,paste0('type',i,'.sd.inc')]<-sd(temp2$rt)
  
#}

temp<-filter(vs,eventnum %in% c(13,23)) #acc for both t, nt
temp2<-filter(vs,eventnum %in% c(14,24))
vs.df[subnum,'ot.acc']<-nrow(temp)/(nrow(temp)+nrow(temp2))
vs.df[subnum,'ot.n.c']<-nrow(temp)
vs.df[subnum,'ot.n.inc']<-nrow(temp2)
vs.df[subnum,'ot.median.c']<-median(temp$rt)
vs.df[subnum,'ot.median.inc']<-median(temp2$rt)
vs.df[subnum,'ot.mean.c']<-mean(temp$rt)
vs.df[subnum,'ot.mean.inc']<-mean(temp2$rt)
vs.df[subnum,'ot.sd.c']<-sd(temp$rt)
vs.df[subnum,'ot.sd.inc']<-sd(temp2$rt)

temp<-filter(vs,eventnum %in% c(33,53,63)) #acc for both t, nt
temp2<-filter(vs,eventnum %in% c(34,54,64))
vs.df[subnum,'mw.acc']<-nrow(temp)/(nrow(temp)+nrow(temp2))
vs.df[subnum,'mw.n.c']<-nrow(temp)
vs.df[subnum,'mw.n.inc']<-nrow(temp2)
vs.df[subnum,'mw.median.c']<-median(temp$rt)
vs.df[subnum,'mw.median.inc']<-median(temp2$rt)
vs.df[subnum,'mw.mean.c']<-mean(temp$rt)
vs.df[subnum,'mw.mean.inc']<-mean(temp2$rt)
vs.df[subnum,'mw.sd.c']<-sd(temp$rt)
vs.df[subnum,'mw.sd.inc']<-sd(temp2$rt)


#save result
save(sart.df, vs.df, file='beh reports.rdata')

#mw rate

#load('probe reports.rdata')

#temp=filter(sart,probe_on==1)
#temp2=filter(vs,probe_on==1)
#for (i in 1:6){
#  content.n[subnum,paste0('sart.type',i)]<-nrow(filter(temp,response_probe_content==i))
#  content.n[subnum,paste0('vs.type',i)]<-nrow(filter(temp2,response_probe_content==i))
#}

#save(content.n,file='probe reports.rdata')


#func end
}
