R.to.mat<-function(subnum){
library(R.matlab)
#remove outliers (beyond 3sd) simultaneously
   
#----------set parameters---------------------------------------------
individual.file.path<-'c:\\TOPIC_mind wandering\\3data\\beh_rfile\\'
mat.file.path<-'c:\\topic_mind wandering\\3data\\beh_matfile\\'
#---------------------------------------------------------------------
#LOAD DATA & FUNC
setwd(individual.file.path)
load(paste0(subnum,'.rdata'))

#CREATE EMPTY MATRIX
mat1<-NULL
mat2<-NULL

#FORM SART MAT
for (i in c(55, 65)){
  data.c<-sart[sart[,'eventnum']==i,c('correct','rt')]
  data.inc<-sart[sart[,'eventnum']==i+1,c('correct','rt')]
  mat1<-rbind(mat1,form.mat(data.c,data.inc,i))
}
for (i in c(74, 79, 84, 89)){
  data.c<-vs[vs[,'eventnum']==i,c('correct','rt')]
  data.inc<-vs[vs[,'eventnum']==i+1,c('correct','rt')]
  mat2<-rbind(mat2,form.mat(data.c,data.inc,i))
}

#OUTPUT MAT
setwd(mat.file.path)
writeMat(paste0(subnum,'.mat'), sart=mat1, vs=mat2)

} #END FUNC

form.mat<-function(data.c,data.inc,i){
  temprt<-data.c$rt;
  range<-c(mean(temprt)-3*sd(temprt),mean(temprt)+3*sd(temprt))
  data.c[,'outlier']<-temprt<range[1] | temprt>range[2]
  if (nrow(data.inc)>0){data.inc[,'outlier']<-FALSE}
  data<-rbind(data.c,data.inc)
  data<-data[!data[,'outlier'],]
  data[,'eventnum']<-i
  data[,'rt']<-data[,'rt']/1000
  data<-data[,c('eventnum','correct','rt')]
  return (as.matrix(data))
}