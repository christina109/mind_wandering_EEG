# Author:  Christina Jin
# Contact: christina.mik109@gmail.com

rm(list = ls())

drive <- 'c'

f_sep <- '\\'
drive  <- paste0(drive, ':', f_sep)

f_main <- paste0(drive, 'topic_mind wandering', f_sep, '3data', f_sep)
f_code <- paste0(f_main, 'code_r', f_sep)
f_tool <- paste0(drive, 'files', f_sep, 'tool_code', f_sep)
f_result <- paste0(drive, 'topic_mind wandering', f_sep, '4result', f_sep)

setwd(f_main)

source(paste0(f_code, 'pars.r'))
source(paste0(f_code, 'feat_modelling2.r'))
source(paste0(f_code, 'beh_analysis.r'))
source(paste0(f_code, 'plot_data.r'))
source(paste0(f_tool, 'my func.r'))
library(tictoc)
library(beepr)
library(tcltk)
library(lme4)

# global pars
# subs  <- get.subs(threshold = 30)  # default: mw vs. ot
# subs  <- get.subs(threshold = 20, 'trialCount_SvsNS.rdata')  # for mw.s vs. mw.ns
# subs  <- get.subs(30, 'trialCount_content.rdata')  # for eoa vs. ioa
subs.common <- intersect(get.subs(30, 'trialCount_12vs35.rdata'), get.subs(30, 'trialCount_1vs235.rdata'))
subs <- get.subs(30, 'trialCount_12vs35.rdata')
# subs <- 1:30
tasks <- c('sart', 'vs')
# tasks <- c('all')

states   <- c('ot', 'mw')
# states <- c('mw.ns', 'mw.s')
# states <- c('eoa', 'ioa')

# contentsets <- FALSE  # load data through get.data()
# contentsets <- list(paste0(1, c('c', 'i')), paste0(rep(c(2,3,5), each = 2), c('c', 'i')))
contentsets <- list(paste0(rep(c(1,2), each = 2), c('c', 'i')), paste0(rep(c(3,5), each = 2), c('c', 'i')))

f_measure_matfile <- 'measure_matfile_content'

##################################
#       Behaviral analysis       #
##################################

###   analyze MW rate  ###

mwRate   <- matrix(0,length(subs),3) 
taskSets <- c('all','sart','vs')
colnames(mwRate) <- taskSets

for (seti in 1:length(taskSets)){
  set <- taskSets[seti]
  if (set == 'all'){set <- c('sart','vs')}
  mwRate[,seti] <- get.mwrate(subs, set)
}

# save as: mwRate.rdata

library(lsr)
summary(mwRate)
se <- colSds(mwRate)/sqrt(nrow(mwRate)-1)
t.test(mwRate[,'sart'],mwRate[,'vs'], paired = TRUE)
cohensD(mwRate[,'sart'],mwRate[,'vs'], method = 'paired')



###   analyze MW rate: cat by contentsets  ###

perc <- get.content.rate(subs, 'sart')  # task = 'sart', 'vs', or 'merge'
colMeans(perc)
colSds(as.matrix(perc))


###   analyze ACC & RT   ###

# get averaged data (getMean = TRUE) / pooled data (getMean = FALSE)
getMean = TRUE 

behMeasures <- c('acc','rt')
dim <- 'eventnum'
for (measure in behMeasures){
  
  # initialize
  if (getMean){
    perf <- matrix(0, length(subs), length(tasks) * length(states))
    colnames(perf) <- paste0(rep(tasks, each = 2), '_', states)
    rownames(perf) <- subs
  }
  
  for (taski in 1:length(tasks)){
    task <- tasks[taski]
    if (task == 'sart') {
      triggerbase <- c(0,5)
    } else if (task == 'vs') {
      triggerbase <- 3
    } else if (task == 'merge') {
      triggerbase <- c(0,3,5)
    }
    for (si in 1:length(states)){
      state <- states[si]
      # to use default conditions
      if (!is.list(contentsets)){
        triggers <- list(triggerSets[[taski]][[si]])
        triggers <- unlist(triggers)
      } else { # get triggers based on the content
        content <- contentsets[[si]]
        triggertens <- unique(as.integer(substr(content,1,1)))
        triggers <- rep(10*triggertens, each = length(triggerbase)) + triggerbase
      }
      if (measure == 'acc'){ # if analyze acc, error trials should be included
        triggers <- list(c(triggers, triggers +1))
      } else {
        triggers <- list(triggers)
      }
      
       
      for (subi in 1:length(subs)){
        sub <- subs[subi]
        temp <- eval(parse(text = paste0('get.', measure, '(sub, task, dim, triggers, getMean)'))) 
        if (getMean){
          perf[subi,si + (taski - 1)*2] <- temp
        } else {
          temp <- data.frame(val = temp, state = state, task = task, sub = sub)
          if (taski * si * subi == 1){
            df <- temp
          } else {
            df <- rbind(df, temp)
          }
        }
      }  # loop over subs
    } # loop over states
  } # loop over tasks
  
  if (getMean){
    eval(parse(text = paste0(measure, '<- perf')))
  } else {
    eval(parse(text = paste0(measure, '<- df')))
  }
  
}

# save as: behMeas_12vs35.rdata

summary(acc)
summary(rt)

library(lsr)
stat <- matrix(0, 4, 3)
rownames(stat) <- paste0(rep(tasks, each = 2), '_', c('acc','rt'))
colnames(stat) <- c('t', 'p', 'd')
for (taski in 1:2){
  task <- tasks[taski]
  for (mi in 1:2){
    eval(parse(text = paste0('measure <- ', c('acc', 'rt')[mi])))
    x <- measure[,paste0(task, '_', 'mw')]
    y <- measure[,paste0(task, '_', 'ot')]
    temp <- t.test(x, y, paired = TRUE)
    stat[(taski-1)*2+mi, 1:3] <- c(temp$statistic, temp$p.value, cohensD(x, y, method = 'paired'))
  }
}



###   plot ACC & RT   ###

file = 'behMeas_12vs35.rdata'
load(file)

accDiff <- matrix(0, nrow(acc), 2)
 rtDiff <- matrix(0, nrow(acc), 2)
colnames(accDiff) <- tasks
colnames(rtDiff)  <- tasks

for (taski in 1:2) {
  task <- tasks[taski]
  accDiff[, taski] <- acc[, paste0(task, '_mw')] - acc[, paste0(task, '_ot')]
   rtDiff[, taski] <-  rt[, paste0(task, '_mw')] -  rt[, paste0(task, '_ot')]
}


library(ggpubr)

acc2plot  <- arrangeData2plot(accDiff, sumData = TRUE, separateNames = 'Task', varRank = list(tasks), se = 'ci')
rt2plot   <- arrangeData2plot( rtDiff, sumData = TRUE, separateNames = 'Task', varRank = list(tasks), se = 'ci')

plotAcc <- plot.bar(acc2plot, valName = 'mean', groupName = 'Task', errorValName = 'ci') +
  scale_x_discrete(labels = toupper(tasks)) +
  labs(title = 'ACC difference: MW minus OT', y = 'Accuracy')

plotRt  <- plot.bar( rt2plot, valName = 'mean', groupName = 'Task', errorValName = 'ci') +
  scale_x_discrete(labels = toupper(tasks)) +
  labs(title =  'RT difference: MW minus OT', y = 'Response time [ms]')

ggarrange(plotAcc, plotRt, ncol = 2,nrow = 1, common.legend = TRUE)



##################################
#          ML analysis           #
##################################

###   count trials   ###
# compatible for S vs. NS

contentsetsOn <- TRUE
back3On <- TRUE
for (task in tasks) {
  for (sub in 1:30) {
    if (contentsetsOn) {
      data <- get.data.content(sub, task, states, contentsets, measures, feats, folders, back3On = back3On)
    } else {
      data <- get.data(sub, task, measures, feats, folders)  
    }
    if (sub == 1){
      trialCount <- summary(data$state)
    } else {
      trialCount <- rbind(trialCount, summary(data$state))
    }
  }
  rownames(trialCount) <- 1:30
  eval(parse(text = paste0('trialCount.', task, ' <- trialCount' )))
}

# save as: trialCount.rdata

# compute the remaining rate 
rawCount <- readMat('trialCount_beforeRej.mat')
rawCount <- rawCount[[1]]
remainCount <- rowSums(trialCount.sart) + rowSums(trialCount.vs)
remainRate  <- c(remainCount/rawCount[1,])[subs]
summary(remainRate)



###   individual modelling   ###
# Note. Check "states" & "path" setting in the "featModelling2.r"
get.settings()

source(paste0(f_code, f_sep, 'feat_modelling_main.r'))
# set pars
mtype           <- 'lr'  # algorithm: lr, svm, knn
validType       <- 'cv'  # 'cv', 'loo'

# pars when mtype == 'svm'
f_parMat        <- NaN    # if NaN, use the default pars
gridSearchOn    <- TRUE
searchBy        <- 'acc'  # 'acc', 'kappa', 'sen-spe'

# special treat on data before training
taskMergeOn     <- TRUE
sessNormalize <- FALSE
get3bkData    <- FALSE

# parellel processing?
parallelOn    <- FALSE  # parellel Id or FALSE to disablen it


f_temp_fun <- featModelling(subs, tasks, mtype, validType, f_parMat, gridSearchOn, searchBy, 
              sessNormalize, get3bkData, contentsets, taskMergeOn, parallelProcId = parallelOn)
print(paste0('Load data from ', f_temp_fun))
load(f_temp_fun)
acc.df   <- data.frame(accMat, sub = subs)
kappa.df <- data.frame(kapMat, sub = subs)
sensitivity.df <- data.frame(senMat, sub = subs)
specificity.df <- data.frame(speMat, sub = subs)

f_perf <- 'featModelling_lr_12vs35_taskMerge.rdata'
if (mtype == 'svm' && gridSearchOn){
  save(file = f_perf, acc.df, sensitivity.df, specificity.df, kappa.df, parMat)
} else {
  save(file = f_perf, acc.df, sensitivity.df, specificity.df, kappa.df)
}



###   test features   ###
source(paste0(f_code, f_sep, 'feat_testing_main.r'))

get.settings()

# parallel processing setting
subs.bk <- subs
subsetid <- NaN  # Nan - no parallel processing
parallelProcId <- FALSE  # FALSE or a number
if (is.vector(subsetid) && !is.nan(subsetid)) {
  subs <- subs.bk[subsetid]
  print(paste0('Process subset: ', subsetid))
  print(paste0('Pallell processing id: ', parallelProcId))
}

# set pars
mtype     <- 'lr'  # algorithm: lr, svm, knn
validType <- 'cv'  # 'cv'
taskMerge     <- TRUE

# pars when mtype == 'svm'
useParMat    <- FALSE
f_parMat     <- FALSE  # FALSE or .rdata name
gridSearchOn <- FALSE
searchBy     <- ''  # 'acc', 'kappa', 'sen-spe'

# special treat on data before training
contentsetsOn <- TRUE

if (mtype == 'svm' && !gridSearchOn && !is.logical(f_parMat)){
  load(f_parMat)
  if (nrow(parMat) == 30) { 
    parMat <- parMat[subs,] 
    sub.test <- acc.df[subs,]$sub
    }
  # check sub id
  if (!all.equal(sub.test,subs)) {warning('Check if the specified subs match the parMat!')}
} else {
  parMat <- NaN
}


if (contentsetsOn) {
  featTesting(subs, tasks, mtype, validType, parMat, taskMerge, useParMat, states = states, contentsets = contentsets, gridSearchOn = gridSearchOn, searchBy = searchBy, parallelProcId = parallelProcId)
} else {
  featTesting(subs, tasks, mtype, validType, parMat, taskMerge, useParMat)
}

load('temp_featTesting.rdata')
 

f_feat <- 'featTesting_taskMerge_lr_12vs35.rdata'
if (taskMerge){
  save(file = f_feat, accPerFeat)
} else {
  save(file = f_feat, list = paste0('accPerFeat.', tasks))
}



###   get trial indices   ###

idxList <- list()
for (subi in 1:length(subs)){
  sub <- subs[subi]
  for (ti in 1:2){
    task <- tasks[ti]
    temp <- get.trialIdx(sub, task, measures, feats, folders)
    if (ti == 1){
      df <- temp
    } else {
      df <- rbind(df, temp)
    }
  }
  idxList[[subi]] <- df
}

# save as: trialIdx4ml.rdata
# convert to csv 
for (subi in 1:length(subs)){
  sub <- subs[subi]
  temp <- idxList[[subi]]
  write.csv(file = paste0('trialIdx4ml_csvfile\\', sub, '.csv'), temp)
}

  
  
###   plot feature performance   ###
sortOn <- FALSE
sortBy <- 'lowCI' # 'mean', 'lowCI'
wholeModelOn <- TRUE
markSigOn <- FALSE

autoSaveOn <- FALSE

f_feat  = 'featTesting_taskMerge_searchByAcc_12vs35.rdata'
f_whole = 'featModelling_svm_parByAcc_12vs35_taskMerge.rdata'
load(f_feat)

if (wholeModelOn) {
  load(f_whole)
  # subset whole modelling data
  if (nrow(acc.df) == 30) {
    acc.df <- acc.df[as.integer(rownames(accPerFeat)),]
  }
}

if (wholeModelOn) { 
  df <- data.frame(accPerFeat, whole = acc.df[, 'accMat'], sub = acc.df$sub)
  df.sum <- arrangeData2plot(df, se = 'ci')  # plot 95% ci
  df.sum$key <- factor(df.sum$key, levels = c(measures, 'whole'))
  df.sum <- arrange(df.sum, key)
  df.sum$name <- c(measureNames, 'whole model')
  df.sum$color<- c(measureColors, '#000000')
  df.sum$type <- c(measureTypes, 'whole model')
  df.sum$type <- factor(df.sum$type, labels = c('whole model', 'ERP', 'power', 'ISPC'), levels = c('whole model', 'ERP', 'power', 'ISPC'))
  shapes <- c(11, 15, 16, 17) # matching df.sum$type
} else {
  df <- as.data.frame(accPerFeat)
  df.sum <- arrangeData2plot(df, se = 'ci')
  df.sum$key <- factor(df.sum$key, levels = measures)
  df.sum <- arrange(df.sum, key)
  df.sum$name <- measureNames
  df.sum$color <- measureColors
  df.sum$shape <- measureShapes
  df.sum$type <- measureTypes
  df.sum$type <- factor(df.sum$type, labels = c('ERP', 'power', 'ISPC'), levels = c('ERP', 'power', 'ISPC'))
  shapes <- c(15, 16, 17)
}

if (sortOn){
  if (sortBy == 'mean') {
    sortedLabels <- arrange(df.sum, desc(mean))$name
  } else if (sortBy == 'lowCI') {
    sortedLabels <- arrange(df.sum, desc(mean - se))$name
  } else {
    print('Invalid sorting method! Use mean instead.')
    sortedLabels <- arrange(df.sum, desc(mean))$name
  }
  df.sum$name <- factor(df.sum$name, levels = sortedLabels, labels =  sortedLabels)
  df.sum <- arrange(df.sum, name)
} else {
  df.sum <- arrange(df.sum, type)
  df.sum$name <- factor(df.sum$name, levels = unique(df.sum$name), labels =  unique(df.sum$name))
}

# compare single feat perf to 0.5: modify df.sum
mu <- 0.5
df.sum$t <- 0 
df.sum$p <- 0
df.sum$sig <- ''
for (feati in 1:length(feats)) {
  res <- t.test(df[, measures[feati]], mu = mu)
  df.sum[df.sum$key == measures[feati], c('t', 'p', 'sig')] <- c(round(res$statistic, 3), round(res$p.value, 3), ifelse(res$p.value < 0.001, '***', ifelse(res$p.value < 0.01, '**', ifelse(res$p.value < 0.05, '*', ''))))
}

# generate ggplot
p <- plot.point.flip(df.sum, 'mean', 'type', 'name', 'ci', ylim = c(0.45, 0.7), yintercep = 0.5, colorName = 'color', shape = shapes)
p <- p + labs(y = 'Accuracy', x = 'EEG marker', color = 'Marker type', shape = 'Marker type')
if (markSigOn) {
  p <- p + scale_x_discrete(labels = paste0(df.sum$sig, df.sum$name))
}

if (autoSaveOn) {ggsave(sub('rdata', 'png', f_feat), device = 'png', plot = p, path = f_result, dpi = 300, width = 25, height = 20, units = 'cm')}

# compare single feat perf to whole model perf
t2wm.res <- data.frame(marker = measureNames, t = 0, p = 0, sig = '', stringsAsFactors = FALSE)
for (feati in 1:length(feats)) {
  res <- t.test(df[, measures[feati]], df[,'whole'], paired = TRUE)
  t2wm.res[feati, 2:4] <- c(round(res$statistic, 3), round(res$p.value, 3), ifelse(res$p.value < 0.001, '***', ifelse(res$p.value < 0.01, '**', ifelse(res$p.value < 0.05, '*', ''))))
}
t2wm.res$marker <- factor(t2wm.res$marker, levels = measureNames, labels = measureNames)
t2wm.res <- arrange(t2wm.res, t)

# plot significantly predictive markers
get.settings()
f_feat
feats2plot <- get.sig.feats(df.sum)
plot.freq.change(subs, tasks, states, contentsets, feats2plot, normalizeOn = TRUE, 
                 f_output = sub('Testing', 'Sig', sub('.rdata', '', f_feat)))

# compare between different f_perfs
f_perf2 <- 'featTesting_taskMerge_searchByAcc_12vs35.rdata'
accPerFeat1 <- accPerFeat
load(f_perf2)
t2perf2.res <- data.frame(marker = measureNames, t = 0, p = 0, sig = '', stringsAsFactors = FALSE)
for (feati in 1:length(feats)) {
  res <- t.test(accPerFeat[, measures[feati]], accPerFeat1[, measures[feati]], paired = TRUE)
  t2perf2.res[feati, 2:4] <- c(round(res$statistic, 3), round(res$p.value, 3), ifelse(res$p.value < 0.001, '***', ifelse(res$p.value < 0.01, '**', ifelse(res$p.value < 0.05, '*', ''))))
}
t2perf2.res$marker <- factor(t2wm.res$marker, levels = measureNames, labels = measureNames)
t2perf2.res <- arrange(t2wm.res, t)


###   plot ML result: ACC   ###

library(ggpubr)
library(lsr)

datasumed = FALSE
f_perf <- 'featModelling_svm_parByAcc_12vs35.rdata'

load(f_perf)
task2plot <- c('sart','vs','sart2vs','vs2sart')
taskNames <- toupper(c('sart', 'vs', 'sart-vs', 'vs-sart'))
tcolors <- c("#ff7f00", "#a6d854", "#984ea3", "#386cb0")

# when data contains all subs
if (nrow(acc.df) == 30) {
  subs2plot <- subs
  acc.df <- acc.df[subs,]
} 
# acc.df <- na.omit(acc.df)
# subs2plot <- subs[as.integer(rownames(df.test))]
# subs2plot <- get.subs(30, 'trialCount_content.rdata')
# acc.df <- acc.df[subs2plot,]

df <- arrangeData2plot(acc.df, sumData = datasumed)
if (!'sub' %in% colnames(df)) {df$sub <- subs2plot}
df$sub <- factor(df$sub)
df$key <- factor(df$key, labels = task2plot, levels = task2plot)

if (!datasumed){
  p <- ggplot(df, aes(x = sub, y = val, fill = key)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.5) +
    scale_fill_manual(name = '', values = tcolors, labels = taskNames) +
    scale_x_discrete(labels = 1:length(subs2plot)) +  # x-tick names
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    labs(title = '', x = 'Subject', y = 'Prediction accuracy') +
    geom_hline(aes(yintercept = 0.5, col = '#800000'), linetype = 2) +
    get.mytheme()
} else {
  p <- plot.bar(df, 'mean',groupName = 'key', errorValName = 'se', ylim = c(0, 1), autoColor = FALSE) +
    scale_x_discrete(labels = toupper(taskNames)) +
    labs(title = '', x = '', y = 'Prediction accuracy') + 
    scale_fill_manual(name = '', values = tcolors, labels = taskNames)
  p <- ggplot(df, aes(x = key, y = mean, fill = key)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.5) +
    # scale_fill_manual(name = 'Training task-testing task', values = tcolors, labels = taskNames) +
    scale_x_discrete(labels = toupper(taskNames)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    labs(title = '', x = 'Training task-testing task', y = 'Prediction accuracy') +
    get.mytheme()
}

# to do t-test between acc and chance level:
chance <- 0.5
task <-'sart'
t.test(acc.df[,task], mu = chance)  # t-test for each task
cohensD(acc.df[,task], mu = chance)
acc.arrange <- arrangeData2plot(acc.df, sumData= FALSE)  
t.test(acc.arrange$val, mu = chance)  # t-test for all

# modelling perf comparison
f_perf2 <- 'featModelling_svm_parByAcc_12vs35.rdata'
acc.df1 <- acc.df
load(f_perf2)
if (nrow(acc.df) == 30) {
  subs2plot <- subs
  acc.df <- acc.df[subs,]
 } 
# check
if (!all.equal(acc.df$sub, acc.df1$sub)) {
  print('Unmatched subs!')
}
task <- 'sart'
t.test(acc.df[,task], acc.df1[,task], paired = TRUE)  # t-test for each task
cohensD(acc.df[,task], acc.df1[,task], method = 'paired')


###   plot ML result: ACC, sensitivity, specificity   ###

library(ggpubr)

f_perf <- 'featModelling_lr_12vs35.rdata'

load(f_perf)

indications <- c('acc','sensitivity','specificity')

if (nrow(acc.df) == 30 && !'sub' %in% names(acc.df)) {
  subs2plot <- subs
  acc.df <- acc.df[subs,]
  sensitivity.df <- sensitivity.df[subs,]
  specificity.df <- specificity.df[subs,]
} 

task2plot <- c('sart','vs','sart2vs','vs2sart')
taskNames <- toupper(sub('2', '-', x = task2plot))  # replace 2 as _ to be the label

for (indi in 1:length(indications)){
  indication <- indications[indi]
  eval(parse(text = paste0('temp <-', indication, '.df')))

  temp <- arrangeData2plot(temp, sumData = FALSE)
  temp$indication <- indication
  if (!'sub' %in% colnames(temp)) {temp$sub <- subs2plot}
  if (indi == 1){
    df <- temp
  } else {
    df <- rbind(df, temp)
  }
}
df$sub <- factor(df$sub)

for (taski in 1:length(task2plot)) {
  task <- task2plot[taski]
  dfSub <- subset(df, key == task)
  p <- plot.point(dfSub, xName = 'sub', yName = 'val', condName = 'indication', ylim = c(0,1))
  p <- p + scale_x_discrete(labels = 1:length(subs2plot)) +  # x-axis tick names
    scale_shape_manual(name = 'Performance Measure', values = 15:17, labels = c('Accuracy', 'Sensitivity', 'Specificity')) +
    scale_color_manual(name = 'Performance Measure', values = c('#4B0082', colors[2], colors[1]), labels = c('Accuracy', 'Sensitivity', 'Specificity'))
  if (length(task2plot) > 1){
    if (taski == 1){
      p <- p + labs(title = taskNames[taski], x = 'Subject', y = '')
    } else {
      p <- p + labs(title = taskNames[taski], x = '', y = '') 
    }
  }
  eval(parse(text = paste0('p', taski, '<- p')))
}
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, common.legend = TRUE)



###   correlation: MW rate & learning biase   ###

library(ggpubr)
load('mwRate_12vs35.rdata')
load('featModelling_svm_parByAcc_12vs35.rdata')
idx <- c('sensitivity', 'specificity')
if (nrow(sensitivity.df) == 30 ) {
  print('Subset the loaded data frame by the specified sub number.')
  print(paste0('The original data count is ', nrow(sensitivity.df)))
  print(paste0('The specified sub numbers are '))
  subs
  sensitivity.df <- sensitivity.df[subs,]
  specificity.df <- specificity.df[subs,]
}
if (nrow(mwRate) == 30 ) {
  mwRate <- mwRate[subs,]
}

# check if sub count is equal 
if (nrow(mwRate) != nrow(sensitivity.df)) {
  stop('Sub count must be the same in both perf file and mwRate file. Check please')
}

for (i in 1:length(idx)){
  for (ti in 1:length(tasks)){
    task <- tasks[ti]
    indication <- idx[i]
    eval(parse(text = paste0('temp <- ', indication, '.df$', task)))
    temp <- data.frame(indication = temp)
    temp$mwrate <- mwRate[,task]
    if (indication == 'sensitivity'){
      p <- ggscatter(temp, x = "mwrate", y = 'indication', add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5, ylim = c(0,1),
                     xlab = "mind-wandering rate", ylab = indication, title = toupper(task))
    } else {
      p <- ggscatter(temp, x = "mwrate", y = 'indication', add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "spearman", cor.coef.size = 5, ylim = c(0,1),
                     xlab = "mind-wandering rate", ylab = indication, title = toupper(task),
                     cor.coeff.args = list(label.x.npc = "right", label.y.npc = "top"))
    }
    p <- p + get.mytheme()
    eval(parse(text = paste0('p', (i-1)*2 + ti, '<- p')))
  }
}
g <- ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

ggsave('Figure 8.tif', device = 'tiff', plot = g, path = f_result, dpi = 300, width = 25, height = 25, units = 'cm')

