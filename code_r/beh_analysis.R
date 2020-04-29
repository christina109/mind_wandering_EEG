library(dplyr)



get.rt <- function(sub, task, dims, triggerSets, meanOn = TRUE){
  data <- get.dataSubset(sub, task, dims,triggerSets)
  data <- filter(data, rt>0) # remove the target trials
  if (meanOn){
    return(mean(data$rt))
  } else {
    return(data$rt)
  }
}



get.acc <- function(sub, task, dims, triggerSets, meanOn = TRUE){
  data <- get.dataSubset(sub, task, dims,triggerSets)
  if (meanOn){
    return(mean(data$correct))
  } else {
    return(data$correct)
  }
}



get.trialCount <- function(sub, task, dims, triggerSets){
  data <- get.dataSubset(sub, task, dims,triggerSets)
  return(nrow(data))
}



get.dataSubset <- function(sub, task, dims, triggerSets){
  
  # subset the data on the specified dimensions x triggerSets
  
  # check inputs
  if (length(dims) != length(triggerSets)){stop('Dims and triggerSets must be matched in length!', call. = FALSE)}
  
  load(paste0("beh_rfile\\", sub, ".RData"))
  eval(parse(text = paste0('data <- ', task)))
  
  for (dimi in 1:length(dims)){
    dim <- dims[dimi]
    if (!dim %in% c('orientation','stickiness','emotion','eventnum')){stop('Invalid dimension name!', call. = FALSE)}
    triggers <- triggerSets[[dimi]]
    eval(parse(text = paste0('data <- filter(data, ', dim, ' %in% triggers)')))
  }
  
  return(data)
  
}



get.mwrate <- function(subs, tasks){
  
  # if length(tasks) > 1, generate combined mw rate
  # Note trialCount.rdata stores only the number of the trials entering the machine
  
  load('probe reports.rdata')  # get content.n
  
  stateId <- which(states == 'mw')
  types   <- typeSets[[stateId]]
  
  for (taski in 1:length(tasks)){
    
    task <- tasks[taski]
    for (typei in 1:length(types)){
      type <- types[typei]
      if (typei * taski == 1) {
        eval(parse(text = paste0('mwCount <- content.n$', task, '.type', type)))
      } else {
        eval(parse(text = paste0('mwCount <- mwCount + content.n$', task, '.type', type)))
      }
    }
    
    allTypeIdx <- which(grepl(task, colnames(content.n))) 
    if (taski == 1){
      allCount   <- rowSums(content.n[, allTypeIdx])
    } else {
      allCount   <- allCount + rowSums(content.n[, allTypeIdx])
    }
    
  }
  
  return(mwCount[subs]/allCount[subs])
}

get.content.rate <- function(subs, task) {
  
  # generate content propostion 
  # Note trialCount.rdata stores only the number of the trials entering the machine
  
  load('probe reports.rdata')  # get content.n
  
  # subset
  if (task %in% c('sart', 'vs')) {
    perc <- content.n[subs, paste0(task, '.type', 1:6)]
  } else if (task %in% c('merge', 'all')) {
    perc <- content.n[subs, paste0('sart', '.type', 1:6)] + content.n[subs, paste0('vs', '.type', 1:6)]
  } else {
    print(paste0('Invalid task: ', task))
  }
  colnames(perc) <- paste0('a', 1:6)
  
  # convert to percentage
  perc <- perc / matrix(rep(rowSums(perc), 6), nrow = length(subs), ncol = 6)
  
}


get.subs <- function(threshold, f_trialCount = 'trialCount.rdata'){
  
  load(f_trialCount)

  subs <- c(1:30)[trialCount.sart[,1]>=threshold & trialCount.sart[,2]>=threshold &
                    trialCount.vs[,1]>=threshold &   trialCount.vs[,2]>=threshold]
  
}
