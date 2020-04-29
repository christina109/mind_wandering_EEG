library(R.matlab)
library(matrixStats)
library(e1071)
library(lme4)
library(class)
library(caret)
library(DMwR)
library(utils)
library(tcltk)




get.settings <- function(){
  print(paste0('Current feature path: ', f_measure_matfile))
  print(paste0('Tasks to build models: ', tasks))
  print(paste0('Current states of interest: ', states))
  print(paste0('Current definition of ', states, ' : ', contentsets))
  print(paste0('Subject count: ', length(subs)))
  
}



grid.search.svm <- function(data, cVec, gVec, validType = 'cv', balanced = 'copy', searchBy = 'acc', checkOn = FALSE) {
  
  mat <- matrix(0,length(cVec), length(gVec))
  count <- length(cVec) * length(gVec)
  ti <- 1
  pb <- tkProgressBar(title = 'Grid search SVM', min = 0, max = count, width = 500)
  for (ci in 1:length(cVec)){
    for (gi in 1:length(gVec)){
      parList <- list(c = cVec[ci], gamma = gVec[gi])
      
      # only for check purpose
      if (checkOn) {tic(paste0('Grid search when c=', parList$c, ', gamma=', parList$gamma))}
      
      if(validType == 'loo'){
        perf <- leave.one.out(data, mtype = 'svm', parList = parList, balanced = balanced)
      } else {
        perf <- cross.validation(data, mtype = 'svm', parList = parList, balanced = balanced)
      }
      
      # fail to build a model?
      if (is.nan(perf$accuracy)){
        print('No model being built. Quit grid search of the input data')
        close(pb)
        return(NaN)
      }
      
      if (searchBy == 'acc'){
        mat[ci,gi] <- perf$accuracy
      } else if (searchBy == 'kappa'){
        mat[ci,gi] <- perf$kappa
      } else if (searchBy %in% c('sen-spe', 'spe-sen')) {
        mat[ci,gi] <- perf$sensitivity + perf$specificity
      } else {
        warning('Invalid search criteria. Use the highest accuracy.')
        mat[ci,gi] <- perf$accuracy
      }
      
      if (checkOn){toc()}
      
      setTkProgressBar(pb, ti, label = paste(round(ti/count * 100, 1), "% done"))
      ti <- ti + 1
    }
  }
  close(pb)
  return(mat)
  
}



leave.one.out <- function(data, mtype, parList = list(), balanced = 'copy'){
  
  for (i in 1:nrow(data)) {
    
    test  <- data[i, ]
    train <- data[-i,]
    
    if (is.character(balanced)) {
      train <- balance.class(train, balanced)
    }
    
    pred <- feat.modeling(train, test, mtype, parList)
    if (i == 1){
      pred_class <- pred$predictions
      obs_class  <- pred$observations
    } else {
      pred_class <- unlist(list(pred_class, pred$predictions))
      obs_class  <- unlist(list(obs_class, pred$observations))
    }
    
  }
  
  performance <- measure.performance(pred_class, obs_class)
  return(performance)
  
}



cross.validation <- function(data, mtype, nfold = 10, parList = list(), balanced = 'copy'){
  
  # balanced indicates the way to balance the training dataset 
  
  idx <- split.sample(data, nfold)
  
  # unable split? (because insufficient samples)
  if (length(idx) == 0) {
    return(list(accuracy = NaN, kappa = NaN, 
                sensitivity = NaN, specificity = NaN))
  }
  
  for (foldi in 1:nfold) {
    
    train <- data[idx$trainIdx[[foldi]],]
    test  <- data[idx$testIdx[[foldi]], ]
    
    if (is.character(balanced)) {
      train <- balance.class(train, balanced)
    }
    
    pred <- feat.modeling(train, test, mtype, parList)
    if (foldi == 1){
      pred_class <- pred$predictions
      obs_class  <- pred$observations
    } else {
      pred_class <- unlist(list(pred_class, pred$predictions))  # to remain the factor type
      obs_class  <- unlist(list(obs_class, pred$observations))
    }
    
  }
  
  performance <- measure.performance(pred_class, obs_class)
  return(performance)
  
}



feat.modeling <- function(train, test, mtype, parList = list()){
  
  if (mtype == 'lr'){
    
    m <- glm(state ~ ., family = binomial('logit'), data = train)
    p <- predict(m, test, type = "response")
    states <- names(summary(train$state))
    if (is.factor(train$state)) {
      states <- factor(states, levels = states, labels = states)
    }
    p_class <- ifelse(p>0.5, states[2], states[1])
    p_class <- factor(p_class, levels = 1:length(states), labels = states)
    #pStrength <- p
    
  } else if (mtype == 'svm') {
    
    if (length(parList) > 0){
      m <- svm(state ~ ., train, probability = TRUE, cost = parList$c, gamma = parList$gamma)  
    } else {
      m <- svm(state ~ ., train, probability = TRUE)
    }

    p_class <- predict(m, test)
    #prob <- attr(predict(m, test, probability = TRUE), 'probabilities')
    #pStrength <- prob[,2] - prob[,1]
    
  } else if (mtype == 'knn') {
    
    # split feats from lab
    trainFeats <- train[, -which(colnames(train) %in% 'state')]
    testFeats  <-  test[, -which(colnames(test)  %in% 'state')]
    p_class <- knn(trainFeats, testFeats, train$state, k = 5)
    #p <- attr(knn(trainFeats, testFeats, train$state, k = 5, prob = TRUE),'prob')
    #p[p_class == states[1]] <- -p[p_class == states[1]]
    #pStrength <- p
    m <- 'No model for KNN'
    
  } else {
    
    stop('Invalid model type!')
    
  }
  
  return(list(predictions = p_class, observations = test$state, model = m))
  
}



measure.performance <- function(pred_class, obs_class) {
  
  perf <- confusionMatrix(pred_class, obs_class)

  if (perf[['positive']] == states[1]){
    tnr <- perf[[c('byClass','Sensitivity')]]
    tpr <- perf[[c('byClass','Specificity')]]
  } else if (perf[['positive']] == states[2]){
    tpr <- perf[[c('byClass','Sensitivity')]]
    tnr <- perf[[c('byClass','Specificity')]]
  }
  
  return(list(accuracy = perf[[c('overall','Accuracy')]], kappa = perf[[c('overall','Kappa')]], 
              sensitivity = tpr, specificity = tnr))
  
}



split.sample <- function(data, nfold = 10, mincount = 10){ 
 
  #set.seed(54)
  
  rawIdx <- list()
  count <- c()
  ndataPerFold <- c()
  
  for (si in 1:length(states)){ 
    
    rawIdx[[si]] <- c(1:nrow(data))[data$state == states[si]]
    count[si] <- length(rawIdx[[si]])
    
    ndataPerFold[si] <- ceiling(count[si]/nfold)
    
    if (ndataPerFold[si] <= ndataPerFold[si]*nfold - count[si]) {
      ndataPerFold[si] <- floor(count[si]/nfold)
    }
    
    if (count[si] > 1) {rawIdx[[si]] <- sample(rawIdx[[si]], count[si])} # shuffle idx
    
  }
  
  # enough samples?
  if (min(count) < mincount) {
    print(paste0('Class size less than ', mincount, '. Fail to split data.'))
    return(list())
  }
  
  trainIdx <- list()
  testIdx  <- list()
  
  for (foldi in 1:nfold){
    
    startPosition <- ndataPerFold * (foldi - 1) + 1
    
    if (foldi < nfold) {
      endPosition <- ndataPerFold * foldi
    } else {
      endPosition <- count
    }
    
    for (si in 1:length(states)){
      testPositions <- startPosition[si]:endPosition[si]
      if (si == 1){
        testIdx[[foldi]]  <- rawIdx[[si]][testPositions]
        trainIdx[[foldi]] <- rawIdx[[si]][-testPositions]
      } else {
        testIdx[[foldi]]  <- c(testIdx[[foldi]], rawIdx[[si]][testPositions])
        trainIdx[[foldi]] <- c(trainIdx[[foldi]], rawIdx[[si]][-testPositions])
      }
    }
    
  }
  
  return(list(trainIdx = trainIdx, testIdx = testIdx))
  
}



balance.class <- function(data, method = 'copy'){
  
  count <- c()
  for (si in 1:length(states)){
    eval(parse(text = paste0('data', si, ' <- subset(data, state == states[si])')))
    eval(parse(text = paste0('count[si] <- nrow(data', si, ')')))
  }
  
  # if one class is empty
  if (min(count) == 0){
    print('One of the classes is empty! Return NaN')
    return(NaN)
  }
  if (max(count) == min(count)) {return(data)}
  
  nCopy   <- floor(max(count)/min(count)) - 1
  nSelect <- max(count) %% min(count)
  
  if (method == 'copy'){
   
    data2copy <- eval(parse(text = paste0('data', which(count == min(count)))))
    
    if (nCopy > 0){
      for (copyi in 1:nCopy){
        if (copyi == 1){
          copy <- data2copy
        } else {
          copy <- rbind(copy, data2copy)
        }
      }
    }
    
    if (nSelect > 0){
      if (nCopy > 0){
        copy <- rbind(copy, data2copy[sample(1:min(count), nSelect),])
      } else {
        copy <- data2copy[sample(1:min(count), nSelect),]
      }
    } 
    
    newData <- rbind(data, copy)
    
  } else if (toupper(method) == 'SMOTE'){
    
    newData <- SMOTE(state~., data, perc.over = nCopy*100 + 100*(min(1,nSelect)), 
                     k = 5, perc.under = (1 + 1/(nCopy + min(1,nSelect))) * 100)
    
  } 
 
  return(newData)
  
}



normalize <- function(data, algorithm, pars = list()){
  
  # normalize data within each column
  # algorithm options are: 
  # - range
  # - z (pars: colMeans, colSds)
  
  # normalize each column (feature)
  labs <- data$state
  
  dataMat <- as.matrix(subset(data, select = -state))
  nObs    <- nrow(dataMat)
  
  if (algorithm == 'range'){
    
    minMat   <- matrix(rep(colMins(dataMat), nObs), nrow = nObs, byrow = TRUE)
    maxMat   <- matrix(rep(colMaxs(dataMat), nObs), nrow = nObs, byrow = TRUE)
    dataNorm <- (dataMat - minMat) / (maxMat - minMat)
    dataPars <- list(mins = colMins(dataMat), maxs = colMaxs(dataMat))
    
    
  } else if (algorithm == 'z'){
    
    if (length(pars) == 0) {
      meanMat  <- matrix(rep(colMeans(dataMat), nObs), nrow = nObs, byrow = TRUE)
      sdMat    <- matrix(rep(colSds(dataMat), nObs), nrow = nObs, byrow = TRUE)
    } else {
      meanMat  <- matrix(rep(pars$means, nObs), nrow = nObs, byrow = TRUE)
      sdMat    <- matrix(rep(pars$sds, nObs), nrow = nObs, byrow = TRUE)
    }
    dataNorm <- (dataMat - meanMat) / sdMat
    dataPars <- list(means = colMeans(dataMat), sds = colSds(dataMat))
    
  } else {
    
    return('Invalid algorithm!')
    
  }
  
  dataNorm <- as.data.frame(dataNorm)
  dataNorm$state <- labs
  
  return(list(dataNorm = dataNorm, dataPars = dataPars))
  
}



get.all.data <- function(subs, task, measures, feats, folders, normalize = TRUE){
  
  for (subi in 1:length(subs)){
    
    sub  <- subs[subi]
    temp <- get.data(sub, task, measures, feats, folders)
    
    if (normalize) {
      temp <- normalize(temp, 'z')$dataNorm
    }
    
    if (subi == 1){
      data <- temp
    } else {
      data <- rbind(data, temp)
    }
    
  }
  
  return(data)
  
}



get.data.3bk <- function(sub, task, measures, feats, folders) {
  
  load('trialIdList.rdata')
  
  for (feati in 1:length(feats)){
    
    feat   <- feats[feati]
    folder <- folders[feati]
    
    all <- readMat(paste0(f_measure_matfile, f_sep, folder, f_sep, sub, '.mat'))
    
    for (si in 1:length(states)){
      
      state <- states[si]
      eval(parse(text = paste0('temp <- all$', feat, '.', task, '.', state)))
      
      # filter for 3 preceding trials
      for (sessioni in 1:2){
        temp <- temp[temp[,sessioni] %in% unlist(idList[[sessioni]][[sub]]) | temp[,sessioni] == 0,]
      }
      
      if (feat %in% c('alpha', 'theta')){
        temp <- cbind(temp[, 3:4], si - 1)  # select feats, add lab
      } else {
        temp <- cbind(temp[, 3:5], si - 1)
      }
      
      if (si == 1){
        data <- temp 
      } else {
        data <- rbind(data, temp)
      }
      
    }
    
    if (feat %in% c('alpha','theta')){
      colnames(data) <- c(paste0(measures[feati], '.', c('Base','StimOn')), 'state')
    } else {
      colnames(data) <- c(paste0(measures[feati], '.', c('Size','Time','Scale')), 'state')
      data[data[, 1] == 0, 1] <- NaN  # mark detection failure in single trial ERP
    }
    
    if (feati == 1){
      df <- data
    } else {
      df <- cbind(df, data[, -ncol(data)])
    }
    
  }
  
  df <- as.data.frame(df)
  df$state <- factor(df$state, levels = c(1:length(states)) - 1, labels = states)
  df <- na.omit(df)  # remove feat detection failure trials

  return(df)
  
}



get.trialIdx <- function(sub, task, measures, feats, folders){
  
  for (feati in 1:length(feats)){
    
    feat   <- feats[feati]
    folder <- folders[feati]
    
    all <- readMat(paste0(f_measure_matfile, f_sep, folder, f_sep, sub, '.mat'))
    
    for (si in 1:length(states)){
      
      state <- states[si]
      eval(parse(text = paste0('temp <- all$', feat, '.', task, '.', state)))
      
      if (feati == 1){
        if (feat %in% c('alpha', 'theta')){
          temp <- cbind(temp[, 1:4], si - 1)  # trial idx + feats
        } else {
          temp <- cbind(temp[, 1:5], si - 1)
        }
      } else {
        if (feat %in% c('alpha', 'theta')){
          temp <- cbind(temp[, 3:4], si - 1)  # feats
        } else {
          temp <- cbind(temp[, 3:5], si - 1)
        }
      }
      
      if (si == 1){
        data <- temp 
      } else {
        data <- rbind(data, temp)
      }
      
    }
    
    if (feati == 1) {
      if (feat %in% c('alpha','theta')){
        colnames(data) <- c('ID.s1', 'ID.s2', paste0(measures[feati], '.', c('Base','StimOn')), 'state')
      } else {
        colnames(data) <- c('ID.s1', 'ID.s2', paste0(measures[feati], '.', c('Size','Time','Scale')), 'state')
        data[data[, 3] == 0, 1] <- NaN  # mark the detection failure of single trial ERP
      }
    } else {
      if (feat %in% c('alpha','theta')){
        colnames(data) <- c(paste0(measures[feati], '.', c('Base','StimOn')), 'state')
      } else {
        colnames(data) <- c(paste0(measures[feati], '.', c('Size','Time','Scale')), 'state')
        data[data[, 1] == 0, 1] <- NaN  # mark the detection failure of single trial ERP
      }
    }
    
    if (feati == 1){
      df <- data
    } else {
      df <- cbind(df, data[, -ncol(data)])
    }
    
  }
  
  df       <- as.data.frame(df)
  df$state <- factor(df$state, levels = c(1:length(states)) - 1, labels = states)
  df <- na.omit(df)  # remove feat detection failure trials
  df.idx <- df[,1:2] 
  
  return(df.idx)
}




get.data.content <- function(sub, task, states, contentsets, measures, feats, folders, back3On = FALSE){
  
  if (back3On) {
    load('trialIdList.rdata')
    print('Filter data for 3 preceding trials.')}
  
  # intialize
  df <- matrix(NaN, 0, 0) 
  
  for (feati in 1:length(feats)){
    
    feat   <- feats[feati]
    folder <- folders[feati]
    
    all <- readMat(paste0(f_measure_matfile, f_sep, folder, f_sep, sub, '.mat'))
    
    # intialize
    data <- matrix(NaN, 0, 0) 
    
    for (si in 1:length(states)){
      
      state <- states[si]
      contents <- contentsets[[si]]
      
      # combine lists from different contents
      for (ci in 1:length(contents)){
        
        content <- contents[ci]
        eval(parse(text = paste0('temp <- all$', feat, '.', task, '.', content)))
        
        # convert to df; easy for case of onse obs
        temp <- as.data.frame(temp)  
        
        # if temp is not empty
        if (nrow(temp) > 0){
          
          # filter for 3 preceding trials
          if (back3On) {
            for (sessioni in 1:2){
              temp <- temp[temp[,sessioni] %in% unlist(idList[[sessioni]][[sub]]) | temp[,sessioni] == 0,]
            }
          }
          
          # if the filterd temp is not empty
          if (nrow(temp) > 0) {
            # select feats, add lab
            if (feat %in% c('alpha', 'theta')){
              temp <- cbind(temp[, 3:4], si - 1)  
            } else {
              temp <- cbind(temp[, 3:5], si - 1)
            }
            
            # combined with data in other specified labels
            if (nrow(data) == 0){ # if data is empty
              data <- temp 
            } else {
              data <- rbind(data, temp)
            }
          }
        }
        
      }  # loop over contents

    }  # loop over states
    
    # if data being extracted in the specified labels are not empty
    if (nrow(data) > 0){
      
      # add column names
      if (feat %in% c('alpha','theta')){
        colnames(data) <- c(paste0(measures[feati], '.', c('Base','StimOn')), 'state')
      } else {
        colnames(data) <- c(paste0(measures[feati], '.', c('Size','Time','Scale')), 'state')
        # mark the detection failure of single trial ERP
        data[data[, 1] == 0, 1] <- NaN  
      }
      
      # combined with data of other specified features
      if (feati == 1){
        df <- data
      } else {
        df <- cbind(df, data[, -ncol(data)])
      }
      
    } else { # if no data are extracted in the specified label
      
      df <- as.data.frame(df)  # output empty
      break  # exit the iteration because no data will be extracted for other features as well
      
    }
    
  }  # loop over feats
  
  # convert label type
  df$state <- factor(df$state, levels = c(1:length(states)) - 1, labels = states)
 
  # remove feat detection failure trials 
  df <- na.omit(df)  
  
  return(df)
  
}



get.data <- function(sub, task, measures, feats, folders, splitSessions = FALSE){
  
  for (feati in 1:length(feats)){

    feat   <- feats[feati]
    folder <- folders[feati]
  
    all <- readMat(paste0(f_measure_matfile, f_sep, folder, f_sep, sub, '.mat'))
    if (splitSessions) {sesPos <- c()}  # to store the end position of data in each session
    
    for (si in 1:length(states)){
      
      state <- states[si]
      eval(parse(text = paste0('temp <- all$', feat, '.', task, '.', state)))
      
      if (splitSessions){sesPos <- c(sesPos, max(which(temp[,1]>0)), nrow(temp))}
      
      if (nrow(temp) > 0){
        if (feat %in% c('alpha', 'theta')){
          temp <- cbind(temp[, 3:4], si - 1)  # select feats, add lab
        } else {
          temp <- cbind(temp[, 3:5], si - 1)
        }
      }
      
      if (si == 1){
        data <- temp 
      } else {
        if (nrow(data) > 0){
          if (nrow(temp) > 0){
            data <- rbind(data, temp)
          }  # else, no act
        } else {
          data <- temp
        }
      }
      
    }
    
    if (splitSessions){sesPos[3:4] <- sesPos[3:4] + sesPos[2]}
    
    if (nrow(data) > 0){
      if (feat %in% c('alpha','theta')){
        colnames(data) <- c(paste0(measures[feati], '.', c('Base','StimOn')), 'state')
      } else {
        colnames(data) <- c(paste0(measures[feati], '.', c('Size','Time','Scale')), 'state')
        data[data[, 1] == 0, 1] <- NaN  # mark the detection failure of single trial ERP
      }
      
      if (feati == 1){
        df <- data
      } else {
        df <- cbind(df, data[, -ncol(data)])
      }
    } else {
      df <- data
    }

  }
  
  df       <- as.data.frame(df)
  df$state <- factor(df$state, levels = c(1:length(states)) - 1, labels = states)
  
  if (splitSessions){
    sesOneIdx <- c(1:sesPos[1], (sesPos[2]+1):sesPos[3])
    df.split <- list()
    df.split[[1]] <- df[sesOneIdx, ]
    df.split[[2]] <- df[-sesOneIdx, ]
    df <- df.split
  }
  
  if (is.data.frame(df)){
    df <- na.omit(df)  # remove feat detection failure trials
  } else {
    df[[1]] <- na.omit(df[[1]])
    df[[2]] <- na.omit(df[[2]])
  }
  
  return(df)
  
}



simulate.data <- function(sub, task, measures, measureTypes, hIncreaseOn){
  
  load('trialCount.rdata')
  centers <- c(0, 1)
  sharp   <- 1
  
  eval(parse(text = paste0('trialCount <- trialCount.', task)))
  
  for (si in 1:length(states)){
    state <- states[si]
    
    # get class size
    size <- trialCount[sub, state]
    
    # simlute data
    for (mi in 1:length(measures)){
      
      measure     <- measures[mi]
      measureType <- measureTypes[mi]
      increaseOn  <- hIncreaseOn[mi]
      
      if (state == 'ot'){
        if (increaseOn) {
          center <- min(centers)
        } else {
          center <- max(centers)
        }
      } else {
        if (increaseOn){
          center <- max(centers)
        } else {
          center <- min(centers)
        }
      }
      
      if (toupper(measureType) == 'ERP'){
        temp <- cbind(rnorm(size, mean = center, sd = sharp),
                      rnorm(size, mean = mean(centers), sd = sharp),
                      rnorm(size, mean = mean(centers), sd = sharp))
        colnames(temp) <- paste0(measure, '.', c('Size','Time','Scale'))
      } else {
        temp <- cbind(rnorm(size, mean = center, sd = sharp),
                      rnorm(size, mean = center, sd = sharp))
        colnames(temp) <- paste0(measure, '.', c('Base','StimOn'))
      }
      
      if (mi == 1){
        data <- temp
      } else {
        data <- cbind(data, temp)
      }
 
    }
    
    data <- cbind(data, state = si-1)
    if (si ==  1){
      df <- data
    } else {
      df <- rbind(df, data)
    }
    
  }

  df       <- as.data.frame(df)
  df$state <- factor(df$state, levels = c(1:length(states)) - 1, labels = states)

  return(df)
  
}



simulate.data.random <- function(sub, task, measures, measureTypes, equalSizeOn = FALSE){
  
  center <- 0
  sharp  <- 1
  size.default <- 100
  
  if(is.logical(equalSizeOn) && !equalSizeOn) {
    load('trialCount.rdata')
    eval(parse(text = paste0('trialCount <- trialCount.', task)))
  }
  
  for (si in 1:length(states)){
    state <- states[si]
    
    # get class size
    if (is.logical(equalSizeOn) && !equalSizeOn){
      size <- trialCount[sub, state]
    } else if (is.logical(equalSizeOn) && equalSizeOn) {
      size <- size.default
    } else if (is.numeric(equalSizeOn)){
      size <- equalSizeOn
    } else {
      size <- NaN
    }
    
    
    # simlute data
    for (mi in 1:length(measures)){
      
      measure     <- measures[mi]
      measureType <- measureTypes[mi]
   
      if (toupper(measureType) == 'ERP'){
        temp <- cbind(rnorm(size, mean = center, sd = sharp),
                      rnorm(size, mean = center, sd = sharp),
                      rnorm(size, mean = center, sd = sharp))
        colnames(temp) <- paste0(measure, '.', c('Size','Time','Scale'))
      } else {
        temp <- cbind(rnorm(size, mean = center, sd = sharp),
                      rnorm(size, mean = center, sd = sharp))
        colnames(temp) <- paste0(measure, '.', c('Base','StimOn'))
      }
      
      if (mi == 1){
        data <- temp
      } else {
        data <- cbind(data, temp)
      }
      
    }
    
    data <- cbind(data, state = si-1)
    if (si ==  1){
      df <- data
    } else {
      df <- rbind(df, data)
    }
    
  }
  
  df       <- as.data.frame(df)
  df$state <- factor(df$state, levels = c(1:length(states)) - 1, labels = states)
  
  return(df)
  
}