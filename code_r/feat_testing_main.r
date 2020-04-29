featTesting <- function(subs, tasks, mtype, validType, parMat,
                        taskMerge, useParMat, 
                        states = FALSE, contentsets = FALSE, 
                        gridSearchOn = FALSE, searchBy = '', parallelProcId = 0) {
# can only be performed on successfully modelled data
  
  if (mtype == 'svm' && gridSearchOn){
    cPowerVec <- -5:15
    gPowerVec <- -15:3
    
    parMat <- matrix(0, length(subs), length(feats) * 2)
    colnames(parMat) <- paste0(rep(measures, each = 2) , '_',  c('c', 'gamma'))
  }
  
  if (taskMerge) {
    
    accPerFeat <- matrix(0, length(subs), length(feats))
    colnames(accPerFeat) <- measures
    rownames(accPerFeat) <- subs
    
    n  <- length(subs) * length(feats)
    ti <- 0
    if (!is.numeric(parallelProcId)) {
      pb0 <- tkProgressBar(title = 'Testing each feature', min = 0, max = n, width = 300)
    } else {
      pb0 <- tkProgressBar(title = paste0('Testing each feature', parallelProcId), min = 0, max = n, width = 300)
    }
    
    for (subi in 1:length(subs)){
      
      sub <- subs[subi]
      if (mtype == 'svm' && !gridSearchOn && useParMat){
        if (ncol(parMat) > 2){
          parList1 <- list(c = parMat[subi,'sart_c'], gamma = parMat[subi, 'sart_gamma'])
          parList2 <- list(c = parMat[subi,  'vs_c'], gamma = parMat[subi,   'vs_gamma'])
        } else {
          parList <- list(c = parMat[subi, 'c'], gamma = parMat[subi, 'gamma'])
        }
      } else {
        print('Grid search is off and no specified pars. Use default pars to model!')
        parList <- list()
      }
      tic(paste('Testing each feature on sub', sub))
      
      for (feati in 1:length(feats)){
        
        print(paste0('Test marker ', measureNames[feati]))
        for (taski in 1:length(tasks)){
          task <- tasks[taski]
          
          if (is.logical(states) && !states){
            temp <- get.data(sub, task, measures[feati], feats[feati], folders[feati])
          } else {
            temp <- get.data.content(sub, task, states, contentsets, measures[feati], feats[feati], folders[feati])
          }
   
          temp <- normalize(temp, 'z')$dataNorm
          if (taski == 1){
            data <- temp  # intialize
          } else {
            data <- rbind(data, temp)
          }
        }  # over tasks
        
        
        # get parList, fit models
        if (mtype == 'svm' && !gridSearchOn && useParMat){ # grid search off, parMat specified
          if (ncol(parMat) > 2){
            if (validType == 'loo'){
              perf1 <- leave.one.out(data, mtype, parList = parList1, balanced = 'copy')
              perf2 <- leave.one.out(data, mtype, parList = parList2, balanced = 'copy')
              
            } else {
              perf1 <- cross.validation(data, mtype, parList = parList1, balanced = 'copy')
              perf2 <- cross.validation(data, mtype, parList = parList2, balanced = 'copy')
            }
          } else {
            if (validType == 'loo'){
              perf <- leave.one.out(data, mtype, parList = parList, balanced = 'copy')
            } else {
              perf <- cross.validation(data, mtype, parList = parList, balanced = 'copy')
            }
          }

        } else if (mtype == 'svm' && gridSearchOn){ # grid search on
          
          mat <- grid.search.svm(data, 2^cPowerVec, 2^gPowerVec, validType = 'cv', searchBy = searchBy)
          # fail to do grid search?
          if (!is.matrix(mat) && is.nan(mat)){
            print('Fail to do grid search. Use default parameters to fit model!')
            parMat[subi, c(1:2) + (feati - 1)*2] <- NaN
            parList <- list()
          } else {
            pos <- which.localMax(mat, gPowerVec, cPowerVec)
            parMat[subi, c(1:2) + (feati - 1)*2] <- 2^pos
            parList <- list(c = parMat[subi, paste0(measures[feati], '_c')], gamma = parMat[subi, paste0(measures[feati], '_gamma')])
          }
          
          if (validType == 'loo'){
            perf <- leave.one.out(data, mtype, parList = parList) # [default] balanced = 'copy' for the training split
          } else {
            perf <- cross.validation(data, mtype, parList = parList) # [default] balanced = 'copy' for the training split
          }
          
        } else { # grid search off, no parMat specified
          if (validType == 'loo'){
            perf <- leave.one.out(data, mtype, balanced = 'copy')
          } else {
            perf <- cross.validation(data, mtype, balanced = 'copy')
          }
        }  # end of model fitting
        
        if (mtype == 'svm' && !gridSearchOn && useParMat && ncol(parMat) > 2){
          accPerFeat[subi, feati] <- max(perf1$accuracy, perf2$accuracy)
        } else {
          accPerFeat[subi, feati] <- perf$accuracy
        }
        
        if (!is.numeric(parallelProcId)) {
          f_temp <- 'temp_featTesting.rdata'
        } else {
          f_temp <- paste0('temp_featTesting', parallelProcId, '.rdata')
        }
        
        print(paste0('Autosave in ', f_temp))
        if (mtype == 'svm' && gridSearchOn){
          save(file = f_temp, accPerFeat, parMat)
        } else {
          save(file = f_temp, accPerFeat)
        }
        
        ti <- ti + 1
        setTkProgressBar(pb0, ti, label = paste(round(ti/n * 100, 1), "% done"))
        
      }
      
      toc()
      
    }
    close(pb0)
    
  } else { # task-separate
    
    accPerFeat <- matrix(0, length(subs), length(feats))
    colnames(accPerFeat) <- measures
    rownames(accPerFeat) <- subs
    
    for (taski in 1:length(tasks)){
      eval(parse(text = paste0('accPerFeat.', tasks[taski], ' <- accPerFeat')))
    }
    
    for (taski in 1:length(tasks)){
      
      task <- tasks[taski]
      
      n <- length(subs) * length(feats)
      ti <- 0
      pb0 <- tkProgressBar(title = paste('Testing each feature of', task), min = 0, max = count, width = 300)
      
      for (subi in 1:length(subs)){
        
        sub <- subs[subi]
        tic(paste('Testing each feature of', task, 'on subject', sub))
        
        for (feati in 1:length(feats)){
          
          if (is.logical(states) && !states){
            data <- get.data(sub, task, measures[feati], feats[feati], folders[feati])
          } else {
            data <- get.data.content(sub, task, states, contentsets, measures[feati], feats[feati], folders[feati])
          }
          
          data <- normalize(data, 'z')$dataNorm
          
          if(useParMat) {
            parList <- list(c = parMat[subi, paste0(task, '_c')], gamma = parMat[subi, paste0(task, '_gamma')])
          } else {
            parList = list()
          }
          
          if (validType == 'loo'){
            perf <- leave.one.out(data, mtype, parList = parList, balanced = 'copy' )
          } else {
            perf <- cross.validation(data, mtype, parList = parList, balanced = 'copy')
          }
          
          eval(parse(text = paste0('accPerFeat.', task, '[subi, feati] <- perf$accuracy')))
          
          if (parallelProcId == 0) {
            f_temp = 'temp_featTesting.rdata'
          } else {
            f_temp = paste0('temp_featTesting', parallelProcId, '.rdata')
          }
          save(file = f_temp, list = paste0('accPerFeat.', tasks))
          print(paste0('Autosave in ', f_temp))
          
          ti <- ti + 1
          setTkProgressBar(pb0, ti, label = paste(round(ti/n * 100, 1), "% done"))
        }
        toc()
      }
      
      close(pb0)
    }
  }
  
}