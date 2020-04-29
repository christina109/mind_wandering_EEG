library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)



myTheme <- theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(axis.text = element_text(size = 16)) +
  theme(title = element_text(size = 18))

get.mytheme <- function() {
  return(myTheme)
}



plot.point.flip <- function(df.sum, valName, condName, groupName, errorValName = NaN, colorName = NaN, shape = NaN, ylim = c(), yintercep = NaN){
  
  eval(parse(text = paste0('p <- ggplot(df.sum, aes(', groupName, ',', valName, ', shape = ',condName, ', color = ', groupName, '))')))
  p <- p + geom_point(size = 3)
  
  if (!(is.na(errorValName))){
    eval(parse(text = paste0('p <- p + geom_errorbar(aes(ymin = ', valName, ' - ', errorValName,
                             ', ymax = ', valName, ' + ', errorValName, '), width = 0.5)')))
  }
  
  if (!is.na(colorName)){
    eval(parse(text = paste0("p <- p + scale_color_manual(values = df.sum$", colorName, ")")))
  }
  
  if (is.vector(na.omit(shape))){
    p <- p + scale_shape_manual(values = shape)
  }
 
  if (!is.na(yintercep)){
    p <- p + geom_hline(aes(yintercept = yintercep), linetype = 2)
  }
  
  if (!is.null(ylim)){
    p <- p + myTheme + coord_flip(ylim = ylim)
  } else {
    p <- p + myTheme + coord_flip()
  }
 
  return(p)
  
}



plot.trend <- function(df, xName, yName, condName, shapeByCond = TRUE, errorValName = NaN, ylim = c(), ybreaks = c()){
  
  if (shapeByCond){
    eval(parse(text = paste0('p <- ggplot(df, aes(', xName, ',', yName, ', color =', xName, 
                             ', shape =', condName, ', group =', condName, '))')))
    p <- p + geom_point(size = 4) + geom_line(size = 1, alpha = 0.5, linetype = 2, color = "grey")
  } else {
    eval(parse(text = paste0('p <- ggplot(df, aes(', xName, ',', yName, ', color =', condName,
                             ', group =', condName, '))')))
    p <- p + geom_point(size = 4, alpha = 0.5) + geom_line(size = 1, alpha = 0.5, linetype = 2)
  }
  

  if (!(is.na(errorValName))){
    eval(parse(text = paste0('p <- p + geom_errorbar(aes(ymin = ', yName, ' - ', errorValName,
                             ', ymax = ', yName, ' + ', errorValName, '), width = 0.1)')))
  }
  
  if (!is.null(ylim)){
    if (!is.null(ybreaks)){
      p <- p + scale_y_continuous(breaks = ybreaks, limits = ylim)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks, limits = ylim)
    }
  }
  
  p <- p + myTheme
  return(p)
  
}



plot.point <- function(df, xName, yName, condName, errorValName = NaN, ylim = c()){
  
  eval(parse(text = paste0('p <- ggplot(df, aes(', xName, ',', yName, ', color =', condName, 
                           ', shape =', condName, ', group =', condName, '))')))
  p <- p + geom_point(size = 4) + geom_line(size = 1, alpha = 0.5) 

  
  if (!(is.na(errorValName))){
    eval(parse(text = paste0('p <- p + geom_errorbar(aes(ymin = ', yName, ' - ', errorValName,
                             ', ymax = ', yName, ' + ', errorValName, '), width = 0.1)')))
  }
  
  if (!is.null(ylim)){
    p <- p + coord_cartesian(ylim = ylim)
  }
  
  p <- p + myTheme
  return(p)
  
}



plot.bar <- function(df.sum, valName, condName = NaN, groupName, errorValName = NaN, ylim = c(), autoColor = TRUE){
  
  barWidth <- 0.6
  posn.d <- position_dodge(barWidth)
  colorDiff <- paste0("#",as.hexmode(substr(colors[2],2,7)) - as.hexmode(substr(colors[1],2,7)))
  
  if (!is.na(condName)){
    
    eval(parse(text = paste0('p <- ggplot(df.sum, aes(', groupName, ',', valName, ', fill = ',condName, '))')))
    p <- p + geom_bar(stat = 'identity', position = 'dodge', width = barWidth)
    
  } else {
    
    eval(parse(text = paste0('p <- ggplot(df.sum, aes(', groupName, ',', valName, '))')))
    if (autoColor) {
      p <- p + geom_bar(stat = 'identity', position = 'dodge', width = barWidth, color = 'black', fill = colorDiff, size = 1)
    } else {
      p <- p + geom_bar(stat = 'identity', position = 'dodge', width = barWidth, color = 'black', size = 1)
    }
    
  }
  
  if (!(is.na(errorValName))){
    eval(parse(text = paste0('p <- p + geom_errorbar(aes(ymin = ', valName, ' - ', errorValName, ', ymax = ', valName, ' + ', errorValName, 
                             '), width = barWidth * 0.6, position = posn.d, size = 1)')))
  }
  
  if (autoColor){
    if (!is.nan(condName)){
      p <- p + scale_fill_manual(values = colors)
    } 
  }
  
  if (!is.null(ylim)){
    p <- p + coord_cartesian(ylim = ylim)
  }
  
  
  p <- p + myTheme
  return(p)
  
}



plot.ci95 <- function(df, valName, condName, groupName, ylim = c(), subName = c(), autoColor = TRUE){
  
  posn.d <- position_dodge(0.6)
  
  if (is.null(subName)){
    eval(parse(text = paste0('p <- ggplot(df, aes(', groupName, ',', valName, ', color = ',condName, '))')))
  } else {
    eval(parse(text = paste0('p <- ggplot(df, aes(', groupName, ',', valName, ', color = ',condName, ', label = ', subName, '))')))
  }
  
  p <- p + 
       geom_point(position = posn.d, size = 4, alpha = 0.3) +
       stat_summary(fun.data = "mean_cl_normal", geom = "crossbar", fatten = 1.5, width = 0.5, position = posn.d, size = 1)
  
  if (!is.null(subName)) {
    eval(parse(text = paste0('p <- p + geom_text(aes(label = ', subName, '), hjust = -0.5, vjust = -0.5, size = 3, position = posn.d)')))
  }
  
  if (autoColor){
    p <- p + scale_color_manual(values = colors)
  }
  
  
  if (!is.null(ylim)){
    p <- p + coord_cartesian(ylim = ylim)
  }
  
  
  p <- p + myTheme
  return(p)
  
}



plot.freq.change <- function(subs, tasks, states, contentsets, feats2plot, normalizeOn = FALSE, 
                             f_output = NaN) {
  print('Reading feature data...')
  feats.val <- get.feat.val(subs, tasks, states, contentsets, feats2plot, normalizeOn)
  df <- arrangeData2plot(feats.val, se = 'se', separateNames = c('marker', 'measure'))
  
  # subset for band frequency features
  feats.freq <- measures[measures %in% feats2plot & measureTypes != 'ERP']
  feats2plot <- feats2plot[feats2plot %in% feats.freq]  # not change the input feats order
  
  # get plot arrangement 
  arrangement <- get.plot.arrangement(length(feats2plot))
  
  plist <- list()
  for (feati in 1:length(feats2plot)) {
    print(paste0('Generating feature graph ', feati, ' out of ', length(feats2plot), '...'))
    feature <- feats2plot[feati]
    temp <- df[df$marker == feature,]
    p <- plot.point(temp, 'measure', 'mean', 'state', 'se')
    p <- p + labs(title = measureNames[measures == feature], x = '', y = 'z-score')
    p <- p + scale_x_discrete(labels = c('base', 'ASO'))
    p <- p + scale_color_manual(values = c(scolors['mw'], scolors['ot']), breaks = c('mw', 'ot'), labels = c('MW', 'OT'))
    p <- p + scale_shape_manual(values = c(16, 17), breaks = c('mw', 'ot'), labels = c('MW', 'OT'))
    p <- p + theme(axis.text = element_text(size = 24))
    plist[[feati]] <- p 
  }
  parr <- ggarrange(plotlist = plist, ncol = arrangement[2], nrow = arrangement[1], common.legend = TRUE)
  if (!is.na(f_output)) {
    ggsave(paste0(f_output, '.png'), device = 'png', plot = parr, path = f_result, dpi = 300, width = 8*arrangement[2], height = 8*arrangement[1], units = 'cm')
  }
  return(parr)
}



arrangeData2plot <- function(df, sumData = TRUE, separateNames = c(), sep = NaN, varRank = list(), se = 'se', confidence = 0.95){
  
  # se options: (default) 'se', 'ci', 'se-within'
  
  # pars
  alpha <- 1- confidence
  cols2exclude <- c('sub', 'task', 'state') 
  cols2group <- c('task', 'state')
  
  df <- data.frame(df)
  
  if (!'sub' %in% colnames(df)) { stop('Sub is required to compute SE!') }
  subs <- unique(df$sub)
  
  base <- 'df <- gather(df, key = key, val = val'
  
  for (ci in 1:length(cols2exclude)) {
    col2ex <- cols2exclude[ci]
    if (col2ex %in% colnames(df)) {base <- paste0(base, ', -', col2ex)}
  }
  eval(parse(text = paste0(base, ')')))  # execute gather()
  
  if (sumData) {

    if (se == 'se') {
      base <- 'df.sum <- group_by(df, key'
      for (ci in 1:length(cols2group)) {
        column <- cols2group[ci] 
        if (column %in% colnames(df)) {base <- paste0(base, ', ', column)}
      }
      eval(parse(text = paste0(base, ')')))
      df.sum <- summarise(df.sum, mean = mean(val), se = sd(val)/sqrt(length(subs)-1))
      
    } else if (se == 'ci') {
      base <- 'df.sum <- group_by(df, key'
      for (ci in 1:length(cols2group)) {
        column <- cols2group[ci] 
        if (column %in% colnames(df)) {base <- paste0(base, ', ', column)}
      }
      eval(parse(text = paste0(base, ')')))
      df.sum <- summarise(df.sum, mean = mean(val), ci = sd(val)/sqrt(length(subs)-1) * qnorm(1-alpha/2))
      
    } else if (se == 'se-within') {
      
      im <- summarise(group_by(df, sub, key), mean = mean(val))
      gm <- summarise(group_by(df, key), mean = mean(val))
      features <- unique(df$key)
      for (subi in 1:length(subs)) {
        sub <- subs[subi]
        for (feati in 1:length(features)) {
          feature <- features[feati]
          val2change <- df[df$sub == sub & df$key == feature, 'val']
          change <- - im[im$sub == sub & im$key == feature, 'mean'] + gm[gm$key == feature, 'mean']
          df[df$sub == sub & df$key == feature, 'val.corr'] <- val2change + rep(as.numeric(change), length(val2change))
        }
      }
      
      base <- 'df.sum <- group_by(df, key'
      for (ci in 1:length(cols2group)) {
        column <- cols2group[ci] 
        if (column %in% colnames(df)) {base <- paste0(base, ', ', column)}
      }
      eval(parse(text = paste0(base, ')')))
      df.sum <- summarise(df.sum, mean = mean(val), se.within = sd(val.corr)/sqrt(length(subs)-1))

    } else {
      stop('Invalid error type!')
    }
    
    df <- df.sum
  } 
  
  if (!is.null(separateNames)) {
    if (!is.nan(sep)) {
      df <- separate(df, key, separateNames, sep)
    } else {
      df <- separate(df, key, separateNames)
    }
  }
  
  if (length(varRank) > 0) {
    for (vari in 1:length(varRank)){
      var <- separateNames[vari]
      eval(parse(text = paste0('df$', var, ' <- ', 'factor(df$', var, ', levels = varRank[[vari]])')))
    }
  }

  return(df)
  
}



get.sig.feats <- function(df.sum) {
  if (sum(c('key', 'sig') %in% colnames(df.sum)) < 2) {
    stop('The input df.sum must contain a key and a sig column.')
  }
  sigfeats <- filter(df.sum, sig != '')$key
  sigfeats <- as.character(sigfeats)
  return(sigfeats)
}



get.feat.val <- function(subs, tasks, states, contentsets, feats2get, normalizeOn = FALSE) {

  # merging data from different tasks
  
  fidx <- which(measures %in% feats2get)
  
  # intialize
  df <- data.frame(sub = subs)
  
  for (subi in 1:length(subs)) {
    sub <- subs[subi]
    for (taski in 1:length(tasks)) {
      task <- tasks[taski]
      
      print(paste0('Read data ', sub, ' of task ', task, '...'))
      temp <- get.data.content(sub, task, states, contentsets, measures[fidx], feats[fidx], folders[fidx])
      if (normalizeOn) {temp <- normalize(temp, 'z')$dataNorm }
      
      # merge data from different tasks
      if (taski == 1) {
        data <- temp
      } else {
        print(paste0('Merge data ', sub, ' from multiple tasks...'))
        data <- rbind(data, temp)
      }
    }
    
    # compute mean & register
    for (statei in 1:length(states)) {
      state <- states[statei]
      
      print(paste0('Compute mean scores for data ', sub, ' in state ', state, '...'))
      temp  <- data[data$state == state,]
      temp$state <- NULL
      
      # intialize
      if (subi == 1 && statei == 1) {
        df <- matrix(0, length(subs)*length(states), ncol(temp) + 2)
        colnames(df) <- c(colnames(temp), 'sub', 'state')
        df <- as.data.frame(df)
      } 
      
      # register
      print('Register as a new row.')
      df[(subi - 1)*length(states) + statei, 1:ncol(temp)] <- colMeans(temp)
      df[(subi - 1)*length(states) + statei, 'sub']   <- sub
      df[(subi - 1)*length(states) + statei, 'state'] <- state
      
    }  # loop over states
    
    print(paste0(round(subi / length(subs)), '% has finished.'))

  }  # loop over subs

  return(df)
}



get.plot.arrangement <- function(nplots) {
  if (nplots < 4) {
    nrow <- 1
    ncol <- nplots
  } else if (nplots == 4) {
    nrow <- 2
    ncol <- 2
  } else if (nplots <= 9) {
    ncol <- 3
  } else if (nplots <= 16){
    ncol <- 4
  } else if (nplots <= 25){
    ncol <- 5
  } else if (nplots <= 36){
    ncol <- 6
  }
  nrow <- ceiling(nplots/ncol) 
  return(c(nrow, ncol))
}