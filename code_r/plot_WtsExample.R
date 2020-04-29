library(ggplot2)
library(plotly)

setwd('c:\\topic_mind wandering\\3data\\')

x <- 446.3
y <- 1188.5
w <- 39.4

margin <- unit(c(1,1,1,1), 'line')



plot.contourExample <- function() {
  
  load('code_r\\WtsExample.rdata')
  
  f <- list(size = 30)
  
  m <- list(
    l = 150, r = 50, b = 80, t = 50
  )
  
  a <- list(
    x = x, y = y,
    text = 'local maximun',
    showarrow = TRUE, 
    arrowhead = 7,
    ax = 20,
    ay = -40, 
    font = list(size = 20)
  )
  
  xaxis <- list(
    autotick = FALSE,
    range = c(0, 1000),
    dtick = 500,
    title = 'Time lag [ms]', titlefont = f
  )
  
  yaxis <- list(
    autotick = FALSE,
    range = c(0, 2400),
    dtick = 1000,
    title = 'Scale [ms]', titlefont = f
  )
  
  p <- plot_ly(type = 'contour',
               z = Wts, 
               x = seq(1, 1000, 1000/256), 
               y = seq(1, 2400, 1000/256)
               ) %>% layout(
                 title = 'W-value contour map ', font = list(size = 20), 
                 xaxis = xaxis,
                 yaxis = yaxis,
                 annotations = a,
                 margin = m
               )

  return(p)
}



plot.trialExample <- function() {
  
  load('code_r\\trialExample.rdata')
  p <- ggplot(trial, aes(V1, V2)) +
    geom_line(size = 1) +
    theme_classic() + 
    labs(title = 'A single trial EEG', x = 'Time [ms]', 
         y = expression(paste('Amplitude [', mu, 'V]'))) +
    theme(axis.line = element_line(size = 1), 
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18, hjust = 0.5),
          plot.margin = margin)
  return(p)
  
}



plot.mexicanHat <- function(times = -400:1200, t = x, s = y) {
  
  y <- get.mexicanHat(times, t, s) 
  p <- ggplot(data.frame(x = times, y = y), aes(x, y) ) + 
    geom_line(size = 1) +
    theme_classic() +
    labs(title = 'Mexican hat wavelet', x = '', y = '', size = 18) +
    theme(axis.line = element_line(size = 1), 
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18, hjust = 0.5),
          plot.margin = margin)
  return(p)
  
}



get.mexicanHat <- function(times, t, s) {
  
  x   <- (times-t)/s
  phi <- (1-16*x*x)*exp(-8*x*x)
  return(phi)
  
}