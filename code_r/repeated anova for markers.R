library(ez)
feats.val <- get.feat.val(subs, tasks, states, contentsets, feats2plot, normalizeOn)
df <- arrangeData2plot(feats.val, se = 'se-within', separateNames = c('marker', 'measure'), sumData = FALSE)
for (mi in 1:length(measures)) {
  mi <- mi + 1
  marker <- measures[mi]
  type <- measureTypes[mi]
  if (type != 'ERP') {
    temp <- df[df$marker == marker,]
    temp$sub <- as.factor(temp$sub)
    temp$state <- as.factor(temp$state)
    temp$measure <- as.factor(temp$measure)
    model <- ezANOVA(data = temp, dv = val, wid = sub, within = .(state, measure), detailed = TRUE)
    print(mi)
    print(paste0('Repeated ANOVA on ', measureNames[mi]))
    print(model$ANOVA)
    ezPlot(data = temp, dv = val, wid = sub, within = .(state, measure), x = .(measure), do_bars = TRUE, do_lines = FALSE)
  }

}