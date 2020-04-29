
typeSets <- list(1:2, c(3,5,6))
triggerSets <- list(list(c(10, 20, 15, 25), c(30, 50, 60, 35, 55, 65)),
                    list(c(13, 23), c(33, 53, 63)))
colors <- c('#87CEEB', '#FF6347')
scolors <- colors
names(scolors) <- c('ot', 'mw')

measures <- c('fAlpha', 'pALpha', 'loAlpha', 'roAlpha', 'fTheta', 'pTheta', 'loTheta', 'roTheta', 
              'loP1', 'roP1', 'loN1', 'roN1', 'P3', 
              'pfCohAlpha', 'ploCohAlpha', 'proCohAlpha', 'loroCohAlpha', 'lofCohAlpha', 'rofCohAlpha',
              'pfCohTheta', 'ploCohTheta', 'proCohTheta', 'loroCohTheta', 'lofCohTheta', 'rofCohTheta')
feats    <- c('alpha', 'alpha', 'alpha', 'alpha', 'theta', 'theta', 'theta', 'theta',
              'p1', 'p1', 'n1', 'n1', 'p3',
              'alpha', 'alpha', 'alpha', 'alpha', 'alpha', 'alpha',
              'theta', 'theta', 'theta', 'theta', 'theta', 'theta')
folders  <- c('alpha chan85', 'alpha chan19', 'alpha chan10', 'alpha chan39', 
              'theta chan85', 'theta chan19', 'theta chan10', 'theta chan39', 
              'p1 chan10', 'p1 chan39', 'n1 chan10', 'n1 chan39', 'p3 chan19',
              'ispc alpha chan19-85', 'ispc alpha chan10-19', 'ispc alpha chan19-39', 
              'ispc alpha chan10-39', 'ispc alpha chan10-85', 'ispc alpha chan39-85',
              'ispc theta chan19-85', 'ispc theta chan10-19', 'ispc theta chan19-39', 
              'ispc theta chan10-39', 'ispc theta chan10-85', 'ispc theta chan39-85')
measureNames <- c('alpha C21', 'alpha A19', 'alpha A10', 'alpha B7', 
                  'theta C21', 'theta A19', 'theta A10', 'theta B7', 
                  'P1 A10', 'P1 B7', 'N1 A10','N1 B7', 'P3 A19',
                  'alpha A19-C21', 'alpha A19-A10', 'alpha A19-B7',
                  'alpha A10-B7',  'alpha A10-C21', 'alpha B7-C21',
                  'theta A19-C21', 'theta A19-A10', 'theta A19-B7',
                  'theta A10-B7',  'theta A10-C21', 'theta B7-C21')
measureTypes <- c('power', 'power', 'power', 'power',
                  'power', 'power', 'power', 'power',
                  'ERP', 'ERP', 'ERP', 'ERP', 'ERP',
                  'ISPC', 'ISPC', 'ISPC', 'ISPC', 'ISPC', 'ISPC', 
                  'ISPC', 'ISPC', 'ISPC', 'ISPC', 'ISPC', 'ISPC')
measureColors <- c("#FF0000", '#0000ff', '#00ff00', '#e1e11a',
                   "#FF0000", '#0000ff', '#00ff00', '#e1e11a',
                   '#00ff00', '#e1e11a', '#00ff00', '#e1e11a', '#0000ff',
                   '#7f007f', "#007f7f", "#7f7f7f", "#007f00", "#7f7f00", "#ff7f00", 
                   '#7f007f', "#007f7f", "#7f7f7f", "#007f00", "#7f7f00", "#ff7f00") 
measureShapes <- c(rep(21,8), rep(22,5), rep(24,12))
# measureShaps <- c(rep(1,4), rep(2,4), rep(3,5), rep(1,6), rep(2,6))

hIncreaseOn <- as.logical(c(0, rep(1, 3), rep(0, 4 + 5 + 6 * 2)))
