source("Monthly graph/Temperature M.R")
PLOTT(startDay =  "2005-03-01 00", endDay =  "2008-08-01 00", numberYear =   10, station = "Napier")

source("Monthly graph/Radiation M.R")
PLOTR(startDay =  "2005-03-01 00", endDay =  "2008-08-01 00", numberYear =   10, station = "Napier")

source("Monthly graph/Rainfall M.R")
PLOTP(startDay =  "2009-03-01 00", endDay =  "2013-08-01 00", numberYear =   10,station = "Napier")

library(grid)
library(gridExtra)
p4 <- arrangeGrob(p1, p2, p3,  nrow = 2,
                  main = textGrob("Monthly rainfall, temperature and radiation", 
                                  just = "top", vjust = 0.75, 
                                  gp = gpar(fontsize = 14, lineheight = 1, 
                                            fontface = "bold")))
                          
grid.newpage()
grid.draw(p4,recording = T)
