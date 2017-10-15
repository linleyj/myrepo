source("Daily graph/Temperature D.R")
DPLOTT(startDay =  "2005-03-01 00", endDay =  "2008-08-01 00", numberYear =   10, station = "3925")

source("Daily graph/Radiation D.R")
DPLOTP(startDay =  "2012-03-01 00", endDay =  "2013-08-01 00", numberYear =   10, station = "3925")

source("Daily graph/Rainfall D.R")
DPLOTR(startDay =  "2009-03-01 00", endDay =  "2013-08-01 00", numberYear =   10, station = "3925")

library(grid)
library(gridExtra)
P4 <- arrangeGrob(P1, P2, P3,  nrow = 2,
                  main = textGrob("Daily rainfall, temperature and radiation", 
                                  just = "top", vjust = 0.75, 
                                  gp = gpar(fontsize = 14, lineheight = 1, 
                                            fontface = "bold")))

grid.newpage()
grid.draw(P4,recording = T)

install.packages("ggmap")
