


library("jmcm")

# 3.1. Analysis of a balanced longitudinal dataset
data("cattle", package = "jmcm")
head(cattle)

library("lattice")
xyplot(weight ~ day | group, 
          group = id, 
          data = cattle, 
          xlab = "days",
          ylab = "weight", 
          col = 1, 
          type = "l")


cattleA <- cattle[cattle$group=='A', ]
fit1 <- jmcm(weight | id | I(day/14 + 1) ~ 1 | 1,
             data = cattleA,
             triple = c(8, 3, 4), 
             cov.method = 'mcd')
fit1
regressogram(fit1, time = 1:11)

fit2 <- jmcm(weight | id | I(day/14 + 1) ~ 1 | 1, 
             data = cattleA,
             triple = c(8, 3, 4), 
             cov.method = 'acd')
fit2
regressogram(fit2, time = 1:11)

fit3 <- jmcm(weight | id | I(day/14 + 1) ~ 1 | 1, 
             data = cattleA,               
             triple = c(8, 2, 2), 
             cov.method = 'hpc')
fit3
regressogram(fit3, time = 1:11)

# 3.2. Analysis of an unbalanced longitudinal dataset

data("aids", package = "jmcm")
head(aids)

library("lattice")
xyplot(sqrt(cd4) ~ time, data = aids,
          panel = function(x, y, ...) {
            panel.xyplot(x, y, ...)
            panel.lines(x[aids$id==10002], y[aids$id==10002], col = 2, lwd = 2)
            panel.lines(x[aids$id==10005], y[aids$id==10005], col = 3, lwd = 2)
            panel.lines(x[aids$id==10029], y[aids$id==10029], col = 4, lwd = 2)
            panel.lines(x[aids$id==10039], y[aids$id==10039], col = 5, lwd = 2)
            panel.lines(x[aids$id==10048], y[aids$id==10048], col = 6, lwd = 2)
            panel.lines(x[aids$id==10052], y[aids$id==10052], col = 7, lwd = 2)
          },
          xlab = "Time", 
       ylab = "CD4 cell numbers", 
       col = 1)
  
fit4 <- jmcm(I(sqrt(cd4)) | id | time ~ 1 | 1, 
             data = aids,        
             triple = c(8, 1, 3), 
             cov.method = 'mcd')
fit4
bootcurve(fit4, nboot = 1000)

fit5 <- jmcm(I(sqrt(cd4)) | id | time ~ 1 | 1, 
             data = aids,
             triple = c(8, 1, 3), 
             cov.method = 'acd')
fit5
bootcurve(fit5, nboot = 1000)

fit6 <- jmcm(I(sqrt(cd4)) | id | time ~ 1 | 1, 
             data = aids,
             triple = c(8, 1, 1), 
             cov.method = 'hpc')
fit6
bootcurve(fit6, nboot = 1000)



