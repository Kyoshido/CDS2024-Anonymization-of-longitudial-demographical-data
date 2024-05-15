library(LongCART)

help(LongCART)


#--- Get the data
data(ACTG175)
# https://search.r-project.org/CRAN/refmans/BART/html/ACTG175.html

#-----------------------------------------------#
#   model: cd4~ time + subject(random)          #
#-----------------------------------------------#

#--- Run LongCART()  
gvars=c("gender", "wtkg", "hemo", "homo", "drugs",
        "karnof", "oprior", "z30", "zprior", "race",
        "str2", "symptom", "treat", "offtrt")
tgvars=c(0, 1, 0, 0, 0,
         1, 0, 0, 0, 0,
         0, 0, 0, 0)


out1<- LongCART(data=ACTG175, 
                patid="pidnum", fixed=cd4~time,
                gvars=gvars, tgvars=tgvars, alpha=0.05,
                minsplit=100, minbucket=50, coef.digits=2)

#--- Plot tree
par(mfrow=c(1,1))
par(xpd = TRUE)
plot(out1, compress = TRUE)
text(out1, use.n = TRUE)

#--- Plot longitudinal profiles of subgroups
ProfilePlot(x=out1, timevar="time")

#-----------------------------------------------#
#   model: cd4~ time+ time^2 + subject(random)  #
#-----------------------------------------------#

ACTG175$time2<- ACTG175$time^2

out2<- LongCART(data=ACTG175, patid="pidnum", fixed=cd4~time + time2,
                gvars=gvars, tgvars=tgvars, alpha=0.05,
                minsplit=100, minbucket=50, coef.digits=2)


par(mfrow=c(1,1))
par(xpd = TRUE)
plot(out2, compress = TRUE)
text(out2, use.n = TRUE)

ProfilePlot(x=out2, timevar="time", timevar.power=c(1,2))


#--------------------------------------------------------#
#   model: cd4~ time+ time^2 + subject(random) + karnof  #
#--------------------------------------------------------#

out3<- LongCART(data=ACTG175, patid="pidnum", fixed=cd4~time + time2 + karnof,
                gvars=gvars, tgvars=tgvars, alpha=0.05,
                minsplit=100, minbucket=50, coef.digits=2)


par(mfrow=c(1,1))
par(xpd = TRUE)
plot(out3, compress = TRUE)
text(out3, use.n = TRUE)

#the value of the covariate karnof is set at median by default
ProfilePlot(x=out3, timevar="time", timevar.power=c(1,2, NA)) 

#the value of the covariate karnof is set at 120
ProfilePlot(x=out3, timevar="time", timevar.power=c(1,2, NA), 
            covariate.val=c(NA, NA, 120)) 
