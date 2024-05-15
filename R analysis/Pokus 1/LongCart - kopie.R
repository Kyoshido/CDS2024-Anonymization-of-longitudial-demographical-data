library("LongCART")

help(LongCART)


#--- Get the data
load("36422-0001-Data.rda")
df <- da36422.0001
rm(da36422.0001)

str(df)

df_sorted <- df[order(df$PAT_ID), ]
result <- cbind(df_sorted$PAT_ID, df_sorted$MARITAL)
df$PAT_ID[which(duplicated(df$PAT_ID) == TRUE)]
#-----------------------------------------------#
#   model: cd4~ time + subject(random)          #
#-----------------------------------------------#

# subject id variable
id_subject <- "PAT_ID"

# time variable
timevar <- "ADM_DATE"

# partitioning variables of interest
vars <- data.frame(
  # list of partitioning variables of interest. 
  # Value of these variables should not change over time. 
  # Regarding categorical variables, only numerically coded categorical 
  # variables should be specified. 
  # For nominal categorical variables or factors, please first create 
  # corresponding dummy variable(s) and then pass through gvars.
  gvars = c("FEMALE"
            ),
  # types (categorical or continuous) of partitioning variables specified in gvar. 
  # For each of continuous partitioning variables, specify 1 and for each of 
  # the categorical partitioning variables, specify 0. 
  # Length of tgvars should match to the length of gvars
  tgvars = c("0"
             )
)

###############################################################################
# Run LongCART()  
###############################################################################

out1 <- LongCART(data = df
                 # name of the subject id variable
                 ,patid = id_subject
                 # fixed-effects part of the model
                 ,fixed = AGE ~ ADM_DATE
                 # list of partitioning variables of interest.
                 ,gvars = vars$gvars
                 # types of partitioning variables specified 
                 ,tgvars = vars$tgvars
                 )


out1 <- LongCART(data = df
                # name of the subject id variable
                ,patid = id_subject
                
                # a two-sided linear formula object describing the fixed-effects
                # part of the model, with the response on the left of a ~ operator 
                # and the terms, separated by + operators, on the right. 
                # Model with -1 to the end of right side indicates no intercept. 
                # For model with no fixed effect beyond intercept, please 
                # specify only 1 right to the ~ operator.
                ,fixed = AGE ~ 1
                
                # list of partitioning variables of interest.
                ,gvars = vars$gvars
                # types of partitioning variables specified 
                ,tgvars = vars$tgvars

                # Parameters --------------------------------------------------
                # the minimum number of observations that must exist in a node 
                # in order for a split to be attempted.
                ,minsplit = 100
                # minimum number of observations in any terminal node.
                ,minbucket = 50
                # alpha (i.e., nominal type I error) level for parameter 
                # instability test
                ,alpha = 0.05  
                # decimal points for displaying coefficients in the tree 
                # structure
                ,coef.digits = 2
                )

#--- Plot tree
par(mfrow=c(1,1))
par(xpd = TRUE)
plot(out1, compress = TRUE)
text(out1, use.n = TRUE)

#--- Plot longitudinal profiles of subgroups
ProfilePlot(x = out1, 
            timevar = timevar)

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
