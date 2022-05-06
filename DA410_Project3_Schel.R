
###########################
###########################
#                         #
#  Discriminant Analysis  #
#                         #
###########################
###########################

# We can read the SAT scores (with groupation information) data from the Internet:

sids <- read.table("sids.txt", header=TRUE)

# If not connected to the Internet, we could save the file and read it
# similarly to the following:
# satgroup<-read.table("Z:/stat_530/satgroup.txt", header=T)


# The first three columns are the SAT scores on the 3 tests,
# the last column (called group) is an indicator of whether the student successfully groupated
# (1 = groupated, 0 = did not groupate)

attach(sids)

# Using the built-in lda function in the MASS package
# for linear discriminant analysis:

library(MASS)

# assuming equal prior probabilities of groupating or not:
dis <- lda((group ~ hr + bw + factor68 + gessage), data=sids, prior=c(0.5, 0.5))

# a1, a2, a3 are given as "Coefficients of linear discriminants".

# Let's predict whether a new applicant with
# SAT scores of: hr = 550, bw = 610, factor68 = 480
# HR = 100, BW = 3000, Factor68 = 0.3, Gesage = 40
# will groupate:
newobs <- rbind( c(100, 3000, 0.3, 40) )
dimnames(newobs) <- list(NULL,c('hr', 'bw', 'factor68', 'gessage'))


# newobs <- rbind( c(550,610,480) )
# dimnames(newobs) <- list(NULL,c('hr','bw', 'factor68'))
newobs <- data.frame(newobs)
predict(dis,newdata=newobs)$class

# Posterior probabilities of this applicant being in each group:

predict(dis,newdata=newobs)$posterior

# Making predictions for several new individuals at once:



newobs <- rbind( c(145.5, 3940, .304, 41), c(139.7, 3740, .409, 40), c(121.3, 3005, .626, 38) )
dimnames(newobs) <- list(NULL,c('hr','bw', 'factor68'))
newobs <- data.frame(newobs)
predict(dis,newdata=newobs)


# assuming prior probabilities of groupating is about twice as large
# as probability of not groupating:

# dis <- lda(group ~ hr + bw + factor68, data=satgroup, prior=c(0.33, 0.67))

# If we do not specify any prior probabilities, it will by default use the proportions
# of the sampled individuals that are in each group as the prior probabilities.

dis2 <- lda(group ~ hr + bw + factor68, data=sids)

dis2


##### Misclassification rate of LDA rule:

# Simple plug-in misclassification rate:

group<-predict(dis, sids, method='plug-in')$class
table(group,group)

# The plug-in misclassification rate for LDA here is (11+4)/40 = 0.375.

# cross-validation rate of LDA rule:

########
correct<-rep(0,times=nrow(sids) )
for (j in 1:nrow(sids) ) {
mydis<-lda(grouping=group[-j], x=sids[-j,1:3], prior=c(0.5,0.5))
mypred<-predict(mydis,newdata=sids[j,1:3])$class
correct[j] <- (mypred==group[j])
}
cv.misclass <- 1-mean(correct)
cv.misclass
#########

# The cross-validation misclassification rate for LDA here is 0.425.
