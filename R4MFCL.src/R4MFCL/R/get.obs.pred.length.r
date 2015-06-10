

get.obs.pred.length <- function(fitfile,fishery,ssfile,nage=40,type="length")
{
# SJH 5/19/2014 7:24:40 PM
# Goes to the fit file and gets the observed and predicted proportions at length
# and gets the residual  (p-phat)
# then returns all three in a list object
# one fishery at a time!

#
##gets residuals from length.fit file
filename <- fitfile
#number of fisheries
nfish <- scan(filename, nlines=1, skip=2)
#records per fishery
nfish2 <- scan(filename, nlines=1, skip=3)
#length intervals
lendat <- scan(filename, nlines=1, skip=1)
lenint <- lendat[2] + 1:lendat[1] * lendat[3] - lendat[3]

##time step for fishery matrix
tstep <- matrix(NA, nfish2[fishery], 3)
##set up obs. and pred. matrices
obslf <- matrix(NA, nfish2[fishery], lendat[1])
predlf <- matrix(NA, nfish2[fishery], lendat[1])

##read in
loadfit <- readLines(filename)
##locate first line
line1 <- grep(paste("# fishery", fishery), loadfit)
#deal with multiples
line1 <- ifelse(fishery == 1, line1[1], line1)
line1 <- ifelse(fishery == 2, line1[1], line1)

    if(type=="length") {
    x <- c(1,4,5)
    count2 <- nage + 7
    }
    if(type=="weight") {
    x <- c(1,6,8)
    count2 <- nage + 11
    }

count <- line1
#browser()
for(i in 1:nfish2[fishery]){
#print(i)
        tstep[i,] <- as.numeric(unlist(strsplit(loadfit[count+x[1]]," ")))
        obslf[i,] <- as.numeric(unlist(strsplit(loadfit[count+x[2]]," "))[-1])
        predlf[i,] <- as.numeric(unlist(strsplit(loadfit[count+x[3]]," "))[-1])
        count <- count + count2
}
#residuals: rows = time; col = length interval
resid <- obslf - predlf
return(list(obslf=obslf,predlf=predlf,resid=resid))
}

