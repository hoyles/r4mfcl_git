 read.ests <-
function(rep.obj,ests="C:/assessments/alb/2008/6_area/28.splitgr3/ests.rep",x=1)
{
# Simon D Hoyle 24/6/09
# Based on do.critical.calcs
myrep <- rep.obj

# Dimensioning stuff
Nages <- myrep$nAges
Nfish <- myrep$nFisheries
Nyears <- (myrep$nTimes/myrep$nRecs.yr)-1
Fyear <- myrep$Year1

# Biological stuff
lenatage <- myrep$mean.LatAge
wtatage <- myrep$mean.WatAge

a <- readLines(ests)
line1 <- grep("Predicted catch by fishery by year and age", a)

# Determine number of lines to skip
start.lines <- seq(from=line1,length=Nfish,by=(Nyears+x)) + 1:Nfish
catage <- array(NA,dim=c(Nyears,Nages,Nfish),dimnames=list(seq(from=Fyear,length=Nyears,by=1),c(1:Nages),c(1:Nfish)))

# create an array with all the catch at age data
    for(i in 1:Nfish)
    {
    dat <- scan(ests, skip=start.lines[i], nmax=Nyears*Nages)
    catage[,,i] <- matrix(dat,nrow=Nyears,ncol=Nages,byrow=T)
    }

start.lines <- grep("fishery  realiz. realiz. ratio      predicted     observed   observed    effort      number of", a)



return(list(
    catage = catage))
}
