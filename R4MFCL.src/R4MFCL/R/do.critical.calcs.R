 do.critical.calcs <-
function(repfile="P:\\yft\\2007\\BaseYFT\\yftfinal2007.rep",ests="P:\\yft\\2007\\BaseYFT\\ests.rep")
{
# Uses the dimensioning stuff provided in the rep file and the mean lengths and weights at age
# And takes the fishery specific catch at age from the ests file
# SJH 19/06/09

myrep <- read.rep(repfile)

# Dimensioning stuff
Nages <- myrep$nAges
Nfish <- myrep$nFisheries
Nyears <- myrep$nTimes/4
Fyear <- myrep$Year1

# Biological stuff
lenatage <- myrep$mean.LatAge
weiatage <- myrep$mean.Watage


line1 <- grep("Predicted catch by fishery by year and age", readLines(ests))

# Determine number of lines to skip
start.lines <- seq(from=line1,length=Nfish,by=Nyears) + 1:Nfish
catage <- array(NA,dim=c(Nyears,Nages,Nfish),dimnames=list(seq(from=1952,length=Nyears,by=1),c(1:Nages),c(1:Nfish)))

# create an array with all the catch at age data
    for(i in 1:Nfish)
    {
    dat <- scan(ests, skip=start.lines[i], nmax=Nyears*Nages)
    catage[,,i] <- matrix(dat,nrow=Nyears,ncol=Nages,byrow=T)
    }

####### Overall average size and length at capture by year
# note that I am ignoring the missing values as we are summing all catches so insignificant
overall <- array(NA,dim=c(Nyears,Nages),dimnames=list(seq(from=Fyear,length=Nyears,by=1),c(1:Nages)))
    for(i in 1:Nyears)
    {
    overall[i,] <- apply(catage[i,,],1,sum)
    }
overall.prop <- overall/apply(overall,1,sum)
overall.mean.age <- overall.prop%*%c(1:Nages)
overall.mean.length <- overall.prop%*%lenatage

####### Fishery average size and length at capture by year
# note that I am NOT ignoring the missing values as it is done on a fishery / year basis
fishery.mean.age <- array(NA,dim=c(Nyears,Nfish),dimnames=list(seq(from=Fyear,length=Nyears,by=1),c(1:Nfish)))
fishery.mean.length <- array(NA,dim=c(Nyears,Nfish),dimnames=list(seq(from=Fyear,length=Nyears,by=1),c(1:Nfish)))

    for(i in 1:Nfish)
    {
    tmp <- catage[,,i]
    tmp[apply(tmp,1,sum)==Nages,] <- 0
    tmp2 <- tmp/apply(tmp,1,sum)
    fishery.mean.age[,i] <- tmp2%*%c(1:Nages)
    fishery.mean.length[,i] <- tmp2%*%lenatage
    }
####  Get the optimum size and age etc from the same rep file
xxx <- get.critical.age(data=myrep)

return(list(crit = xxx[[1]],yield.at.age=xxx[[2]],rel.yield.at.age=xxx[[3]],
overall.mean.age=overall.mean.age,overall.mean.length=overall.mean.length,
fishery.mean.age=fishery.mean.age,fishery.mean.length=fishery.mean.length))
}
