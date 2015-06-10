get.all.outcomes <- function(my.rundir=paste(drv,"/stm/2012/assessment/Model_runs/",sep=""),
rpfl=c("plot-12.par.rep"),parfl=c("12.par"),crpfl=c("catch.rep"),
runflds=c("bcase_oldexe", "run2", "run3", "run3a", "run4", "run5"),modnames=NULL,nofish=T,nofishp=c(44,4),debug=F,lateyr=2011)
{
# SJH 7/3/2014 8:57:48 PM - you need to have the output file names ....
# It creates a data.frame with all the goodies
# nofishp is the time for the SBF=0 calculations

vrep <- vector(mode="character",length=length(runflds))
vpar <- vector(mode="character",length=length(runflds))
vcrep <- vector(mode="character",length=length(runflds))
for(i in 1:length(runflds)){
  vrep[i] <- paste(my.rundir,runflds[i],rpfl,sep="/")
  vpar[i] <- paste(my.rundir,runflds[i],parfl,sep="/")
  vcrep[i] <- paste(my.rundir,runflds[i],crpfl,sep="/")
  }

#browser()
out <- unlist(get.outcomes.2014(read.rep(vrep[1]),read.par(vpar[1]),catch.rep=vcrep[1],nofish=nofish,nofishp=nofishp, lateyr=lateyr))

for(k in 2:length(runflds))
{
if(debug) print(runflds[k])
out <- cbind(out,unlist(get.outcomes.2014(read.rep(vrep[k]),read.par(vpar[k]),catch.rep=vcrep[k],nofish=nofish,nofishp=nofishp, lateyr=lateyr)))
}

if(is.null(modnames)) dimnames(out)[[2]] <- paste("Run",1:length(runflds))  else dimnames(out)[[2]] <- modnames

return(out)
}

 