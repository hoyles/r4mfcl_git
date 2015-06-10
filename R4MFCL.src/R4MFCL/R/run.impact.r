run.impact <- function(
rootdir=  "C:/bet/2014/assessment/Setup",
basedir="C:/bet/2014/assessment/Model_runs/2014s/Run354",
mfclfiles=c("mfclo64.exe","mfcl.cfg"),
runfiles=c("bet.frq","bet.tag","10.par"),
gears=list(ll=c(1:13,23),psas=c(14,16),psun=c(15,17),idph=c(18,19,24),oth=c(20,21,22,25)),
mfclcmd =c("mfclo64 bet.frq 10.par junk.par -switch","2 171 1 1 1 1 1 189 0 -999 55 0"))
{
#tmp.dir <- getwd()

    rundir <- paste(basedir,"impact",sep="/")
    dir.create(rundir)
    setwd(rundir)   # Set working directory for running MFCL

    # copy across MFCL execution files
    file.copy(paste(basedir,mfclfiles,sep="/"),to=rundir,overwrite=T)
    # copy across bet.frq,bet.tag, and 11.par
    file.copy(paste(basedir,runfiles,sep="/"),to=rundir,overwrite=T)
#


        for(k in 1:length(gears))
        {
        switches <- paste("-",gears[[k]]," 55 1",sep="",collapse=" ")
        doit <- paste(mfclcmd[1],length(gears[[k]])+4,mfclcmd[2],switches,collapse=" ")
        write(doit,paste(names(gears)[[k]],"doit.txt",sep=""))
        system(doit,show.output.on.console = F,invisible = F,wait=TRUE)
        file.copy(paste(rundir,"plot-junk.par.rep",sep="/"),paste(rundir,"/",names(gears)[[k]],".rep",sep=""))
        }
setwd(wkdir)
}
