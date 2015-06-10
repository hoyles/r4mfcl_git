# SJDM 7/7/2014 a really pared down version of the method to get grid outcomes that I got off Nick D. Put in a function. Will be called by show.your.box.r

generate.grid.summaries <- function(grid.dir = "L:/yft/2014/assessment/Model_runs/grid",parfl = "12.par",
  repfl = "plot-12.par.rep",out.dir="C:/Users/SamM/Desktop",lateyr=2011)
{
    dirlist <- list.files(path=grid.dir,full.names=TRUE)

    # Specify exclusions - grid options that crashed
    excl <- c("")
    dirlist <- dirlist[!dirlist %in% excl]

    # Initialise grid summary file
    mod.res <- get.outcomes.2014(file.rep=paste(dirlist[1],repfl,sep="/"),
      file.par=paste(dirlist[1],parfl,sep="/"),
      catch.rep=paste(dirlist[1],"catch.rep",sep="/"),
      nofish=T,nofishp=c(44,4),lateyr=lateyr)
    grid.out <- unlist(mod.res)

    for(i in 2:length(dirlist))
    {
        if(file.exists(paste(dirlist[i],parfl,sep="/")))
        {
            mod.res <- get.outcomes.2014(file.rep=paste(dirlist[i],repfl,sep="/"),
              file.par=paste(dirlist[i],parfl,sep="/"),
              catch.rep=paste(dirlist[i],"catch.rep",sep="/"),
              nofish=T,nofishp=c(44,4),lateyr=lateyr)
            grid.out <- cbind(grid.out,unlist(mod.res))
            #grid.runs <- c(grid.runs,substring(dirlist[run],27,nchar(dirlist[run])))
        } else {
            print(paste(" Error: no parfile in rundir:   ",dirlist[run],sep=""))
        }
    }

    dimnames(grid.out)[[2]] <- list.files(path=grid.dir,full.names=FALSE)

  write.table(grid.out,paste(out.dir,"/grid.out.txt",sep=""))
}
