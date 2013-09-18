condor.go <-
function(run.dir,frq.obj,tag.obj,doitall.obj,ini.obj,sub.obj,species="alb",
         condor_f=condor_files,par.obj=NA,run_now=TRUE,fixpermissions=TRUE) {
##===================================================================================
## condor.sub can be missing if file "condor.sub" already in place in run.dir.
##  likewise condor_f can be missing if requites files already in run.dir
## by Simon Hoyle June 2008 -- modified, PK May-June 2011
##===================================================================================
  ## go to run directory for creating files
  thisdir <- getwd()
  setwd(run.dir)
  on.exit(setwd(thisdir))
  
  frqname<-paste(species,"frq",sep=".")
  tagname<-paste(species,"tag",sep=".")
  doitname<-paste("condor_doitall",species,sep=".")
  ininame<-paste(species,"ini",sep=".")
  write(doitall.obj,doitname)
  
  if(Sys.info()[1] != "Linux") {  ## is Windows:
    system(paste("c:\\cygwin\\bin\\dos2unix.exe",doitname,sep=" "))
  }
  
  ## if sub.obj not given, assume it's already copied to run.dir as "condor.sub"
  if(missing(sub.obj)) sub.obj <- readLines("condor.sub")

  p1 <- grep("TRANSFER_INPUT_FILES",sub.obj)
  if(length(p1) != 1) stop("condor.sub file malformed")
     
  if(length(grep("[\\]+[^//]",sub.obj[-(1:p1)])) > 0) {  ## was from windows
    sub.obj.w <- c(sub.obj[1:p1],gsub("[\\]+([^//])","\\\\\\\\\\1",sub.obj[-(1:p1)]))
    sub.obj.l <- c(sub.obj[1:p1],gsub("[\\]+([^//])","/\\1",sub.obj[-(1:p1)]))
  } else {
    sub.obj.w <- c(sub.obj[1:p1],gsub("/+([^/])","\\\\\\\\\\1",sub.obj[-(1:p1)]))
    sub.obj.l <- sub.obj
  }

  if(identical(sub.obj.w,sub.obj.l)) {
    same <- TRUE
    write(sub.obj.w,"condor.sub")
  } else {
    same <- FALSE
    write(sub.obj.w,"condor.windows.sub")
    write(sub.obj.l,"condor.linux.sub")
  }
  
  write.frq(frqname,frq.obj)
  write.tag(tagname,tag.obj)
  if(!is.na(ini.obj[1])) write.ini(ininame,ini.obj)
  if(!is.na(par.obj[1])) write.par("start.par",par.obj)

  ## return to calling directory for copying files 
  setwd(thisdir)  
  if(!missing(condor_f)) {
    for(i in 1:length(condor_f)) file.copy(from=condor_f[i],to=run.dir,overwrite=T)
  }

  ## set rw permission for everyone on all files in run directory
  if(fixpermissions) Sys.chmod(paste(run.dir,list.files(run.dir),sep="/"),mode="0777")

  ## launch condor run
  if(run_now){
    setwd(run.dir)
    if(same) {
      system(paste("condor_submit", "condor.sub"))
    } else {
      if(Sys.info()[1] != "Linux") {  ## is Windows:
        system(paste("condor_submit", "condor.windows.sub"))
      } else {  ## is Linux:
        system(paste("condor_submit", "condor.linux.sub"))
      }
    }
  }
}

