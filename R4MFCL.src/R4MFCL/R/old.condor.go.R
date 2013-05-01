 old.condor.go <-
function(run.dir,frq.obj,tag.obj,doitall.obj,ini.obj,sub.obj=suball,species="alb",condor_f=condor_files,par.obj=NA,run_now=T) {
  # by Simon Hoyle June 2008
  setwd(run.dir)
  frqname<-paste(species,"frq",sep=".");tagname<-paste(species,"tag",sep=".");doitname<-paste("condor_doitall",species,sep=".");ininame<-paste(species,"ini",sep=".")
  write(doitall.obj,doitname)
  system(paste("c:\\cygwin\\bin\\dos2unix.exe",doitname,sep=" "))
  write(sub.obj,"condor.sub")
  write.frq(frqname,frq.obj)
  write.tag(tagname,tag.obj)
  if (is.na(ini.obj[1])==F) write.ini(ininame,ini.obj)
  if (is.na(par.obj[1])==F) write.par("start.par",par.obj)
  file.copy(condor_f,to=run.dir,overwrite=T)
  if(run_now) system("condor_submit condor.sub")
}
