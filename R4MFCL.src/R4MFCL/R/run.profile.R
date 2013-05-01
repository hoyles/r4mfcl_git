 run.profile <-
function(rundir, rungrp, startpar=NA, ptype="Fmult", target, nsteps=300, penalty=500000) {
  # by Simon Hoyle June 2008
  rg <- rungrp ; frq <- rg$frq ; tag<-rg$tag ; ini <- rg$ini
  if(is.na(startpar)) { par.obj <- rg$par } else { par.obj <- startpar }
  tg <- trunc(100*target)
  prof.type <- ifelse(ptype=="Fmult",0,1)
  Fmult.base <- rungrp$rep$Fmult
  commandline <- paste("alb.frq ",startpar," ", ptype,tg,".par -switch 4 1 1 ",nsteps," 2 167 ",prof.type," 2 165 ",tg," 2 166 ",formatC(penalty,format="d"),sep="")
  doitname <- paste("condor_doitall_cmd",ptype,tg,".alb",sep="")
  gocondor.commandline.alb(rundir,frq,tag,ini,commandline,parname=startpar,doitall.name=doitname, overwr=TRUE)
  }
