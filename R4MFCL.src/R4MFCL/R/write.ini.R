write.ini <-
function(ini.file,ini.obj, old.format=FALSE) {
##============================================================================
## by Simon Hoyle June 2008
##  revised, PK June 2011
##============================================================================
  o <- ini.obj
  #a1 <- character()
  con <- file(ini.file,open="w")
  on.exit(close(con))
   
  if(with(ini.obj,exists("version")) & !old.format) {
    writeLines(c("# ini version number",o$version),con)
  }

  writeLines(c("# number of age classes",o$nages),con)
                                                   
  if(with(o,exists("tag.fish.rep"))) {
    writeLines("# tag fish rep",con)
    write.table(o$tag.fish.rep,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("grpflags"))) {
    writeLines("# tag fish rep group flags",con)
    write.table(o$grpflags,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("activeflags"))) {
    writeLines("# tag_fish_rep active flags",con)
    write.table(o$activeflags,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("reptarget"))) {
    writeLines("# tag_fish_rep target",con)
    write.table(o$reptarget,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("reppenalty"))) {
    writeLines("# tag_fish_rep penalty",con)
    write.table(o$reppenalty,con,quote=F,append=T,row.names=F,col.names=F)
  }

  writeLines(c("# maturity at age",paste(o$mat,collapse=" ")),con)
               
  writeLines(c("# natural mortality (per year)",o$M),con)

  if(with(o,exists("movemap"))) writeLines(c("# movement map",paste(o$movemap,collapse=" ")),con)
                                            
  if(with(o,exists("diffcoffs"))) writeLines("# diffusion coffs (per year)",con)
  write.table(o$diffcoffs,con,quote=F,append=T,row.names=F,col.names=F)
                                             

  writeLines("# age_pars",con)
  write.table(o$age_pars,con,quote=F,append=T,row.names=F,col.names=F)

  writeLines(c("# recruitment distribution by region",paste(o$recbyreg,collapse=" ")),con)

  writeLines(c("# The von Bertalanffy parameters",
               "# Initial  lower bound  upper bound",
               "# ML1",
                paste(paste(o$VBLmin,collapse=" ")),
               "# ML2",
                paste(paste(o$VBLmax,collapse=" ")),
               "# K (per year)",
                paste(paste(o$VBK,collapse=" ")),
               "# Length-weight parameters",            
                paste(paste(o$LW,collapse=" ")),
               "# sv(29)",
                o$sv29,
               "# Generic SD of length at age",
                paste(paste(o$sdLatA,collapse=" ")),
               "# Length-dependent SD",
                paste(paste(o$Ldep_sd,collapse=" ")),
               "# The number of mean constraints",
                paste(paste(o$Nmeanconstr,collapse=" "))), con)
}
