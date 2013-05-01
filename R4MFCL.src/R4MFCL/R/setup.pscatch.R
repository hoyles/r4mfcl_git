 setup.pscatch <-
function(rungrp,sourcedir,PScatch,spp) {
  if(PScatch != 0){        # Use sensitivity model run Purse Seine catches
# Replace the section relating to the Purse seine catch
    if(spp == c("yft")){
#     Find base directory sub-folder holding sensitivity option files
      sensfilepath <- paste(paste(sourcedir,paste("PScatch",spp,sep="."),sep="\\"),paste(spp,"frq",sep="."),sep="\\")  # This assumes the sensitivity is a sub-folder of sourcedir (basedir); alternatively specify a file path
      psfshries <- c(14,15,16,17,20)
      rungrp$frq <- add.catch.frq(rungrp$frq,sensfilepath,psfshries)
    } else {
      print(paste("No change to *.frq made for species = ",spp,sep=""))  
    }
  }
  return(rungrp) 
}
