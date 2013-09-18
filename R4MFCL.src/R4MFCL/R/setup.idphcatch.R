 setup.idphcatch <-
function(rungrp,fisheries=c(19,24),sourcedir,idph,spp) {
  if(idph != 0){        # Use sensitivity model run for assumed Indonesian-Philippines catches
# Replace the *.frq section relating to the Indonesian-Philippines catch
    if(spp == c("yft")){
#     Find base directory sub-folder holding sensitivity option files
      sensfilepath <- paste(paste(sourcedir,paste("idph",spp,sep="."),sep="\\"),paste(spp,"frq",sep="."),sep="\\")  # This assumes the sensitivity is a sub-folder of sourcedir (basedir); alternatively specify a file path
      idphfshries <- fisheries
      rungrp$frq <- add.catch.frq(rungrp$frq,sensfilepath,idphfshries)
    } else {
      print(paste("No change to *.frq made for species = ",spp,sep=""))  
    }
  }




  sensdir <- paste(sourcedir,paste("idph",spp,sep="."),sep="\\")
  rungrp$frq <- read.frq(paste(sensdir,paste(spp,"frq",sep="."),sep="\\"))
  return(rungrp) 
}
