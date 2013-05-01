 setup.cpue <-
function(rungrp,sourcedir,cpue,spp) {
  if(cpue != 0){        # Use sensitivity model run a given CPUE time series
# Find base directory sub-folder holding sensitivity option files
    sensdir <- paste(sourcedir,paste("cpue",spp,sep="."),sep="\\")
    frq.obj <- read.frq(paste(sensdir,paste(spp,"frq",sep="."),sep="\\"))
# Replace the section relating to the Purse seine catch
    if(spp == c("yft")){
      # Replace the relevant section of the yft.frq file - while accounting for the possibility that the number of rows or order differ between the sensitivity runs
      m <- rungrp$frq$mat
      # Remove all rows from rungrp..mat corresponding to PSfisheries
      m <- m[(m[,4] %in% psfshries)==F,]
      # Add all rows to rungrp..mat corresponding to PSfisheries in frq.obj
      b <- frq.obj$mat
      b <- b[(b[,4] %in% psfshries)==T,]
      rungrp$frq$mat <- rbind(m,b)
      # Sort according to fisheries again
      rungrp$frq <- sort.frq(rungrp$frq)
    } else {
      print(paste("No change to *.frq made for species = ",spp,sep=""))  
    }
  }
  return(rungrp) 
}
