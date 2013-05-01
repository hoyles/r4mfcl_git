steepness.doit <-
function(doitall,new.steepness,add_header=T,gap=2) {
  # Inserts a new line "recruitmentConstraints 01.par ###" after PHASE 1
  # by Simon Hoyle June 2008
  # modified by Nick Davies July 2009 - default format of doitall file has 2 lines between "PHASE1" and "recruitmentConstraints 01.par ###"
  # modified by SDH July09 to allow gap to be set as an option
  a <- doitall
  pos1 <- grep("#  PHASE 0 ",a)
  if(add_header) {
    stp.header <- readLines("i:\\assessments\\condor_mfcl\\steepness.code")
    a <- c(a[c(1:(pos1-2))],stp.header,a[c((pos1-1):length(a))])
    }
  loc <- grep("PHASE2",a)[2]
  newl <- paste("recruitmentConstraints 02.par",new.steepness)
  a <- c(a[1:(loc+gap)],"#",newl,"#",a[(loc+1+gap):length(a)])
  return(a)
  }
