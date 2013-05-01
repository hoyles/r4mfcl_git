 setup.steepness <-
function(rungrp,newsteep){
  loc <- grep("^recruitmentConstraints",rungrp$doitall)
  rungrp$doitall <- rungrp$doitall[-loc]   #remove all instances of setting
  rungrp$doitall <- steepness.doit(rungrp$doitall,newsteep)
# Check to see if steepness.doit has duplicated the function
  loc <- grep("^# Apply the recruitment",rungrp$doitall)      # Find all positions of the function
  loc2 <- grep("^#  PHASE 0 - create initial par file",rungrp$doitall)      # Find the top of the makepar phase
  if(length(loc) > 1){     # Remove duplicate instances of the function
    rungrp$doitall <- c(rungrp$doitall[1:(loc[2]-1)],rungrp$doitall[(loc2-1):length(rungrp$doitall)])
  }
  return(rungrp)
}
