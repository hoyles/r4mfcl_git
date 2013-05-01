 setup.lensel <-
function(rungrp,fisheries,tog) {
  if(tog == 1){
    newval <- 3
    } else {
    newval <- 2
    }
  for (f in fisheries) {
    rungrp$doitall <- change.flag(rungrp$doitall,flagtype=-f,flagnum=26,newval)
  }
  return(rungrp)
}
