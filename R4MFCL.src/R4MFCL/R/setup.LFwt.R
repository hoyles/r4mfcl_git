 setup.LFwt <-
function(rungrp,newLFwt,fisheries=-999) {
  for(f in fisheries) rungrp$doitall <- add.flag(rungrp$doitall,flagtype=-999,flagnum=49,newval=newLFwt,phase=7)
  return(rungrp)
}
