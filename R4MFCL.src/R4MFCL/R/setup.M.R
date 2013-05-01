 setup.M <-
function(rungrp,newM) {
  rungrp$ini$M <- newM
  rungrp$doitall <- change.flag(rungrp$doitall,2,flagnum=33,newval=0)
  return(rungrp)
}
