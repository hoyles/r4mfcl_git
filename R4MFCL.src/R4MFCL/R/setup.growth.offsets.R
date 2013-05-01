 setup.growth.offsets <-
function(rungrp,ageclasses,penwt,phase,tog) {
  if(tog == 1) {
    rungrp$doitall <- add.flag(rungrp$doitall,flagtype=1,flagnum=173,newval=ageclasses,phase)
    rungrp$doitall <- add.flag(rungrp$doitall,flagtype=1,flagnum=182,newval=penwt,phase)
    rungrp$doitall <- add.flag(rungrp$doitall,flagtype=1,flagnum=184,newval=1,phase)
  }
  return(rungrp)
}
