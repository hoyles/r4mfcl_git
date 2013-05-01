 setup.startyr <-
function(rungrp,newstartyr) {
  rungrp$frq <- start_year.frq(rungrp$frq,newstartyr)
  return(rungrp)
}
