 setup.effcreep <-
function(rungrp,creeprate) {
  nfish <- length(rungrp$frq$fish$fishery)
  rungrp$frq <- effortcreep(rungrp$frq,fisheries=1:nfish,creep=creeprate)
  return(rungrp)
}
