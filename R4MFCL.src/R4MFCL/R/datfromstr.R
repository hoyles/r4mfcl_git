 datfromstr <-
function(datstring) {
  # SDH 4/2/09
  return(as.numeric(unlist(strsplit(datstring,split="[[:blank:]]+"))[-1]))
  }
