 varfromstr <-
function(datstring,cols=c(2:3)) {
# By Pierre Kleiber 
  return(as.numeric(unlist(strsplit(datstring,split="[[:blank:]]+|\\(|\\)|\\,"))[cols]))
  }
