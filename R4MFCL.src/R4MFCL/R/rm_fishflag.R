 rm_fishflag <-
function(doitall,flag) {
  # by Simon D Hoyle March 2010
  mt <- paste("-[[:digit:]]+ +",flag," +[[:digit:]]+",sep="")
  loc <- grep(mt,doitall)                                             # find the rows
  if(length(loc)!=0) { doitall <- doitall[-loc] }
  return(doitall)
  }
