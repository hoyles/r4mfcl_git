 rm_fisheries.doitall <-
function(a,rmfisheries) {
  # by Simon Hoyle June 2008
  for (i in 1:length(rmfisheries)) {
    mt <- paste(c(-rmfisheries[i]," [[:digit:]]+ [[:digit:]]+"),collapse="")
    loc <- grep(mt,a)                                             # find the rows
    a <- a[-loc]
    }
  return(a)
  }
