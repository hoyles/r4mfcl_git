 seas.flag <-
function(a, fishery,flagnum,seasf.list) {
  # by Simon Hoyle June 2008
  i <- fishery
  mt <- paste(c(-i," ",flagnum," [[:digit:]]+"),collapse="")
  loc <- grep(mt,a)                                             # find the row
  if (length(loc)>0) {
    b <- as.numeric(unlist(strsplit(a[loc],"[[:blank:]]+"))[2:4]) # extract fishery, flag number, setting
    nseas <- sum(seasf.list[,2]==i)                               # repeat for the number of seasons for this fishery
    repl <- t(matrix(b,ncol=nseas,nrow=3))                        # a matrix with nrows=nseasons
    repl[,1] <- -seasf.list[seasf.list[,2]==i,1]                  # replace the fishery number
    rpl <- rep("",nseas);  for (j in 1:nseas)  rpl[j] <- paste(repl[j,],collapse=" ") # collapse into a vector
    a <- c(a[1:(loc-1)],rpl,a[(loc+1):length(a)])                 # replace it in the doitall file
    }
  return(a)
}
