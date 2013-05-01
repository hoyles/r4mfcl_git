 add.flag <-
function(doitall,flagtype,flagnum,newval,phase) {
  # by Simon Hoyle June 2008
  mt <- paste(c(flagtype," ",flagnum," [[:digit:]]+"),collapse="")
  ph <- paste("PHASE",phase,"$",sep="")
  loc <- grep(ph,doitall)                                             # find the row
  if (length(loc)>0) {
    pos <- max(loc)
    newl <- paste(" ",flagtype,flagnum, newval,sep=" ") # collapse into a vector
    doitall <- c(doitall[1:(pos-1)],newl,doitall[pos:length(doitall)])                 # add it to the doitall file
    }
  return(doitall)
}
