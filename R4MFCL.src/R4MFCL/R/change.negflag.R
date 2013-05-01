 change.negflag <-
function(doitall,flagtype,flagnum,newval) {
  # by Simon Hoyle June 2008
  mt <- paste(c(" ",flagtype," ",flagnum," -[[:digit:]]+"),collapse="")
  loc <- grep(mt,doitall)                                             # find the row
  if (length(loc)>0) {
    rpl <- paste(" ",flagtype,flagnum, newval,sep=" ") # collapse into a vector
    for (i in 1:length(loc)) {
      commentpos <- grep("#",unlist(strsplit(doitall[loc[i]],"")))
      commented <- ifelse(commentpos > 5 || length(commentpos)==0 ,F,T)
      if (!commented) {
        doitall <- c(doitall[1:(loc[i]-1)],rpl,doitall[(loc[i]+1):length(doitall)])                 # replace it in the doitall file
      }
      }
    }
  return(doitall)
}
