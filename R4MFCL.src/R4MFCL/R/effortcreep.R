 effortcreep <-
function(frq.obj,fisheries,creep) {
  # by Simon Hoyle June 2008
  x <- frq.obj ; f <- fisheries
  m <- x$mat[x$mat[,4] %in% f & x$mat[,6]!=-1,]

  timet <- m[,1] + (m[,2]+1)/12 - 0.125
  m[,6] <- m[,6] * (1+creep/100)^(timet - 1959)

  x$mat[x$mat[,4] %in% f & x$mat[,6]!=-1,] <- m
  frq.obj <- x
  return(frq.obj)
}
