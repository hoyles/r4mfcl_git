 NZtrollglm <-
function(frq.obj,fishery,coef.file) {
  # by Simon Hoyle June 2008
  # Load CPUE coefficients
  co <- read.table(coef.file,header=T)
  x <- frq.obj$mat ; fy <- fishery
  x.f.yrqtr <- paste(x[,4],x[,1] + (x[,2]+1)/12 - 0.125)
  co$f.yrqtr <- paste(fy,co$yrqtr)
  x[x[,4]==fy,6] <- -1                        # remove all effort for the fishery
  mtc <- match(co$f.yrqtr,x.f.yrqtr)          # find the matches  - missing cells in frq give NA

  co <- co[is.na(mtc)==F,]                    # fix the missing one

  x.f.yrqtr <- paste(x[,4],x[,1] + (x[,2]+1)/12 - 0.125)
  mtc <- match(co$f.yrqtr,x.f.yrqtr)          # find the matches  - missing cells in frq give NA
  x[mtc,6] <- (x[mtc,5]/mean(x[mtc,5])) / co$coef               # insert the appropriate effort

  frq.obj$mat <- x
  return(frq.obj)
}
