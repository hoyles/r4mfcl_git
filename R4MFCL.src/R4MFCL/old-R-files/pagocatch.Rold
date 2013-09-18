 pagocatch <-
function(frq.obj) {
  # by Simon Hoyle June 2008
  pagodir <- "L:\\alb\\2008\\Pago\\"
  f <- c(1,8,15,21,2,9,16,22,3,10,17,23)
  nat <- rep(c("JP","KR","TW"),each=4)
  reg <- rep(c(1,2,3,4),times=4)
  if (!exists("pl")) {
    load(file=paste(pagodir,"Keith.pl2.RData",sep=""))
    pl <- pl2[,c("set_yrqtr","flag_id","set_yr","set_qtr","alb_n","hook","reg")]
    rm(pl2)
    }
  x <- frq.obj$mat
  for (i in 1:length(f))
  {
    fy <- f[i] ; flag <- nat[i] ; region <- reg[i]
    m <- x[x[,4]==fy,]
    yrq <- m[,1] + (m[,2]+1)/12 - 0.125
    yr <- trunc(yrq)
    mo <- (yrq - yr + .125) * 12 -1
    a <- pl[pl$flag_id==flag & pl$reg==region & pl$set_yrqtr %in% yrq,]
    catch <- tapply(a$alb_n,a$set_yrqtr,sum)
    effort <- tapply(a$hook,a$set_yrqtr,sum)/100
    pos <- match(names(catch),yrq)
#    m[m[pos,5]<catch,5:6] <- cbind(catch,effort) test <-cbind(pos,m[pos,5],catch,m[pos,6],effort)
#    pos2 <- pos[m[pos,5]<catch]
    m[pos,5] <- apply(cbind(m[pos,5],catch),1,max) # insert the larger catch
    x[x[,4]==fy,] <- m
    }
  frq.obj$mat <- x
  return(frq.obj)
  }
