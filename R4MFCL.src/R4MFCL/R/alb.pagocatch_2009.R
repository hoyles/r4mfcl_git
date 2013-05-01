 alb.pagocatch_2009 <-
function(frq.obj) {
  # by Simon Hoyle June 2008
  pagodir <- "P:\\alb\\2009\\Pago\\CPUE\\"
  f <- c(1,8,15,21,2,9,16,22,3,10,17,23)
  nat <- rep(c("JP","KR","TW"),each=4)
  reg <- rep(c(1,2,3,4),times=4)
  load(file=paste(pagodir,"2009_pago_levuka.RData",sep=""))
  pl <- pl[,c("set_yrqtr","flag_id","set_yr","set_qtr","alb_n","hook","reg")]
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
    m[pos,5] <- apply(cbind(m[pos,5],catch),1,max) # insert the larger catch
    x[x[,4]==fy,] <- m
    a <- pl[pl$flag_id==flag & pl$reg==region & (pl$set_yrqtr %in% yrq)==F,]
    catch_m <- tapply(a$alb_n,a$set_yrqtr,sum)
    effort_m <- tapply(a$hook,a$set_yrqtr,sum)/100
    if(length(catch_m)>0) {
      struc_m <- cbind(tapply(a$set_yr,a$set_yrqtr,mean),tapply((a$set_yrqtr - a$set_yr + .125) * 12 -1,a$set_yrqtr,mean),1,fy,catch_m,effort_m,-1,
          matrix(0,nrow=length(catch_m),ncol=99))
      x <- rbind(x,struc_m)
      }
    }
  frq.obj$mat <- x
  return(frq.obj)
  }
