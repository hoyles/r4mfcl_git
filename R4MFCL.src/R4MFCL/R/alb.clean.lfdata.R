 alb.clean.lfdata <-
function(infrq) {
  # by Simon Hoyle June 2008
  load(file="L:\\alb\\2008\\base\\6_area\\lfd.RData")
  a <- lfd[lfd$area_subst != T,]          # no area_subst data
  a <- a[a$len>=30 & a$len<130,]        # Size limit in mfcl
  a <- a[a$astrat!="T" | a$centlat != -27.5,]       # avoid the 'half inside' limit problem
  a <- a[a$origin_id!="SRUS" | a$yrqtr > 1971,]               #   Remove the unwanted ones
  flagfish <- data.frame(JP=c(1,8,15,21),KR=c(2,9,16,22),TW=c(3,10,17,23),AU=c(4,14,18,24),NC=c(5,14,20,24),
            FJ=c(6,14,20,24),AS=c(7,11,20,24),WS=c(7,11,20,24),TO=c(7,12,20,24),PF=c(7,13,20,24),NZ=c(7,14,19,24))
  flagfish <- cbind(stack(flagfish),1:4)
  names(flagfish) <- c("fishery","flag","reg")
  flagfish$flag.reg <- as.character(interaction(flagfish$flag,flagfish$reg,sep="."))
  a$fishery <- flagfish[match(as.character(interaction(a$flag_id,a$reg,drop=T)),flagfish$flag.reg),1]
  table(a$fishery,a$flag_id,a$reg)
  a <- a[,c(2,3,4,18,20,31,36,37)]
  a2 <- data.frame(yr=c(1962),qtr=2,mon=5,len=30:129,freq=rep(0,100),reg=rep(1,100),yrqtr=1962.375,fishery=1)
  a3 <- rbind(a,a2)
  a3$yrqtr <- factor(a3$yrqtr,levels=seq(1960.125,max(a3$yrqtr),by=0.25))
  fromdb <- tapply(a3$freq,list(a3$yrqtr,a3$len,a3$fishery),sum)
  fromdb[is.na(fromdb)] <- 0

  # replace the frq file length frequencies
  x <- infrq$mat
  for (i in 1:dim(x)[1]) {
  #  if (x[i,7] != -1 && x[i,1] %in% 1960:1970 && x[i,4] %in% temp$fish[temp$fish$nations=="JP",3]) {
    if (x[i,7] != -1 &&  x[i,1] %in% 1960:1970 && x[i,4] <25) {
      f <- x[i,4]
      y <- x[i,1]
      m <- x[i,2]
      yq <- y + (m+1)/12 - 0.125
      a <- fromdb[,,dimnames(fromdb)[[3]]==f]
      x[i,7:106] <- a[dimnames(a)[[1]]==yq,]
    }
  }
  x[apply(x[,7:106],1,sum)==0,7] <- -1
  infrq$mat <- x
  return(infrq)
}
