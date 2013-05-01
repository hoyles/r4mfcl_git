 oldsetup <-
function(oldbasedir){
  setwd(oldbasedir)
  # ---- make a frq object ------
  # Clean up 'movements' line manually to have only 1 item.
  init.frq <- read.frq(paste(oldbasedir,"MFCL_ALB_1.TXT",sep="\\"))
  init.frq <- alb.initial_clean_2008(init.frq)

  # ------------------------------------------------------------------------
  # 1. remove before 1960
  rm_pre_1960.frq <- init.frq
  x <- rm_pre_1960.frq$mat
  x <- x[x[,1]>1959,]
  rm_pre_1960.frq$mat <- x
  rm_pre_1960.frq$struct$yr1 <- 1960
  # ------------------------------------------------------------------------
  # 2. Integrate GLM, but (in this run) keep catchability devs for JP and KR

  addGLM.frq <- rm_pre_1960.frq
  addGLM.frq$fish$nations <- c("JP","KR","TW","AU","NC","FJ","OT","JP","KR","TW","AS,WS","TO","PF","OT","JP","KR","TW","AU","NZ","OT","JP","KR","TW","OT","ALL","ALL","ALL","ALL","ALL","ALL")
  addGLM.frq$fish$areas <- as.character(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,3,4,3,4,5,6))
  addGLM.frq$fish$gear <- c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","T","T","D","D","L","L")

#  pagodir <- "L:\\alb\\2008\\Pago\\"
  pagodir <- "P:\\alb\\2008\\Pago\\"
  fnames <- list.files(path=pagodir,pattern="^[TJK].*dat")
  f <- c(1,8,15,21,2,9,16,22,3,10,17,23)
  coefs <- list()
  # Load CPUE coefficients
  for (i in 1:length(f))
  {
    coefs[[f[i]]] <- read.table(paste(pagodir,fnames[i],sep=""),header=T)
  }

  # Effort = catch / CPUE
  load(file=paste(pagodir,"Keith.pl2.RData",sep=""))
  pl <- pl2[,c("set_yrqtr","flag_id","set_yr","set_qtr","alb_n","reg")]
  rm(pl2)
  x <- addGLM.frq$mat
  for (i in 1:length(f))
  {
    fy <- f[i]
    co <- coefs[[fy]]
    x.f.yrqtr <- paste(x[,4],x[,1] + (x[,2]+1)/12 - 0.125)
    co$f.yrqtr <- paste(fy,co$yrqtr)
    x[x[,4]==fy,6] <- -1                        # remove all effort for the fishery
    mtc <- match(co$f.yrqtr,x.f.yrqtr)          # find the matches  - missing cells in frq give NA

    yrq <- co[is.na(mtc)==T,1]                   # fix the bad ones
    y <- trunc(yrq)
    m <- (yrq - y + .125) * 12 -1
    a <- pl[pl$flag_id==addGLM.frq$fish$nations[fy] & pl$reg==addGLM.frq$fish$areas[fy] & pl$set_yrqtr %in% yrq,]
    catch <- tapply(a$alb_n,a$set_yrqtr,sum)
    miss <- matrix(0,nrow=length(catch),ncol=dim(x)[2])
    miss[,1:7] <- cbind(y,m,1,fy,catch,-1,-1)
    x <-rbind(x,miss)

    x.f.yrqtr <- paste(x[,4],x[,1] + (x[,2]+1)/12 - 0.125)
    mtc <- match(co$f.yrqtr,x.f.yrqtr)          # find the matches  - missing cells in frq give NA
    x[mtc,6] <- x[mtc,5] / co$coef               # insert the appropriate effort

  }
  # cases in 2007 where there's no catch and no effort
  loc <- x[x[,1]==2007 & x[,5]==-1 & x[,6]==-1,1:4]
  for (i in 1:length(loc[,1])) {
    x[x[,1]==2007 & x[,2]==loc[i,2] & x[,4]==loc[i,4],5] <- x[x[,1]==2006 & x[,2]==loc[i,2] & x[,4]==loc[i,4],5]
  }
  addGLM.frq$mat <- x

  # ------------------------------------------------------------------------
  # 2a. Include JP/KR GLM indices as 'trusted' indices - no q deviates
  incl_JPKR.frq <- addGLM.frq
  # ------------------------------------------------------------------------
  # 3. Clean the LF data - get rid of Pago LF pre-1971

  source(paste(basedir,"R lf frq functions.r",sep="\\"))
  cleanlf.frq <- clean.lfdata(addGLM.frq)
  # ------------------------------------------------------------------------
  # 4. Estimate catchability deviates every 6 months, and with lower penalty

  catchdevs.frq <- cleanlf.frq
  # ------------------------------------------------------------------------
  # 5. Allow LL selectivity to decline with age except in region 1, or 2, or both 1 and 2.
  selage.frq <- catchdevs.frq
  selage.frq$fish$nations <- c("JP","KR","TW","AU","NC","FJ","OT","JP","KR","TW","AS,WS","TO","PF","OT","JP","KR","TW","AU","NZ","OT","JP","KR","TW","OT","ALL","ALL","ALL","ALL","JP","JP")
  selage.frq$fish$areas <- as.character(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,3,4,3,4,5,6))
  selage.frq$fish$gear <- c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","T","T","D","D","L","L")

  # ------------------------------------------------------------------------
  # ------------------------------------------------------------------------
  # 6. Seasonal longline fisheries

  seasonal.frq <- selage.frq
  seasonal.frq$fish$nations <- c("JP","KR","TW","AU","NC","FJ","OT","JP","KR","TW","AS,WS","TO","PF","OT","JP","KR","TW","AU","NZ","OT","JP","KR","TW","OT","ALL","ALL","ALL","ALL","JP","JP")
  seasonal.frq$fish$areas <- as.character(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,3,4,3,4,5,6))
  seasonal.frq$fish$gear <- c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","T","T","D","D","L","L")

  # make frq and tag file seasonal
  for (i in c(30,29,24:1)) {
    seasonal.frq <- seas.frq(seasonal.frq,i)
  }
  storefish <- seasonal.frq$fish
  return(storefish)

}
