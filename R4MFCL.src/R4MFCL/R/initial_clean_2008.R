 initial_clean_2008 <-
function(frq.obj) {
  # by Simon Hoyle June 2008
# Cleaning
  frq.obj$struct$td <- 7
  frq.obj$struct$te <- 4
  frq.obj$dl$unknown <- 0
  frq.obj$dl$wfintervals <- 1

  m <- frq.obj$mat
  # tags caught when no effort reported
  x1 <- c(1992, 5, 1, 17, 1000, -1, -1, rep(0,99))
  x2 <- c(1992, 8, 1, 20, 1000, -1, -1, rep(0,99))
  x3 <- c(1992, 2, 1, 30, 1000, -1, -1, rep(0,99))
  x4 <- c(1995, 2, 1, 23, 1000, -1, -1, rep(0,99))
  m <- rbind(m,x1,x2,x3,x4)
  # Remove 2008 data
  m <- m[m[,1]!=2008,]

  # 2007 effort = 2006 where none reported
  KR_TWf <- c(2,3,9,10,16,17,22,23); JPf <- c(1,8,15,21,29,30); PF_OTf <- c(7,13,14,20,24); FJf <- 6; TOf <- 12
  m <- m[!(m[,4] %in% KR_TWf & m[,1]==2007),]                             # remove KR/TW data for 2007
  m <- rbind(m,cbind(2007,m[m[,4] %in% KR_TWf & m[,1]==2006,c(2:106)]))   # replace with 2006 data
  m[m[,4] %in% KR_TWf & m[,1]==2007,c(5,7)] <- -1

  m <- m[!(m[,4] %in% JPf & m[,1]==2007),]                               # remove JP data for 2007
  m <- rbind(m,cbind(2007,m[m[,4] %in% JPf & m[,1]==2006,c(2:106)]))     # replace with 2006 data
  m[m[,4] %in% JPf & m[,1]==2007,c(5,7)] <- -1

  # FJ use 2006 for Q3, Q4 (fishery 6)
  m <- m[!(m[,4] %in% FJf & m[,1]==2007 & m[,2]>6),]                               # remove
  m <- rbind(m,cbind(2007,m[m[,4] %in% FJf & m[,1]==2006 & m[,2]>6,c(2:106)]))     # replace
  m[m[,4] %in% FJf & m[,1]==2007 & m[,2]>6,c(5,7)] <- -1
  # PF, OT use 2006 (fisheries 7, 13, 14, 20, 24)
  m <- m[!(m[,4] %in% PF_OTf & m[,1]==2007),]                               # remove
  m <- rbind(m,cbind(2007,m[m[,4] %in% PF_OTf & m[,1]==2006,c(2:106)]))     # replace
  m[m[,4] %in% PF_OTf & m[,1]==2007,c(5,7)] <- -1
  # TO use 2006 Q1 (fishery 12)
  m <- m[!(m[,4] %in% TOf & m[,1]==2007 & m[,2]==2),]                               # remove
  m <- rbind(m,c(2007,m[m[,4] %in% TOf & m[,1]==2006 & m[,2]==2,c(2:106)]))     # replace
  m[m[,4] %in% TOf & m[,1]==2007 & m[,2]==2,c(5,7)] <- -1

  # fix very low catches where eff dev is on boundary
  m[m[,4]==1 & m[,5]==15,5:6] <- c(150,-1)

  # eliminate zeroes in the catch
  m[m[,5]==0,5] <- 0.1

  # header fishery information
  frq.obj$fish$nations <- c("JP","KR","TW","AU","NC","FJ","OT","JP","KR","TW","AS,WS","TO","PF","OT","JP","KR","TW","AU","NZ","OT","JP","KR","TW","OT","ALL","ALL","ALL","ALL","ALL","ALL")
  frq.obj$fish$areas <- as.character(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,3,4,3,4,5,6))
  frq.obj$fish$gear <- c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","T","T","D","D","L","L")

  # Remove TW LF data for 2005 to 2008 (binned at 2cm)
  TWf <- c(3,10,17,23)
  m[m[,4] %in% TWf & m[,1]>2004,7]  <- -1
  frq.obj$mat <- m
  frq.obj <- sort.frq(frq.obj)
  return(frq.obj)
}
