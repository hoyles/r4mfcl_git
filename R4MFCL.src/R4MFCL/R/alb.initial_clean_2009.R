 alb.initial_clean_2009 <-
function(frq.obj) {
  # by Simon Hoyle June 2009
# Cleaning
  frq.obj$struct$td <- 7
  frq.obj$struct$te <- 4
  frq.obj$dl$wfint <- 0
  frq.obj$struct$ntg <- 14

  m <- frq.obj$mat
  # tags caught when no effort reported
  x1 <- c(1992, 5, 1, 17, 1000, -1, -1, rep(0,99))
  x2 <- c(1992, 8, 1, 20, 1000, -1, -1, rep(0,99))
  x3 <- c(1992, 2, 1, 30, 1000, -1, -1, rep(0,99))
  x4 <- c(1995, 2, 1, 23, 1000, -1, -1, rep(0,99))
  m <- rbind(m,x1,x2,x3,x4)
  # Remove 2009 data
  m <- m[m[,1]!=2009,]

  # Define fisheries
  KRf <- c(2,9,16,22); TWf <- c(3,10,17,23); JPf <- c(1,8,15,21); PFf=13; OTf <- c(7,14,20,24,29,30); NCf=5; FJf=6; TOf=12; SAf=11;
  
  # 2007 catch -> 2008 where none reported
  a <- c(JPf,KRf)
  m <- m[!(m[,4] %in% a & m[,1]==2008),]                               # remove JP, KR data for 2008. What about TW in R1?
  m <- rbind(m,cbind(2008,m[m[,4] %in% a & m[,1]==2007,c(2:106)]))     # replace with 2007 data
  m[m[,4] %in% a & m[,1]==2008,c(6,7)] <- -1

  # 2007 effort -> 2008 where none reported
  a <- c(NCf,FJf,OTf,SAf,TOf,20,24,26,29,30)
  m <- m[!(m[,4] %in% a & m[,1]==2008),]                               # remove
  m <- rbind(m,cbind(2008,m[m[,4] %in% a & m[,1]==2007,c(2:106)]))     # replace
  m[m[,4] %in% a & m[,1]==2008 & m[,2]>6,c(5,7)] <- -1

  # Okay are TWf, 4,18(AU), 13(PF), 19(NZ), 25(NZ troll), 27,28(Driftnet).
  # What about TW offshore? No, it's okay - 99% of vessels in the extract were DW.   
  
  # eliminate zeroes in the catch
  m[m[,5]==0,5] <- -1
  m <- m[m[,5]>9 | m[,6]!=-1,]

  # header fishery information
  frq.obj$fish$nations <- c("JP","KR","TW","AU","NC","FJ","OT","JP","KR","TW","AS,WS","TO","PF","OT","JP","KR","TW","AU","NZ","OT","JP","KR","TW","OT","ALL","ALL","ALL","ALL","ALL","ALL")
  frq.obj$fish$areas <- as.character(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,3,4,3,4,5,6))
  frq.obj$fish$gear <- c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","T","T","D","D","L","L")

  frq.obj$mat <- m
  frq.obj <- sort.frq(frq.obj)
  return(frq.obj)
}
