 setup.timesplit <-
function(rungrp,splitx,storefish) {
  if (splitx == 1) {
    fnt <- storefish$nations; frg <- storefish$areas
    frq <- rungrp$frq ; tag <- rungrp$tag ; doitall <- rungrp$doitall

    splitf1 <- storefish[fnt %in% c("TW","JP","KR") & frg %in% c(1,4),3] ; divyrs1 <- c(1971,1991)  # This matches the nations in fnt with the fisheries defined
#   splitf1 is a vector of the fisheries that will be split
    frq$fish <- storefish 
    lst <- timesplit.frq(frq, div.fish=splitf1, divyrs1)
    frq <- lst$frq
    fishsplit1 <- lst$fishsplit

# Temporary lines added to check operation for single fish split    
    splitf2 <- storefish[lst$frq$fish$nations %in% c("TW","JP","KR") & lst$frq$fish$areas %in% c(2,3),3] ; divyrs2 <- c(1976,1986)
#    splitf2 <- storefish[lst$frq$fish$nations %in% c("TW","JP","KR") & lst$frq$fish$areas %in% c(2),3] ; divyrs2 <- c(1976,1986)   #Test for only region 2
    lst <- timesplit.frq(frq, div.fish=splitf2, divyrs2)
    rungrp$frq <- lst$frq
    fishsplit2 <- lst$fishsplit
    
    tag_tmp <- timesplit.tag(tag, fishsplit1)
    rungrp$tag <- timesplit.tag(tag_tmp, fishsplit2)
#    rungrp$tag <- tag_tmp          # Temporary line to check operation for splitf1 only
    
    a <- timesplit.doitall(doitall, fishsplit1)
    a <- timesplit.doitall(a, fishsplit2)
    rungrp$doitall <- a
    }
  return(rungrp)
  }
