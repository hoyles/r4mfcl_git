# author : rob scott
# date   : 27/02/15
# weather: sunny
# lunch  : ham and cheese baguette

get.tag.prog <- function(tagfile){

  tagLines<- readLines(tagfile)
  lineNum <- grep('Tag_program',tagLines)
  proglst <- lapply(strsplit(tagLines[lineNum], " "), as.character)

  progvec <- unlist(lapply(proglst, function(pp) pp[length(pp)]))
  
  res     <- data.frame(tagEvent = 1:length(proglst),
                        tagProg  = progvec)
  return(res)                      
}


#get.tag.prog('/home/roberts/MFCL/like_prof/skj_steepness/input_files/skj.tag')


