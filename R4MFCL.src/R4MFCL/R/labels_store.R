 labels_store <-
function(labelfile=basecase.labels) {
# Simon Hoyle
  nr <- readLines(labelfile)
  a <- read.table(labelfile, nrows=length(nr)-1,sep=" ")
  a <- cbind(a,apply(a,1,paste,collapse=" "))
  names(a) <- c("fsh","gear","nations","areas","fleet")
  return(a)
}
