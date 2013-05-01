write.tag <- function(tagfile,tag.obj) {
  # by Simon Hoyle June 2008
  # - output comment labels for Tag program

  x <- tag.obj
  a <- "# RELEASE GROUPS    STARTING LENGTH    NUMBER INTERVALS    INTERVAL LENGTH"
  a <- rbind(a,paste("       ",x$hd,collapse="       "),"#","#")
  a <- rbind(a,"# TAG RECOVERIES", paste(c("# ",1:x$hd$nrel),collapse="   "))
  x$nrecov.grp <- table(factor(x$rel.recov$grp,levels=1:x$hd$nrel))
  a <- rbind(a,paste(c("  ",x$nrecov.grp),collapse="   "))
  for (i in 1:x$hd$nrel)
  {

    a <- rbind(a,"#","#","#---------------------------------")
    a <- rbind(a,paste(c("#   ",i," - RELEASE REGION    YEAR    MONTH   Tag_program ",x$tagprog[i]),collapse=""))
    a <- rbind(a,paste(c("    ",x$rel[i,]),collapse="         "))
    a <- rbind(a,paste(x$rel.lens[i,],collapse=" "))
    if (x$nrecov.grp[i] > 0)
    {
      a <- rbind(a,"#","#","# LENGTH RELEASE    FISHERY    RECAP YEAR    RECAP MONTH    NUMBER")
      tags <- x$rel.recov[x$rel.recov[,1]==i,2:6]
      for (j in 1:dim(tags)[1])
      {
        a <- rbind(a,paste(c("",tags[j,]),collapse="          "))
      }
    }
  }
  writeLines(a,tagfile)
}

##################################################################################
# The MIT License
#
# Copyright (c) 2009 Secretariat of the Pacific Community
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this
# software and associated documentation files (the "Software"), to deal in the Software 
# without restriction, including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
# to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies 
# or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
# FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
