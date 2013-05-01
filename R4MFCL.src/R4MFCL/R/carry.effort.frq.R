carry.effort.frq <- function(data=out.data,fishery=1,last=2008)
{
# Arguments
# last = last year of time series for which data must be filled in
# fishery = fishery for which data is to be repaired
# data = frq file data object
#
# Replaces the effort in the last year with effort in the previous year 2007 and sets catch to -1
# SJH 2/6/2009
# updated 29/06/09 to be more general
# extract c/e data for that fishery
# NMD 14/6/2010
# take account of existing effort data for some seasons in the last year
#
xxx <- data[data[,4]==fishery,1:6]

# Get a time unit comparable to the CPUE file to allow using match
# hangover from previous function, but okay
dec.time <- xxx[,1]+((xxx[,2]-0.5)/12)
qtimes <- seq(0.125,0.875,by=0.25)

#replace effort and set catch to -1
for(j in 1:length(qtimes))
{
# Get time periods
  dud <- last:(last-1) + qtimes[j]
  if(xxx[dec.time==dud[1],6] == -1){    #Only replace quarters having missing effort
    # replace effort with 2007 estimate first
    xxx[dec.time==dud[1],6] <- xxx[dec.time==dud[2],6]
    xxx[dec.time==dud[1],5] <- -1
  }
}
return(xxx)
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
