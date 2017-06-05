library(sitools)

f2si<-function (number, rounding=F, digits=ifelse(rounding, NA, 6)) 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24, 1e+27)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k", 
           "M", "G", "T", "P", "E", "Z", "Y", NA)
  
  #lut <- c(0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
  #         1e+24, 1e+27)
  #pre <- c("", "k", 
  #         "M", "G", "T", "P", "E", "Z", "Y", NA)
  
  ix <- findInterval(number, lut)
  if (ix>0 && ix<length(lut) && lut[ix]!=1) {
    if (rounding==T && !is.numeric(digits)) {
      sistring <- paste(round(number/lut[ix]), pre[ix])
    }
    else if (rounding == T || is.numeric(digits)) {
      sistring <- paste(signif(number/lut[ix], digits), pre[ix])
    }
    else {
      sistring <- paste(number/lut[ix], pre[ix])
    } 
  }
  else {
    sistring <- as.character(number)
  }
  return(sistring)
}

