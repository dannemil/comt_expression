# Simple function to increment a counter

inc <- function(xin,bump)  {

     # default increment is 1 set with setGloablOptions(delta = 1) or simply opt(delta = 1)    
     if(missing(bump)) { delta <- opt('delta')} else { delta <- bump} # default increment is 1
     
     xout <- xin + delta

          return(xout)
}
