# Function to determine if there are any genes on the Y chromosome among the top +/- correlations

any.Y <- function(in.Data,lim1,lim2)  {
     
     doY <- c(24)
     
     if (is.na(match('Y',in.Data$loc[lim1:lim2]))) {
          
          doY <- c(23)
     } else {
     }
     
     return(doY)
}