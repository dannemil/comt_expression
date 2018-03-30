# function to determine mismatches on gene symbols when the ENTREZIDs match

mismatch.hgnc <- function(area.Data,tm.All.Data,entrez.Data)   {
     
     Symb.mismatches.temp <- data.frame(matrix(rep(NA,4*dim(area.Data)[1]),ncol=4))
     
     for (i in 1:dim(area.Data)[1])  {
          
          if (is.na(area.Data$Symbol[i]) | is.na(tm.All.Data$Symbol[entrez.Data[i]]))  {
               Symb.mismatches.temp[i,1] <- c(i)
               Symb.mismatches.temp[i,2] <- c('blank')
               Symb.mismatches.temp[i,3] <- area.Data$Symbol[i]
               Symb.mismatches.temp[i,4] <- tm.All.Data$Symbol[entrez.Data[i]]
               
          }  else if (!is.na(area.Data$Symbol[i]) & !is.na(tm.All.Data$Symbol[entrez.Data[i]]))  {
               
               if (area.Data$Symbol[i] == tm.All.Data$Symbol[entrez.Data[i]])      {
                    Symb.mismatches.temp[i,1] <- c(i)
                    Symb.mismatches.temp[i,2] <- c('match')
                    Symb.mismatches.temp[i,3] <- area.Data$Symbol[i]
                    Symb.mismatches.temp[i,4] <- tm.All.Data$Symbol[entrez.Data[i]]
                    
               }  else {
                    Symb.mismatches.temp[i,1] <- c(i)
                    Symb.mismatches.temp[i,2] <- c('mismatch')
                    Symb.mismatches.temp[i,3] <- area.Data$Symbol[i]
                    Symb.mismatches.temp[i,4] <- tm.All.Data$Symbol[entrez.Data[i]]
               }
               
          } else {
               
          }
          
     } # end 20,000 genes loop
     
     return(Symb.mismatches.temp)
     
}  # end function   
     