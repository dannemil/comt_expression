# Program that returns a unique program number
library(digest)
unique.ID <- function(xstring) {
  
  return <- digest(xstring)
  
}