# Program to determine which new objects were created in an R session so that the ones that are temporary can be removed.

# active_objects.txt is the file that contains the objects left after the last round of deleting unused objects.

new.objects <- function(xin,xcurr)  {
     
          new.ones <- setdiff(xin,xcurr)
     
     return(new.ones)

}
