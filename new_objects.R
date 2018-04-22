# Program to determine which new objects were created in an R session so that the ones that are temporary can be removed.


work.path <- c('/Volumes/Macintosh_HD_3/genetics/genenetwork2/')

setwd(work.path)

# Include sourced programs here.
source.prog <- data.frame(rcode='func_cbind_na',
                          'func_rbind_na',
                          'func_new_objects'
)

source(paste(work.path,source.prog$rcode,'.R',sep=''))


# Generate a unique ID for this program by path and filename. This unique ID is attached to this file using a tag in the filesystem.
     
     source('unique_id_generator.R')

fname <- csf()   # function to get current path including file name

prog.Name <- unique.ID(fname)

prog.Name
##############################

# persisting_objects.txt is the file that contains the objects left after the last round of deleting unused objects.

#objects.to.keep <- matrix(c('initialization'),ncol=1)

# fileConn <- file("active_objects.txt")
#      objects.to.edit <- readLines(con = fileConn)
# close(fileConn)


# STEP 1: Uses function new.objects to get the objects that have been added since last rm operation.

# objects.to.edit <- new.objects(ls(all.names=TRUE,sorted=TRUE),objects.to.keep)
     
     # OR after initial use of this program, read in file persisting_objects.txt

fileConn <- file("persisting_objects.txt")
     persisting.objects <- readLines(con = fileConn)
close(fileConn)

# STEP 2: compare list of object currently in memory to persisting list 

     current.objects <- ls(all.names=TRUE,sorted=TRUE)
     objects.new <- new.objects(current.objects,persisting.objects)
     

# STEP 3: write out new objects to edit_these_objects.txt
fileConn <- file("edit_these_objects.txt")
     writeLines(objects.new, fileConn)
close(fileConn)

# STEP 4: Edit file edit_these_objects.txt by DELETING the objects that you don't want to keep.

file.edit("edit_these_objects.txt")

read.edit.done <- function()
{ 
     ans <- readline(prompt="Did you edit the file? (y/n) ")
     if (identical(ans,'n'))
     {
          file.edit("edit_these_objects.txt")
          return(read.edit.done())
     } else if (identical(ans,'y'))   {
          return('Continue to next step.')
     } else {
          cat('Please type y or n.')
          return(read.edit.done())
     }
}

read.edit.done()



# STEP 5: Read in edit_these_objects.txt as objects.to.keep and compute the setdiff with the original objects.new to get the objects to remove.

fileConn <- file("edit_these_objects.txt")
     objects.to.keep <- readLines(con = fileConn)
close(fileConn)

# get the indices of the objects in objects.new that are also in objects.to.keep 
     inverse.remove.indx <- which(objects.new %in% objects.to.keep)
# a sequence with the indices of all objects in the original objects.new
     seq.all.objects <- seq(1,length(objects.new),1)

# Indices of the objects to remove from objects.new
     objects.to.remove <- objects.new[c(setdiff(seq.all.objects,inverse.remove.indx))]

# STEP 6: Remove these objects from memory
     rm(list=objects.to.remove)

# STEP 7a: Append the objects.to.keep to the existing persisting.objects into new.persisting.objects and overwrite this out as persisting_objects.txt
     
     new.persisting.objects <- matrix(rep(NA,(length(persisting.objects)+length(objects.to.keep))))
     
     new.persisting.objects[1:length(persisting.objects)] <- persisting.objects
     new.persisting.objects[(1+length(persisting.objects)):(length(persisting.objects)+length(objects.to.keep))] <- objects.to.keep
     
     fileConn <- file("persisting_objects.txt")
          writeLines(ls(all.names=TRUE,sorted=TRUE), fileConn)
     close(fileConn)

# Repeat from STEP 1

