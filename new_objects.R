# Program to determine which new objects were created in an R session so that the ones that are temporary can be removed.


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

source('/Volumes/Macintosh_HD_3/genetics/genenetwork2/func_new_objects.R')

# STEP 1: Uses function new.objects to get the objects that have been added since last rm operation.

# objects.to.edit <- new.objects(ls(all.names=TRUE,sorted=TRUE),objects.to.keep)
     
     # OR after initial use of this program, read in file persisting_objects.txt

fileConn <- file("persisting_objects.txt")
     objects.to.edit <- readLines(con = fileConn)
close(fileConn)


# STEP 2
fileConn <- file("edit_these_objects.txt")
     writeLines(objects.to.edit, fileConn)
close(fileConn)

# STEP 3: Edit the file edit_these_objects.txt by DELETING the objects that you don't want to keep.

# STEP 4: Read in edit_these_objects.txt as objects.to.keep and compute the setdiff with the original objects.to.edit to get the objects to remove.

fileConn <- file("edit_these_objects.txt")
     objects.to.keep <- readLines(con = fileConn)
close(fileConn)

# get the indices of the objects in objects to edit that are also in objects.to.keep 
     inverse.remove.indx <- which(objects.to.edit %in% objects.to.keep)
# a sequence with the indices of all objects in the original objects.to.edit
     seq.all.objects <- seq(1,length(objects.to.edit),1)

# Indices of the objects to remove from objects.to.edit
     objects.to.remove <- objects.to.edit[c(setdiff(seq.all.objects,inverse.remove.indx))]

# STEP 5: Remove these objects from memory
     rm(list=objects.to.remove)

# Step 6: write out the list of objects that should persist
     fileConn <- file("persisting_objects.txt")
          writeLines(ls(all.names=TRUE,sorted=TRUE), fileConn)
     close(fileConn)
     
# Repeat from STEP 1

