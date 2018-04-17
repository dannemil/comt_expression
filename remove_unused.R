# Remove variables and data that do not need to be included in the current environment


lslist <- noquote(ls())

fileConn<-file("remove_list.txt")
     writeLines(lslist, fileConn)
close(fileConn)

# Now edit the file by hand, read it back in, then execute the rm statement below.
##### Very important: remove the variables from the list that you want to keep. The ones that remain will be deleted.


toDelete <- noquote(readLines(con = "remove_list_edited.txt", n = -1L, ok = TRUE, warn = TRUE,
          encoding = "unknown", skipNul = FALSE))
toDel <- data.frame(delitems=toDelete)

rm(list=toDel$delitems)
