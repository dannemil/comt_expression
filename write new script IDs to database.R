# This function writes the current program ID, filename, date and file description to the program database
  # It does this only for new scripts

write.New <- function(script.In) {
  # Get the descriptors of the script
  output.Dir <- c("/Users/dannemil/paperless/ase_manuscript/sagmb/R_files/01final_results/ase_project/output/")

dfRead <- data.frame()
file.Augment <- data.frame()
nonDuplicate <- data.frame()



  #write.csv(script.In, file = paste(working.Dir,"id_file_database.csv",sep=""),row.names=FALSE)
  dfRead<-read.csv(paste(output.Dir,"ase project R script database.csv",sep=""),header=TRUE,stringsAsFactors=FALSE) # read the existing file
  #dfRead$currentDate <- format(as.Date(dfRead$currentDate),"%m-%d-%y")
  #script.In$currentDate <- format(as.Date(script.In$currentDate),"%m-%d-%y")
#  file.Augment<-rbind(dfRead[,1:3], script.In) # rbind both data.frames
  file.Augment<-rbind(dfRead[,1:3], script.In) # rbind both data.frames
  # get only the non duplicate rows from the new data.frame
  nonDuplicate <- file.Augment[!duplicated(file.Augment)&c(rep(FALSE, dim(dfRead)[1]), rep(TRUE, dim(script.In)[1])), ]
  print(nonDuplicate)
  # append the file with the non duplicate rows

  if (dim(nonDuplicate)[1] > 0) {
  nonDuplicate <- data.frame(nonDuplicate,currentDate = format(Sys.Date(), "%m-%d-%y"))
  } else {

  }
  return(nonDuplicate)
}
