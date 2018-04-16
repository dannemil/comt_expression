read.new.Prog <- function() {

  new.Comment <- c("")

  new.Status <- readline(prompt="Is this a new program. Please enter TRUE or FALSE  ")

  if (new.Status == "TRUE") {

    new.Comment <- readline(prompt="Please enter a brief description of this program.  ")

  } else if (new.Status == "FALSE") {



  } else {

  }
  return(new.Comment)
}
