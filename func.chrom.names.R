# Function to extract chromosome names from oddly formatted strings

get.Chr.names <- function (input.names) {
     
     temp.A <- sub('CHR_HSCHR','',input.names)
     temp.Init <- grep('^[X-Y]|^[1-9]?|^[1]{1}[0-9]{1}|^[2]{1}[0-2]{1}[^:punct:]',temp.A,value=TRUE)
     temp.Init.pf <- substr(temp.Init,1,2)
     temp.Init.pf <- sub('\\_','',temp.Init.pf)
     temp.Init.pf[which(temp.Init.pf == 'CH')] <- NA
     temp.Init.pf[which(temp.Init.pf == 'KI')] <- NA
     
     return(temp.Init.pf)
}