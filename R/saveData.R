#' @title Write data product output
#'
#' @description This function will output a file that is used for dissemination
#' of the data products produced for the research document.
#'
#' @param filename a character string indicating the name of the file.
#' @param destdir Optional string indicating the directory in which to store downloaded files.
#' If not supplied, "." is used, i.e. the data file is stored in the present working directory.
#' @param dateCreated a POSIX format date indicating the time of creation, the
#' default is set using the `presentTime`.
#' @param contactName a character string indicating the person of contact
#' @param contactEMail a character string indicating the contact name's e-mail address.
#' @param description an optional character string providing some details of the data product.
#' @param otherMetadata an optional list indicating additional metadata to provide in the
#' header of the file. The list names should be provided in camelCase.
#' @param data a data.frame of the data that is to be put into the file with appropriate
#' column names, again, prefferably in camelCase
#' @param ndigits the number of digits to output the data, default is 2
#'
#' @author Chantelle Layton
#'
#' @importFrom utils write.table
#'
#' @export
#'

writeProductData <- function(filename, destdir = '.',
                             dateCreated = presentTime(),
                             contactName, contactEMail,
                             description = NULL, otherMetadata = NULL,
                             data, ndigits = 1){
  if(missing(filename)){
    stop('Please provide a filename')
  }
  if(missing(contactName) | missing(contactEMail)){
    stop("Please provide a person of contact and their e-mail address")
  }

  # set up header
  fnameline <- paste0("# ", getMetadata('filename'), ': ', filename)
  dateline <- paste0("# ", getMetadata('dateCreated'), ': ', dateCreated)
  contactline <- paste0("# ", getMetadata('contact'), ': ', contactName, " <", contactEMail, ">")
  descripline <- paste0("# ", getMetadata('description'), ': ', description) # might have to think about translation
  # other meta lines if provided
  if(!is.null(otherMetadata)){
    otherMetaLines <- unlist(lapply(1:length(otherMetadata),
                                    function(k) {paste0("# ", getMetadata(names(otherMetadata)[k]), ': ', otherMetadata[[k]])}))
  }
  # join all the header lines together
  header <- c(fnameline,
              dateline,
              contactline,
              descripline,
              if(!is.null(otherMetadata)){otherMetaLines})

  dataHeader <- enc2utf8(unlist(lapply(names(data), function(k) getData(k))))
  names(data) <- dataHeader
  headerutf8 <- enc2utf8(header)
  con <- file(paste(destdir, filename, sep = '/'), open = "w+", encoding = "UTF-8") # start a connection with UTF-8 encoding
  writeLines(header, con, useBytes = TRUE) # useBytes = TRUE for UTF-8 encoding
  write.table(format(data, digits = ndigits, scientific = FALSE), file = con,
              sep = ',', na = '', row.names = FALSE, col.names = TRUE,
              append = TRUE, quote = FALSE)
  close(con)
}
