#===============================================================================
#                                                                 CACHE DOWNLOAD
#===============================================================================
#' cache_download
#' 
#' Downloads a file from the internet, and caches it locally. So future calls to 
#' the same function simply load the local file instead of downloading all over 
#' again. 
#' 
#' @param url (string) the URL to download from
#' @param dataDir (string) The directory where the local file will be 
#'        stored in
#' @param localName (string) The name you want the file to be called locally
#' @author Ronny Restrepo
#' @note This function has only been tested on Linux, it might not work on other 
#'       operating systems yet. 
#' @import curl
#' @export cache_download
cache_download <- function(url, dataDir, localName){
    require("curl")
    # TODO: Handle circumstances where dataDir is "" or "." or "./"
    # TODO: include option to override existing local file, eg, if there may be 
    #       reason to believe that the data has changed. 
    
    # TODO: Possibly modify handling of file separator, it may not work on Windows. 
    warning("This has only been tested on Linux so far, it may not work on ", 
            "other operating systems yet.")
    localPathToFile = paste(dataDir, localName, sep="/")
    
    #---------------------------------------------------------------------------
    #                     Create a new data directory if it doesnt already exist
    #---------------------------------------------------------------------------
    if(!file.exists(dataDir)){
        message("Creating a new data directory '", dataDir, "'")
        dir.create(dataDir)
    }
    
    #---------------------------------------------------------------------------
    #                     Download the file if it hasn't already been downloaded
    #---------------------------------------------------------------------------
    if(file.exists(localPathToFile)){
        dateDownloaded <- NA    # TODO: Load a datestamp from a file, so that it
        # can be loaded up on start up
        message("Using existing ", localPathToFile, " file downloaded on ",
                dateDownloaded)
    } else {
        message("Downloading and saving data as ", localPathToFile)
        download.file(url, destfile=localPathToFile, method="curl")
        
        # Create a datestamp of the time the download was made
        dateDownloaded <- date()
        message("Download made on ", dateDownloaded)
        # TODO: save the datestamp as a file, so it can be loaded up next time 
        #       the script is run
        message("TODO: save the datestamp as a file, so it can be loaded up next",
                "time the script is run")
    }
}

