#===============================================================================
#                                                                   GET FROM WEB
#===============================================================================
getFromWeb <- function(url, local.dir, save.as=NA, cache=TRUE, unzip=FALSE, 
                       unzip.path=NA, delete.zip=FALSE){
    # WARNING!!! - Not operational Yet
    #===========================================================================
    # Get a file from the internet and save it to local.dir 
    # You can rename the file to something more meaningful by setting save.as 
    # to a "myFile.csv" for instance. 
    # Caching stops the need to have to download the file everytime you run the 
    # script.
    # If you are downloading a zip you can get it to unzip the file 
    # automatically, you can specify that the contents be extracted in a 
    # separate directory using unzip.path. 
    # you can also specify that the zip file be deleted, so you can keep just 
    # the extracted contents. 
    #===========================================================================
    #TODO: Need to rename the variables to match the function arguments. 
    zipFile = "file.zip"
    dataDirectory = "data"
    dataFile = "file.csv"
    dataFilePath = paste(dataDirectory, dataFile, sep="/")
    
    
    # Checks if the csv data has already been extracted
    if(!file.exists(dataFilePath)){
        # Create the data directory if not already present
        if(!file.exists(dataDirectory)){
            message("Creating the directory '", dataDirectory, "'.")
            dir.create(dataDirectory)
        }
        # Download the zip file from URL if its not already in working directory
        if (!file.exists(zipFile)) {
            message("Downloading ", url)
            download.file(url, destfile = zipFile, mode="wb")
        }
        # unzip the contents of the zip file into data directory
        message("Unzipping the contents of ", zipFile, " into ", dataDirectory)
        unzip(zipFile, exdir=dataDirectory)
    }
}