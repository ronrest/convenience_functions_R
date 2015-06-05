

################################################################################
#                                                                      TEMPLATES
################################################################################
# Below are the templates that will be used to polulate the various files that 
# make up a shiny app. 

#-------------------------------------------------------------------------------
#                                                                    ui_template 
#-------------------------------------------------------------------------------
# This is the template for the text that will go in the ui.R file 
ui_template <- "library(shiny)\n
shinyUI(pageWithSidebar(
    headerPanel('Data science FTW!'),
    sidebarPanel(
        h3('Sidebar text')

    ),
    mainPanel(
        h3('Main Panel text')
    )
))
\n
"

#-------------------------------------------------------------------------------
#                                                                server_template 
#-------------------------------------------------------------------------------
# This is the template for the text that will go in the server.R file 
server_template <- "library(shiny)\n
#===============================================================================
#                                                               HELPER FUNCTIONS
#===============================================================================
# Whaatever functions, you need to calculated values, analysis, etc. 

# Loose code you put here will be called only ONCE when you do runApp(), unless 
# it is a function that gets called throughout the user intereaction. 

# This means that if you want values to persist across multiple sessions/page 
# refreshes, initialise the variables here globally eg: 
#     a <<- 45
# And assign new values in the session using the double arrows as well. 

#===============================================================================
#                                                           THE SERVER BEHAVIOUR
#===============================================================================
shinyServer(
    function(input, output) {
        #something here
        # Code inside here that is not inside a reactive statement will get 
        # called ONCE for every new user hit / page refresh. 
        
        # COde in eractive functions get called repeatedly as needed when values
        # are updated. 
    }
)
\n
"


################################################################################
#                                                                ACTUAL FUNCTION
################################################################################


#===============================================================================
#                                                                   CREATE SHINY
#===============================================================================
#' @title createShiny
#' @description Create shiny project. 
#' @details If you specify a value for the "name",  then it will create a 
#'          subdirectory of the same name which acts as the projects root 
#'          directory. Otherwise, it will use the current working directory 
#'          as the root directory of the project. 
#' @note This function has only been tested on Linux, it might not work on other 
#'       operating systems yet. 
#'
#' @param name (string) the name of the project. 
#' 
#'          If you do not provide a value, then it will use the current working 
#'          directory as the project's root directory. 
#'
#' @author Ronny Restrepo
#' @examples 
#' # Convert the current directory to a Shiny Project App
#' createShiny()
#' 
#' # Create a Shiny Project App called "myApp" in a subdirectory called "myApp" 
#' createShiny("myApp")
#'
#' @keywords shiny, app, shiny app
#' @export createShiny
#===============================================================================
createShiny <- function(name=NA){
    # TODO add option to use www/index.html instead of ui.R
    #
    # TODO: Check if the dir.sep value of "/" works on windows or mac. 
    #       Works fine on Linux. 
    
    dir.sep = "/"                      # The symbol used to separate directories
    
    #---------------------------------------------------------------------------
    # Creates a subdiretory with project name, otherwise  use current directory
    #---------------------------------------------------------------------------
    if (!is.na(name)){
        if(!dir.exists(name)){
            dir.create(name)
        }    
    } else {
        name = "."
    }
    
    #---------------------------------------------------------------------------
    #                                                           Create ui.R file 
    #---------------------------------------------------------------------------
    ui_file = paste(name, "ui.R", sep=dir.sep)
    if(!file.exists(ui_file)){
        file.create(ui_file)    
        write(ui_template, ui_file)
    } else {
        warning(name, "/ui.R already exists")
    }
    
    #---------------------------------------------------------------------------
    #                                                       Create server.R file 
    #---------------------------------------------------------------------------
    server_file = paste(name, "server.R", sep=dir.sep)
    if(!file.exists(server_file)){
        file.create(server_file)    
        write(server_template, server_file)
    } else {
        warning(name, "/server.R already exists")
    }
}



