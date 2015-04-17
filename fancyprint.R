
printkv <- function(key, val, sep=" = ", fill=0, fill_char=" ", round=FALSE){
    # Print a key value pair
    #  You can specify that the value be positioned at a certain minimum column 
    #  number, which can be used to line up multiple prinkv() function calls up. 
    #  You can also specity how the gap should be filled up, aswell as the 
    #  separator. 
    #
    # EXAMPLE:
    # >>> printkv("age", age, sep=": ", fill=10, fill_char=".") 
    # age.......: 34
    # 
    # ARGS:
    #  key, val, sep=" = ", fill=0, fill_char=" "
    #  fill   int, the number of columns to fill up before the separator is 
    #         displayed
    #  round: Integer, specifies the number of decimal places to round to. 
    #         If its set to FALSE (defalut value), then there will be no 
    #         rounding
    #
    # 
    # TODO: check that the inputs are of the correct data type.
    # TODO: consider looking at c-style formatting to make it more efficient, 
    #       esp when generating the gap.
    # TODO: Have a parameter "round", so that if the value argument is a numeric
    #       then it should round to that number of decimal places. 
    # 
    
    #-------------------------------------------------------------------------
    #                                 Fill with the necessary amount of spaces
    #-------------------------------------------------------------------------
    filler = ""
    length_key = nchar(key)
    gap = fill - length_key
    if (gap > 0){
        for (i in 1:gap) 
            filler = paste(filler, fill_char, sep="")
    }
    key = paste(key, filler, sep="")
    
    #-------------------------------------------------------------------------
    #                                                           Value ROunding
    #-------------------------------------------------------------------------
    if (round != FALSE){
        #TODO: make sure the value is of type numeric.
        val = round(val, digits=round)
    }
    
    #-------------------------------------------------------------------------
    #                                                     Print out the Result
    #-------------------------------------------------------------------------
    print(paste(key, val, sep=sep))
     
}
