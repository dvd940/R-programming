complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    
    file.list <- list.files(directory, full.names=TRUE)  # Get list of all files
    id.len = length(id)  # Find the length of the id argumemnt so we can create an empty data frame.
    df <- data.frame(id = numeric(id.len), nobs = numeric(id.len))  # Create empty data frame.
    index = 1  # initialize our index counter.
    
    # Read in the required files (from id) and add to a dataframe called df
    for (i in id) {
        file.df <- read.csv(file.list[i])  # read in file (i)
        nobs <- sum(complete.cases(file.df))  # Get completed cases for that file
        df$id[index] = i  # add the monitor number to the id column
        df$nobs[index] = nobs  # add the nobs to the nobs column
        index = index + 1
        }
    
    df   
    

}