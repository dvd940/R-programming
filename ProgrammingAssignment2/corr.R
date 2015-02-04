corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    cr <- numeric(0)
    
    file.list <- list.files(directory, full.names=TRUE)  # Get list of all files
    num.of.files <- length(file.list)
    
    for (i in 1:num.of.files) {
        file.df <- read.csv(file.list[i])  # read in file (i)
        ok.df <- complete.cases(file.df)  # Get complete (OK) cases 
        ok.sulfate <- file.df$sulfate[ok.df]  # Sulfate values for OK cases
        ok.nitrate <- file.df$nitrate[ok.df]  # Nitrate values for OK cases
        num.of.ok <- length(ok.sulfate)  # numer of OK cases
        
        # Check if number of OK values > threshold and if yes, get corrolation. 
        if (num.of.ok > threshold) {  
            corrolation <- cor(ok.sulfate, ok.nitrate)
            cr <- c(cr, corrolation)  # Append corrolation to cr vector
        }
    }
    cr
    
}