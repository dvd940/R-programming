best <- function(state, outcome) {
    ## Read outcome data    
    outcomes <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    valid.states <- outcomes[['State']]
    if (!state %in% valid.states) {
        stop("invalid state")
    }
    
    valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% valid.outcomes) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death    
    ## rate
    
    # Create list of target column names to use as an index depending on the outcome argument. 
    target.column.names = list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    target.column <- target.column.names[[outcome]]
    
    state.hospitals <- outcomes[outcomes[['State']] == state, ]  # get list of hospitals for the argument state
    
    # convert 30 day death rate column to numeric
    state.hospitals[['death.rate.num']] <- suppressWarnings(as.numeric(state.hospitals[[target.column]]))
    
    #Find the hospital with minimum death rate
    min.death.rate <- min(state.hospitals[['death.rate.num']], na.rm = TRUE)  
    best.hospital <- na.omit(state.hospitals[state.hospitals[['death.rate.num']] == min.death.rate, ])
    best.hospital.name <- best.hospital[['Hospital.Name']]
    best.hospital.name
}

# Tests
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
