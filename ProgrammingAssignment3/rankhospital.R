rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    target.column.names = list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    target.column <- target.column.names[[outcome]]
    
    state.hospitals <- outcomes[outcomes[['State']] == state, ]  # get list of hospitals for the argument state
    
    # convert 30 day death rate column to numeric & order by that rate. Also, remove NA.
    state.hospitals[['death.rate.num']] <- suppressWarnings(as.numeric(state.hospitals[[target.column]]))
    ordered.state.hospitals <- state.hospitals[order(state.hospitals[['death.rate.num']],state.hospitals[['Hospital.Name']], na.last=NA ), ]
    
    # add a rank column
    rank.vector <- c(1:nrow(ordered.state.hospitals))
    ordered.state.hospitals[['Rank']] <- rank.vector
    
    # Check for best, worst and invalid rank
    number.of.ranked <- nrow(ordered.state.hospitals)
    
    if (num == "best") {
        num = 1L
    } else if (num == "worst") {
        num = number.of.ranked
    } else if (suppressWarnings(as.numeric(num)) > number.of.ranked) {  # num > number of ranked
        return(NA)
    } else {
        suppressWarnings(as.numeric(num))
    }
    
    required.hospital <- ordered.state.hospitals[ordered.state.hospitals[['Rank']] == num, ]
    required.hospital.name <- required.hospital[['Hospital.Name']]
    required.hospital.name
}

# Tests
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
