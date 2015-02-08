# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking 
# (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates.

rankall <- function(outcome, num = "best") {
    ## Read outcome data    
    outcomes <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% valid.outcomes) {
        stop("invalid outcome")
    }
       
    #What column do we want to read?
    target.column.names = list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    target.column <- target.column.names[[outcome]]
    
        
    ## For each state, find the hospital of the given rank
    
    list.of.states <- sort(unique(outcomes[['State']]))  # Get list of states (sorted)
    list.of.hospitals <- vector()  # A vector to store our hospital names
    
    for (st in list.of.states) {  # cycle through our list of states and find the ranks
        
        state.hospitals <- outcomes[outcomes[['State']] == st, ]  # get list of hospitals for the argument state
        
        # convert 30 day death rate column to numeric & order by that rate. Also, remove NA.
        state.hospitals[['death.rate.num']] <- suppressWarnings(as.numeric(state.hospitals[[target.column]]))
        ordered.state.hospitals <- state.hospitals[order(state.hospitals[['death.rate.num']],state.hospitals[['Hospital.Name']], na.last=NA ), ]
        
        number.of.ranked <- nrow(ordered.state.hospitals)  # How many ranked hosptals for this state?
        
        #add a rank column
        rank.vector <- c(1:number.of.ranked)
        ordered.state.hospitals[['Rank']] <- rank.vector
        
        # Check for best, worst and invalid rank    
        if (num == "best") {
            target.num = 1L
        } else if (num == "worst") {
            target.num = number.of.ranked
        } else if (suppressWarnings(as.numeric(num)) > number.of.ranked) {  # num > number of ranked
            list.of.hospitals <- append(list.of.hospitals, NA)
            next
        } else {
            target.num <- suppressWarnings(as.numeric(num))
        }
        
        required.hospital <- ordered.state.hospitals[ordered.state.hospitals[['Rank']] == target.num, ]
        list.of.hospitals <- append(list.of.hospitals, required.hospital[['Hospital.Name']])  # Add name to hospital names vector
        
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    ranked.all <- data.frame(list.of.hospitals, list.of.states, row.names = list.of.states)
    names(ranked.all) = c("hospital", "state")
    return(ranked.all)
    
    
    
    
    

}



# Tests
# > head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
# > tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY
# Save your