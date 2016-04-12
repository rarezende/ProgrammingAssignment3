best <- function(state, outcome) {
    # Read hospital data
    hospData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check that input state is valid
    if(nrow(hospData[hospData$State==state,]) == 0) {
        stop("invalid state")
    }
    
    # Check that input outcome is valid
    if(outcome == "heart attack") {
        outcomeHeader <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if (outcome == "heart failure") {
        outcomeHeader <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else if (outcome == "pneumonia") {
        outcomeHeader <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    else {
        stop("invalid outcome")
    }
    
    # Filters hospData for input state
    stateData <- split(hospData, hospData$State)
    stateData <- stateData[[state]]
    
    # Order data frame by values of input outcome
    stateData[,outcomeHeader]<-suppressWarnings(as.numeric(stateData[,outcomeHeader]))
    stateData <- stateData[order(stateData[[outcomeHeader]], stateData$Hospital.Name),]

    
    ## Return hospital name in that state with lowest 30-day death rates
    stateData[1,"Hospital.Name"]
}
