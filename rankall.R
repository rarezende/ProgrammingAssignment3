rankall <- function(outcome, num = "best") {
    
    # Read hospital data
    hospData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
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
    
    countryData <- split(hospData, hospData$State)
    
    output <- data.frame(hospital = NA, state = rep(NA, length(names(countryData))))
    j <- 1L
    
    for(state in names(countryData)) {
    
        # Filters countryData for current state
        stateData <- countryData[[state]]
        
        # Order data frame by values of input outcome
        stateData[,outcomeHeader]<-suppressWarnings(as.numeric(stateData[,outcomeHeader]))
        stateData <- stateData[order(stateData[[outcomeHeader]], stateData$Hospital.Name),]
        
        # Clean-up of NA's in outcome column
        stateData <-stateData[!is.na(stateData[[outcomeHeader]]),]
        
        if (num == "best") {
            rank <- 1L
        }
        else if (num == "worst") {
            rank <- nrow(stateData)
        }
        else {
            rank <- num
        }
        
        output[j, "state"] <- state
        output[j, "hospital"] <- stateData[rank,"Hospital.Name"]
        
        j <- j + 1
    }
    
    output
}
