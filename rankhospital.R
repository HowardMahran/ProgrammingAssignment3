rankhospital <- function (state, outcome, num = "best"){
    ## Read outcome data
    rawdata <- read.csv("outcome-of-care-measures.csv",  colClasses = "character", na.strings = "Not Available" )

    ## Create a lookup matrix where Column 1 has the short name, Column 2 has the full name
    outcomeLookup <- matrix(c("heart attack", "heart failure","pneumonia", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), nrow=3, ncol=2)
    
    ## Check that state and outcome are valid
    if(!outcome %in% outcomeLookup[,1] ){ 
        stop("invalid outcome") 
    }
    
    validStates = unique(rawdata[,"State"])
    if (!state %in% validStates) { 
        stop("invalid state") 
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    

    ## Return hospital name in that state with the 30-day death
    ## rate rank equal to the rank number (num)
    
    ## Retrieve the full column name from the lookup
    fullColumnName <- lookup[which (lookup == outcome),2]
    
    ## Get the data for the state from the raw data
    dataForState <- rawdata[rawdata$State==state,]
    
    ## Sort the state data
    
    SortedDataForState <- dataForState[order(as.numeric(dataForState[[fullColumnName]]), dataForState[["Hospital.Name"]], decreasing = FALSE, na.last = NA),]
    
    ## If the rank is 1 or  "best" use the Best function to get the best ranking hospital
    if (num == "best" || num == 1) {
        return (best(state, outcome))
    }   
    ## If the rank is "worst" then set num to the length of the data for the state (SortedataForState)
    if (num == "worst") { 
        num <- nrow(SortedDataForState)
    }
    
    
    SortedDataForState[as.integer(num), "Hospital.Name"]
    
}