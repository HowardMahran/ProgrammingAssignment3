best <- function(state, outcome){
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
        
        ## Retrieve the full column name from the lookup
        fullColumnName <- lookup[which (lookup == outcome),2]
        
        ## Get the data for the state from the raw data
        dataForState <- rawdata[rawdata$State==state,]
        
        ## Find the index row with for the minimum value in the fullColumnName column
        index <- which.min(as.double(dataForState[,fullColumnName]))
        
        ## Use the index to find the Hospital Name in the row 
        best <- dataForState[index,"Hospital.Name"]
        
        ## Return the best hospital name 
        return(best)
     
}