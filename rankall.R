rankall <- function (outcome, num = "best"){
    ## Read outcome data
    rawdata <- read.csv("outcome-of-care-measures.csv",  colClasses = "character", na.strings = "Not Available" )
    
    ## Create a lookup matrix where Column 1 has the short name, Column 2 has the full name
    outcomeLookup <- matrix(c("heart attack", "heart failure","pneumonia", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), nrow=3, ncol=2)
    
    ## Check that state and outcome are valid
    if(!outcome %in% outcomeLookup[,1] ){ 
        stop("invalid outcome") 
    }
    
    
    ## Return hospital name in each state with the given rank
    ## 30-day death rate
    
    
    ## Return hospital name in each state with the 30-day death
    ## rate rank is equal to the rank number (num)
    
    ## Retrieve the full column name from the lookup
    fullColumnName <- lookup[which (lookup == outcome),2]
    
    validStates = sort(unique(rawdata[,"State"]) )
    listOfStates <- NULL
    
    for( nextState in validStates){
       
        dataForState <- NULL
        SortedDataForState <-NULL
        rownum<- num
        
        ## Get the data for the state from the raw data
        dataForState <- rawdata[rawdata$State==nextState,]
    
        ## Sort the state data
    
        SortedDataForState <- dataForState[order(as.numeric(dataForState[[fullColumnName]]), dataForState[["Hospital.Name"]], decreasing = FALSE, na.last = NA),]
    
        ## If the rank is 1 or  "best" use the Best function to get the best ranking hospital
        if (num == "best" || num == 1) {
            x<- (best(nextState, outcome))
            listOfStates <- rbind(listOfStates,c(x, nextState ))
            next
        }   
        ## If the rank is "worst" then set num to the length of the data for the state (SortedataForState)
        if (num == "worst") { 
            rownum <- nrow(SortedDataForState)
        }
    
       ## Add the Hospital to the temp variable 'x' and then add the Hospital Name / State pair to the list of states (listOfStates)
        x<- SortedDataForState[as.integer(rownum), "Hospital.Name"]
        listOfStates<- rbind(listOfStates, c(x, nextState))
}
        ## Give the list colum names
        colnames(listOfStates) <- c("hospital","state")
       
        ## convert to a data.frame and return the listOfStates
        return(data.frame(listOfStates))
        
                 
}
