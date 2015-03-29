## Reads outcome-of-care-measures csv file and returns char vector with name of 
## hospital that contains best (lowest) 30-day mortality rate for the given outcome in that state.
## Outcome options are "heart attack", "heart failure", "pneumonia".
## Hospitals lacking data for a certain outcome are excluded when considering that outcome.
## If there is a tie for an outcome, the first hospital alphabetically is returned.
best <- function(state,outcome)
{
    ## Read outcome data.  It is essential that we use colClasses="character" 
    ## so that later we can properly convert decimal value columns
    outcomeData <-  read.csv("../outcome-of-care-measures.csv",colClasses="character")
    
    ## lower case all column names of the data frame for simplicity
    names(outcomeData) <- sapply(names(outcomeData),tolower)

    ##outcome column names
    heartAttack <-  "hospital.30.day.death..mortality..rates.from.heart.attack" 
    heartFailure <- "hospital.30.day.death..mortality..rates.from.heart.failure"    
    pneumonia <- "hospital.30.day.death..mortality..rates.from.pneumonia"

    hospitalName <- "hospital.name"
    stateName <- "state"

    ## Check that state and outcome are valid
    validStates <- levels(as.factor(outcomeData[,stateName]))
    !(toupper(state) %in% validStates)
    if(is.null(state) || !(toupper(state) %in% validStates))
    {
        stop("invalid state")
    }

    validOutcomeInputs <- c("heart attack", "heart failure", "pneumonia")
    if(is.null(outcome) || !(tolower(outcome) %in% validOutcomeInputs))
    {
        stop("invalid outcome")
    }
    ## map outcome param input to outcome column name
    outcomeColumnNames <- c(heartAttack,heartFailure,pneumonia)
    names(outcomeColumnNames) <- validOutcomeInputs

    
    ## Return hospital name in that state with lowest 30-day death rate
    ## ensuring conversion of the column to numeric and removing NA also
    outcomeColumn <- outcomeColumnNames[[outcome]] 
    #coerce column so we can eliminate NA
    outcomeData[,outcomeColumn] <- as.numeric(outcomeData[,outcomeColumn])
    deathRatesForOutcome <- outcomeData[,c(stateName,hospitalName,outcomeColumn)]
    completeCases <- deathRatesForOutcome[complete.cases(deathRatesForOutcome),]
    deathRatesInState <- completeCases[completeCases[,stateName] == state,]
    deathRatesInState[deathRatesInState[,outcomeColumn] == min(deathRatesInState[,outcomeColumn]),hospitalName]
}
