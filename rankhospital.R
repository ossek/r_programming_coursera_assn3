## ranks hospitals by given outcome for a particular state
rankhospital <- function(state, outcome, num="best")
{
    consts <- initConsts()
    outcomeData <- prepOutcomeData(consts)

    ## these will throw via 'stop' function for errors
    validateStateInput(consts,state,outcomeData)
    validateOutcomeInput(consts,outcome)

    outcomeColumn <- consts$outcomeColumnNames[[outcome]] 
    deathRatesForOutcome <- outcomeData[,c(consts$stateName,consts$hospitalName,outcomeColumn)]
    completeCases <- deathRatesForOutcome[complete.cases(deathRatesForOutcome),]
    deathRatesInState <- completeCases[completeCases[,consts$stateName] == toupper(state),]
    orderedRatesInState <- deathRatesInState[order(deathRatesInState[,outcomeColumn],deathRatesInState[,consts$hospitalName]),]

    if(is.numeric(num))
    {
        orderedRatesInState[num,consts$hospitalName]
    }
    else if (tolower(num) == "best")
    {
        orderedRatesInState[1,consts$hospitalName]
    }
    else if (tolower(num) == "worst")
    {
        orderedRatesInState[nrow(orderedRatesInState),consts$hospitalName]
    }
    else
    {
        NA
    }
}

initConsts <- function()
{
    hospitalName <- "hospital.name"
    stateName <- "state"
    validOutcomeInputs <- c("heart attack", "heart failure", "pneumonia")
    ##outcome column names
    heartAttack <-  "hospital.30.day.death..mortality..rates.from.heart.attack" 
    heartFailure <- "hospital.30.day.death..mortality..rates.from.heart.failure"    
    pneumonia <- "hospital.30.day.death..mortality..rates.from.pneumonia"
    ## map outcome param input to outcome column name
    outcomeColumnNames <- c(heartAttack,heartFailure,pneumonia)
    names(outcomeColumnNames) <- validOutcomeInputs

    list(
         validOutcomeInputs = validOutcomeInputs,
         outcomeColumnNames = outcomeColumnNames,
         hospitalName = hospitalName,
         stateName = stateName
         )
}

prepOutcomeData <- function(consts = initConsts())
{
    ## Read outcome data.  It is essential that we use colClasses="character" 
    ## so that later we can properly convert decimal value columns
    outcomeData <-  read.csv("../outcome-of-care-measures.csv",colClasses="character")

    ## lower case all column names of the data frame for simplicity
    names(outcomeData) <- sapply(names(outcomeData),tolower)

    #coerce column so we can eliminate NA
    for(outcomeColumnName in consts$outcomeColumnNames)
    {
        outcomeData[,outcomeColumnName] <- as.numeric(outcomeData[,outcomeColumnName])
    }
    outcomeData
}

validateStateInput <- function(consts,state,outcomeData)
{
    ## Check that state and outcome are valid
    validStates <- levels(as.factor(outcomeData[,consts$stateName]))
    !(toupper(state) %in% validStates)
    if(is.null(state) || !(toupper(state) %in% validStates))
    {
        stop("invalid state")
    }
}

validateOutcomeInput <- function(consts,outcome)
{
    if(is.null(outcome) || !(tolower(outcome) %in% consts$validOutcomeInputs))
    {
        stop("invalid outcome")
    }
}
