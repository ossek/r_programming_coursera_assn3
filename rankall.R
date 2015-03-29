rankall <- function(outcome, num = "best") {
    consts <- initConsts()
    outcomeData <- prepOutcomeData(consts,state,outcome)

    ## these will throw via 'stop' function for errors
    validateOutcomeInput(consts,outcome)

    #state, hospital name, outcome
    outcomeColumn <- consts$outcomeColumnNames[[outcome]] 
    deathRatesForOutcome <- outcomeData[,c(consts$stateName,consts$hospitalName,outcomeColumn)]
    completeCases <- deathRatesForOutcome[complete.cases(deathRatesForOutcome),c(consts$stateName,consts$hospitalName,outcomeColumn)]
    outcomeByState <- split(completeCases,as.factor(completeCases[,stateName]))
    orderedOutcomeByState <- lapply(outcomeByState,function(grp){grp[order(grp[,outcomeColumn],grp[,consts$hospitalName]),]})
    
    ## restr <- lapply(seq_along(rr),function(i){list(stater=names(rr)[i],peww=rr[i][[1]][,pneu])})


    if(is.numeric(num))
    {
        ranked <- lapply(orderedOutcomeByState,function(grp){grp[num,]})
        ## make the row structure we want, giving state name even when there is no data found.
        ## this makes a list of list. think of the nested lists like data structures whose names
        ## will be used in the final data frame.
        rankedList <- lapply(seq_along(ranked),function(i){list(hospital=ranked[i][[1]][,consts$hospitalName],state=names(ranked)[i])})
        ## use do.call with rbind to rbind across a list 
        ranked <- data.frame(do.call(rbind,rankedList))
        ranked
    }
    else if (tolower(num) == "best")
    {
        ranked <- lapply(orderedOutcomeByState,function(grp){grp[1,]})
        rankedList <- lapply(seq_along(ranked),function(i){list(hospital=ranked[i][[1]][,consts$hospitalName],state=names(ranked)[i])})
        ranked <- data.frame(do.call(rbind,rankedList))
        ranked
    }
    else if (tolower(num) == "worst")
    {
        ranked <- lapply(orderedOutcomeByState,function(grp){grp[nrow(grp),]})
        rankedList <- lapply(seq_along(ranked),function(i){list(hospital=ranked[i][[1]][,consts$hospitalName],state=names(ranked)[i])})
        ranked <- data.frame(do.call(rbind,rankedList))
        ranked
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

prepOutcomeData <- function(consts,state, outcome)
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
