# hospitalAnalysis is a exercise to do analysis about hospitalCompare database
# https://www.medicare.gov/hospitalcompare

## Plot the 30-day mortality rates for heart attack
mortalityHeartAttack <- function(){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    head(outcome)
    ncol(outcome)## quantity of columns
    names(outcome) ## columns name
    ##create an histogram from column 11 - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
    outcome[,11] <- as.numeric(outcome[,11])
    hist(outcome[,11])
}

checkState <- function(state, data){
   # verify if the state is valid 
    ok=FALSE
    state <- toupper(state)
    size <- nrow(data)
    x<- 1
    while(ok !=TRUE && x<= size){
         if(data[x,7]==state) {
            ok=TRUE
        }
        x <- x+1
     }
    if(!ok){
        stop("invalid state")
    }
}

checkOutcome <- function(outcome){
    # verify if the outcome is valid 
    outcome <- tolower(outcome)
    if(outcome !="heart attack" && outcome !="heart failure" && outcome !="pneumonia" ){
        stop("invalid outcome")
     } 
}

selectColumn <- function(outcome){
    if(outcome == "heart attack"){
        column <- 11
    } 
    else if(outcome == "heart failure"){
        column <-17
    } 
    else if(outcome == "pneumonia"){
        column <-23
    }
    column
}

##Finding the best hospital in a state
best<- function(state, outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    checkOutcome(outcome)
    checkState(state,data)
   
    ## Return hospital name in that state with lowest 30-day death
    column <- selectColumn(outcome)
    data <-  subset(data,data$State ==state)
    data[, column][data[, column] == "Not Available"] <- NA
    data <-data[!is.na(data[,column]),]
    data <- data[order( as.numeric(data[,column]), data[,2] ), ]
    result <- data[1,2]
    result
   
# Outcome can be: heart attack, heart failure, or pneumonia
# best("TX", "heart attack")
    
}

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    checkOutcome(outcome)
    checkState(state,data)
    
    ## Return hospital name in that state with the given rank
    
   
    column <- selectColumn(outcome)
    data <-  subset(data,data$State ==state)
    data[, column][data[, column] == "Not Available"] <- NA
    data <-data[!is.na(data[,column]),]
    data <- data[order(as.numeric(data[,column]), data[,2] ), ]
    
    ## 30-day death rate
    if(num=="best"){
        num <- 1
    }
    else if(num=="worst"){
         num <- length(data)
     }
    result <- data[num,2]
    result
    
#rankhospital("TX", "heart failure", 4)    
}



rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    checkOutcome(outcome)
    
    ## For each state, find the hospital of the given rank
    column <- selectColumn(outcome)
    
    ## Return a data frame with the hospital names and the
    data[, column][data[, column] == "Not Available"] <- NA
    data <-data[!is.na(data[column]),]
    data <- data[order( data[,7], as.numeric(data[,column]), data[,2] ), ]
    result <- aggregate(data, by=list(data[,7]), function(x){
        if(num=="best"){
            num <- 1
        }
        else if(num=="worst"){
            num <- length(x)
        }
        x[num]
        
    })
    ## state name
    result <- result[,c(3,1)]
    names(result) <- c("hospital","state")
    return(result)
    
#head(rankall("heart attack", 20), 10)   
}

