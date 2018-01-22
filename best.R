best <- function(state, outcome) {
        ## Read outcome data
        setwd("E:/R/coursera/Assignments/ProgrammingAssignment3")
        outcome_data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        outcome_state <- unique(outcome_data[,7])
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        if (!state %in% outcome_state) {
                stop("Invalid State")
        } else if(!outcome %in% valid_outcomes){
                stop("Invalid Outcome")
        } else {
        ## Return hospital name in that state with lowest 30-day death
                best_hosp <- get_hosp(outcome_data, outcome, state)
        }
        print(best_hosp)
}


get_hosp <- function(data,outcome,state) {
        #rating the hospitals
        state_hosp <- data[data$State == state, ]
        if(outcome == "heart attack") {
                col_order <- order(
                        state_hosp$
                                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                        )
        }else if (outcome == "heart failure") {
                col_order <- order(
                        state_hosp$
                                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                        )
        } else if (outcome == "pneumonia") {
                col_order <- order(
                        state_hosp$
                                Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                        )
        }
        state_hosp1 <- state_hosp[col_order, 2]
        return(state_hosp1[1])
}

