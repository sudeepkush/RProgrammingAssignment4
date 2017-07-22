

helper <- function(data, col_num, state) {
  state_subset <- data[data[, 7]==state, ]
  outcome_arr <- state_subset[, col_num]
  min <- min(outcome_arr, na.rm=TRUE)
  min_index <- which(outcome_arr == min)
  hosp_name <- state_subset[min_index, 2]
  return(hosp_name)
}


best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% data$State) {
    stop("invalid state")
  }else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  else{
    if(outcome == "heart attack"){
      hosp_name <- helper(data, 11, state)
    }
    else if(outcome == "heart failure"){
      hosp_name <- helper(data, 17, state)
    }else {
      hosp_name <- helper(data, 23, state)
    }
    result <- hosp_name
    return(result)
  }
    
}

num_helper <- function(data, col_num, state, num) {
  state_subset <- data[data[, 7]==state, ]
  # get "attack", "failure" and "pneumonia" vector
  outcome_arr <- state_subset[, col_num]
  len <- dim(state_subset[!is.na(outcome_arr), ])[1]
  if (num == "worst") {
    rank <- rank_helper(state_subset, outcome_arr, len)
  } else if (num > len) {
    rank <- NA
  } else {
    rank <- rank_helper(state_subset, outcome_arr, num)
  }
  result <- rank
  return(result)
}

rank_helper <- function(state_subset, outcome_arr, num) {
  result <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[num]]
  return(result)
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  # read the data file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  # change data type from character to numeric
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  } else {
    if (num == "best") {
      rank <- best(state, outcome)
    } else {
      if(outcome == "heart attack") {
        rank <- num_helper(data, 11, state, num) 
      } else if(outcome == "heart failure") {
        rank <- num_helper(data, 17, state, num) 
      } else {
        rank <- num_helper(data, 23, state, num) 
      }
    }
    result <- rank
    return(result)
  }
}

num_helper <- function(state_subset, col_num, num) {
  # get "attack", "failure" and "pneumonia" vector
  outcome_arr <- as.numeric(state_subset[, col_num])
  len <- dim(state_subset[!is.na(outcome_arr), ])[1]
  if (num == "best") {
    rank <- rank_helper(state_subset, outcome_arr, 1)
  } else if (num == "worst") {
    rank <- rank_helper(state_subset, outcome_arr, len)
  } else if (num > len) {
    rank <- NA
  } else {
    rank <- rank_helper(state_subset, outcome_arr, num)
  }
  result <- rank
  return(result)
}

rank_helper <- function(state_subset, outcome_arr, num) {
  result <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[num]]
  return(result)
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  # read the data file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  state_arr <- sort(unique(data$State))
  arr_len <- length(state_arr)
  hospital <- rep("", arr_len)
  
  if (!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  } else {
    for(i in 1:arr_len) {
      # loop for each state
      state_subset <- data[data[, 7]==state_arr[i], ]
      if(outcome == "heart attack") {
        hospital[i] <- num_helper(state_subset, 11, num) 
      } else if (outcome == "heart failure") {
        hospital[i] <- num_helper(state_subset, 17, num) 
      } else {
        hospital[i] <- num_helper(state_subset, 23, num) 
      }
    }
  }
  # create the data frame to return
  df <- data.frame(hospital=hospital, state=state_arr)
  result <- df
  return(result)
}
