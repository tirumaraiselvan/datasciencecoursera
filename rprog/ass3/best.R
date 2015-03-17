best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("data/outcome-of-care-measures.csv", na.strings ="Not Available")
  if(!is.element(state, data$State))
     stop("invalid state")
     
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia")))
    stop("invalid outcome")
  
  index = list(`heart attack` = 11, `heart failure` = 17, `pneumonia` = 23)
  outindex = as.numeric(index[outcome])
  
  sdata <- data[data$State == state, ]
  hospitals = sdata[sdata[, outindex] == min(sdata[, outindex], na.rm = TRUE), 2]
  as.character(sort(hospitals)[1])
  
}