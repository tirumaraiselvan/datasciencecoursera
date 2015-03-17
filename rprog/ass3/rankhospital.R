rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- read.csv("data/outcome-of-care-measures.csv", na.strings ="Not Available")
  if(!is.element(state, data$State))
    stop("invalid state")
  
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia")))
    stop("invalid outcome")
  
  index = list(`heart attack` = 11, `heart failure` = 17, `pneumonia` = 23)
  outindex = as.numeric(index[outcome])
  
  sdata <- data[data$State == state, ]
  
  if(num == "best")
    num = 1
  else if(num == "worst")
    num = sum(!is.na(sdata[,outindex]))
  
  if(num > nrow(sdata))
    return("NA")
  
  res <- order(sdata[,outindex], sdata[,2], na.last = NA)
  as.character( sdata$Hospital.Name[res[num]] )
      
}