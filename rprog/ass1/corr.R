corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  res = complete(directory)
  filterres = which(res$nobs > threshold)
  cor_vector = numeric(length(filterres))
  iter = 0
  for (i in filterres) {
    iter = iter + 1
    FilePath = paste(directory, sprintf("/%.3i.csv",i), sep="") 
    ## print(FilePath)
    data = read.csv(FilePath, header = TRUE )
    cor_vector[iter] = cor(data$nitrate, data$sulfate, use = "complete")
  }
  
  return(cor_vector)
}
