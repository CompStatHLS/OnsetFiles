#only used when you want to subtract one column from another!
make.duration <- function(data, col1, col2){
  duration_list <- vector(mode="list", length=length(data))
  for(i in 1:length(data)){
    #make vector of durations
    duration <- (data[[i]][col2] - data[[i]][col1])/1000
    #check for negative values, return "NA" in place of negatives
    omitneg <- function(x){ifelse(x< 0, "NA",x)}
    dur_omit <- sapply(duration, omitneg)
    #check for NAs, and return warning if any NAs found
    if(isTRUE(all.equal(duration,dur_omit))){
    } else {
      print(paste(i, "Warning: Negative values calculated and set to NA"))
    }
    duration_list[[i]] <- as.numeric(dur_omit)
    #name each column based on iteration of data list
    names(duration_list) <- names(data)
  }
  return(duration_list)
}