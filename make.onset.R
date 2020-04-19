make.onset <- function(data, trigger, condition, duration, weight) {
  onset_file <- vector(mode="list", length(data))
  onset_file_omit <- vector(mode="list", length(data))
  for(i in 1:length(data)){
    #make onset column: put from scanner time into real time
    cond_onset <- (data[[i]][trigger] - data[[i]][condition])/1000
    #make duration column
    if(isTRUE(is.numeric(duration))){
      #if argument is a number, repeat for length of onset file
      dur <- rep(duration, length(cond_onset))
    }
    else if(isTRUE(is.list(duration))){
      #if argument is a list, copy variable to onset file
      dur <- duration[[i]]
    }
    else{
      print(paste(i, "Warning: Duration must be specified as a single number or list"))
    }
    #make weight column
    wgt <- rep(weight, length(cond_onset))
    #put together in a dataframe
    onset_file[[i]] <- data.frame(cond_onset, dur, wgt)
    #omit rows with value NA
    onset_file_omit[[i]] <- na.omit(onset_file[[i]])
    if(isTRUE(all.equal(onset_file_omit[[i]],onset_file[[i]]))){
    } else {
      print(paste(i, "Warning: NAs were removed from onset file"))
    }
  }
  #name each column based on iteration of data list
  names(onset_file_omit) <- names(data)
  #name each column in each list element
  colnames <- c("Onset", "Duration", "Weight")
  onset_file_done <- lapply(onset_file_omit, setNames, colnames)
  return(onset_file_done)
}