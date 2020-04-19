setwd("~/Desktop/R_Scripts") #sets where to find the files


#### function to read in data ####
#reading in data: as function
make.datalist <- function(pattern, file){
  #makes list of files that meet the pattern
  filelist=list.files(pattern=pattern)
  #drops file extension
  filenames <- gsub('.{4}$','',filelist)
  #puts files into a list object
  if(isTRUE(file=="csv")) {
  d=lapply(filelist, function(x)read.csv(x, header=T)) }
  else if(isTRUE(file=="txt")){   
  d=lapply(filelist, function(x)read.delim(x, header=T)) }
  else {
    print("Warning: data must be txt or csv format")}
  #adds names from filenames to elements of list
  names(d) <- paste(filenames)
  return(d)
}

readtest2 <- make.datalist("*Enc1.txt", "txt")
readtest4 <- make.datalist("*Enc1.xlsx", "xlsx")
readtest3 <- make.datalist("*Enc1.csv","csv")


#### function for making duration ####
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

#test above function
test <- vector(mode="list", length = 2)
test <- make.duration(readtest2, "Vividness.OnsetTime", "Vividness.RTTime")
test2 <- make.duration(readtest3, "Vividness.OnsetTime", "Vividness.RTTime")

#### function for onset files ####
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

#test above function
cond_listtest2 <- vector(mode="list", length=length(readtest3))
cond_listtest2 <- make.onset(readtest3, "Trigger.RTTime", "Fixation.OnsetTime", 2, 1)
cond_listtest_withdur <- vector(mode="list", length=length(readtest3))
cond_listtest_withdur <- make.onset(readtest3, "Trigger.RTTime", "Fixation.OnsetTime", test2, 1)

