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
    print("Warning: data not txt or csv format")}
  #adds names from filenames to elements of list
  names(d) <- paste(filenames)
  return(d)
}

filelist=list.files(pattern="*Enc1.csv")

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
    #dur_omit <- duration
    omitneg <- function(x){ifelse(x< 0, "NA",x)}
    dur_omit <- sapply(duration, omitneg)
    #return warning if NAs found
    if(isTRUE(all.equal(duration,dur_omit))){
    } else {
      print(paste(i, "Warning: Negative values set to NA"))
    }
    duration_list[[i]] <- as.numeric(dur_omit)
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
  print(i, "Warning: Duration must be a single number or list")
  }
  #make weight column
  wgt <- rep(weight, length(cond_onset))
  #put together in a dataframe
  onset_file[[i]] <- data.frame(cond_onset, dur, wgt)
  }
  return(onset_file)
}

#test above function
cond_listtest2 <- vector(mode="list", length=length(readtest2))
cond_listtest2 <- make.onset(readtest2, "Trigger.RTTime", "Fixation.OnsetTime", 2, 1)
cond_listtest_withdur <- vector(mode="list", length=length(readtest2))
cond_listtest_withdur <- make.onset(readtest2, "Trigger.RTTime", "Fixation.OnsetTime", test2, 1)

