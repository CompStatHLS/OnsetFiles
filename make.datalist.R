#reading in data
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