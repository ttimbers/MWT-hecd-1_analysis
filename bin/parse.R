## Parse column 1 from merged.file (separate plate, data/time, strain, temperature and time into
## separate columns)
##

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  file <- args[1]
  
  ## parse the first columns of the plate_as_sample
  MWT.df <- read.table(file, header=FALSE)  
  MWT.df.parsed <- parse.column(MWT.df)
  write.table(MWT.df.parsed, file=paste(file,".parsed", sep=""), sep="\t", quote=FALSE, append=FALSE, row.names=FALSE)
}

parse.column <- function(df) {
  
  ## load stringr library
  library(stringr)
  
  ## put column 1 (data to be parsed) into a variable called col.one
  col.one <- df$V1
  
  ## make pattern for strain and extract the strain names
  strain.pattern  <- "[a-zA-Z]{1}[a-zA-Z0-9]{1,4}"
  strain.names  <- str_extract(col.one, strain.pattern)
  ## replace N2-1D- with N2
  strain.names  <- sub("hecd", "hecd-1", strain.names)
  
  ## make a pattern for temperature and extract the temps
  temp.pattern <- "adult-[0-9]{2}"
  temp.values <- str_extract(col.one, temp.pattern)
  temp.values <- sub("adult-", "", temp.values)
  
  ## make pattern for plate and extract the plate names
  plate.pattern  <- "[0-9_]{15}"
  plate.names  <- str_extract(col.one, plate.pattern)
  
  ## make a pattern for date and extract the date names
  date.pattern <- "[0-9]{8}"
  date.names  <- str_extract(col.one, date.pattern)
  
  ## make a pattern for time and extract the time
  time.pattern <- ":[0-9.]{1,}"
  time.values <- str_extract(col.one, time.pattern)
  time.values <- sub(":", "", time.values)
  
  new.df <- cbind(strain.names,temp.values,date.names,plate.names,time.values, df[,2:dim(df)[2]])
  return(new.df)
}

main()