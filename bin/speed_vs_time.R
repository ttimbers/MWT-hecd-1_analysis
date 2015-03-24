## Speed over time plot and statistics

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  file <- args[1]
  
  MWT.df <- read.table(file, header = TRUE)
  
  ## choreography for this analysis was called in this order: DsbMme
  ## using Parse.R, column 1 of merged.file was parsed into strain,
  ## temp, date, plate and time
  
  ## rename columns
  colnames(MWT.df) <- c("strain","temp", "date", "plate","time", "ID", "speed", "bias", "morphwidth", "midline", "area")
  
  ## make time numeric
  ##MWT.df$time <- as.numeric(levels(MWT.df$time))[MWT.df$time]
  
  ## plot speed versus time
  plot.speed.v.time(MWT.df$strain,MWT.df$temp, MWT.df$time,MWT.df$speed)
}

plot.speed.v.time <- function(strain, temp, time, speed) {
  ## get a list of strains to be plotted
  strains.list <- unique(strain)
  
  ## make a data frame from the inputted data
  df <- data.frame(strain, temp, time, speed)
  
  ##bin into time intervals to make it quicker to plot (average speed over every 30s for 10 min)
  cut1 <- cut(time, breaks=seq(0, 700, by = 20))
  df.int <- df
  df.int$time <- cut1
  times <- as.character(df.int$time)
  
  ## rename bins
  library(stringr)
  int_pattern <- ",[0-9]{1,}"
  int <- str_extract(times,int_pattern)
  int <- sub(",", "", int)
  int <- as.numeric(int)
  df.int$time <- int
  
  ## summarize speed (mean) for each time bin over strain and temperature
  library(plyr)
  df.int <- ddply(df.int,.(strain, temp, time), summarise, speed = mean(speed), N=length(speed),sd=sd(speed), se=sd/sqrt(N))  
  
  ##get rid of data from 0-40s of the experiment (sometimes the tracker doesn't start tracking 
  ##until 15s into the experiment)
  df.int <- df.int[which(df.int$time>40),]
  
  ##make plot
  ##plot the points
  plot.new()
  for (i in 1:length(strains.list)) {
    if (i < 2) {
      print(i)
      plot(df.int$time[which(df.int$strain==strains.list[i])], df.int$speed[which(df.int$strain==strains.list[i])], pch=18, xlab="Time(s)",ylab="Speed(mm/s)", ylim = c(0,1.4))
      ##plot the error bars (standard error) ## error bars not plotting... this is because sd and se in df.int are NA... need to fix
      segments(df.int$time[which(df.int$strain==strains.list[i])],df.int$speed[which(df.int$strain==strains.list[i])]-df.int$se[which(df.int$strain==strains.list[i])],df.int$time[which(df.int$strain==strains.list[i])],df.int$speed[which(df.int$strain==strains.list[i])]+df.int$se[which(df.int$strain==strains.list[i])])
    }
    else {
      print(i)
      points(df.int$time[which(df.int$strain==strains.list[i])], df.int$speed[which(df.int$strain==strains.list[i])],col=i+1,pch=18)
      segments(df.int$time[which(df.int$strain==strains.list[i])],df.int$speed[which(df.int$strain==strains.list[i])]-df.int$se[which(df.int$strain==strains.list[i])],df.int$time[which(df.int$strain==strains.list[i])],df.int$speed[which(df.int$strain==strains.list[i])]+df.int$se[which(df.int$strain==strains.list[i])])
    }
  }
}

compare.speed <- function(strain, time, speed){
  
}