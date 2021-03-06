library(lubridate)
library(lattice)
library(dplyr)
library(gridExtra)
library(ggplot2)
# The datafile is created by running sar -A > sar_full.out.
# There are many options to sar but beyond the scope of this
# example report.
# Read the input into a raw file first
# This will facilitate dealing with the non-data lines
#    3 lines of header 
#    1 line of footer (Averages)
sar_raw <- readLines("tmp/vm-helios-003.sar")

sar_title <- sar_raw[1] %>% strsplit("\t") %>% unlist()

report_date <- mdy(sar_title[2])
# Find all the "Averages" strings in the sar_raw. This
# will serve as delimiters to each stanza
sar_delimiter <- grep("Average", sar_raw)

# CPU Section ====
  header_pattern_cpu <- "CPU.*usr.*nice.*sys.*iowait"

  # Find the start of the CPU section, drop into a vector
  sar_cpu_headers <- grep(header_pattern_cpu, sar_raw)

  # The stanza will be delimited by the first occurence of the
  # section break and the first occurrence of the sar_delimiter
  # greater than or equal to sar_cpu_breaks[1]
  # First, select all entries in the sar_delimiter vector greater
  # than the first line of the stanza:
  cpu_firstline <- sar_cpu_headers[1]

  tmp1 <- sar_delimiter[] > sar_cpu_headers[1]

  # Next, return the first element which will be the delimiter for
  # that stanza...
  cpu_lastline <- sar_delimiter[tmp1][1]


  # tmp1 now holds the starting line
  # tmp2 now holds the endling line
  # The difference between these is the number of lines to read
  numLines <- cpu_lastline - cpu_firstline

  sar_cpu_raw <- sar_raw[cpu_firstline:cpu_lastline]

  # Now sar_cpu_raw contains just the CPU entries from the datafile.
  # Next, we need to filter out all the junk lines containing CPU
  # headers and the Averages line. 
  # We have a couple options: 
  # Since we already found sar_cpu_headers, we could re-use that vector
  # by applying some math to determine the new offsets in the file.
  # If there was lots of data then the payoff could be reduced search
  # time at the risk of some code that's a little more difficult to
  # follow. That is, we know the start/end of the segment in the original
  # data. If we subtract the starting offset from all entries in 
  # sar_cpu_headers, this will give the correct offsets in the filtered
  # dataset.
  #
  # The full output is less than 5000 lines so we opt to just re-create
  # the search results vector against the CPU-only dataset.
  
  sar_cpu_headers_2 <- grep(header_pattern_cpu, sar_cpu_raw)

  # The above returns a vector containing all lines that match 
  # header_pattern_cpu. We want everything but those lines:
  #
  # sar_cpu_raw <- sar_cpu_raw[-sar_cpu_headers_2]
  # But, we actually want to keep the first header line, so just select
  # the second occurrence down to the end of the file, determined by
  # the length() function.

  sar_cpu_raw <- sar_cpu_raw[-sar_cpu_headers_2[2:length(sar_cpu_headers_2)]]


  # We also do the same with lines containing "Average".
  # The grep() returns a vector so we can inline the code... 
 # FIX
  #  sar_cpu_raw <- sar_cpu_raw[-(grep("Average", sar_cpu_raw))]


  # Next, we clean up all those blank lines in the dataset.
  # The which() function will return a vector of lines matching a query.
  # For example, which(sar_cpu_raw[] == "") contains blank lines. We 
  # inline this into the function:

  sar_cpu_raw <- sar_cpu_raw[-(which(sar_cpu_raw[] == ""))]

  # At this point our raw data is now ready to be processed. We can use
  # the code from the previous tutorial to convert this dataset into
  # a native R table.

  sar_cpu <- read.table(text=sar_cpu_raw, header=TRUE )

  # Since we no longer need the cpu_raw table, we dispose of it.

  rm("sar_cpu_raw")

  # Rename the tables to make it look prettier when graphed.
  # There are other ways of labeling but this works in a pinch.
  # The main issue with using the names() function is that the input
  # data order may change. Not a big worry here, but keep this in
  # mind if you are reading in other types of data.
  # To avoid this, you can explicitly rename the columns using the 
  # data.table package.
  #   library(data.table)
  #   setnames(mydata, "newname", "oldname")

# Clean up header names ====
# The original column titles were %usr, %sys, etc.. On import, R renames
# these to X.usr, X.sys, etc.. Also, replace the first column name with
# "Time"

cpu_titles <- names(sar_cpu)
cpu_titles <- gsub("X.", "", cpu_titles)
cpu_titles[1] <- "Time"
names(sar_cpu) <- cpu_titles

# Create posixct column ====
# Here, add a new column pTime that's a combination of the Time and MM
# columns. This is so we can use this with the lubridate package.

# If sar is using 12-hr format + AM/PM, use this:
# sar_cpu <- within(sar_cpu, pTime <- paste(Time, AM))

# If sar is using 24-hr format, use this:
sar_cpu <- within(sar_cpu, pTime <- paste(Time))

# Though the character vector for holding the date/time is perfectly
# readable for humans, to use it properly in R we need to convert
# it to a Posix date format. Here, we use dplyr::mutate to add
# another column to our data frame that contains the Posix date.

# Note that we use the %I to indicate a 12-hour format. If %H were 
# used, it would assume 24hr format and drop the %p specifier.

# sar_cpu <- mutate(sar_cpu, pDate = parse_date_time(pTime, "%I%M%S %p", tz="America/New_York"))
sar_cpu <- mutate(sar_cpu, pDate = parse_date_time(pTime, "%H%M%S", tz="America/New_York"))

# By default, the posixct time stamps gets the current date and time.
# So we clean this up by using the date info we saved earlier:

year(sar_cpu[,"pDate"])  <- year(report_date)
month(sar_cpu[,"pDate"]) <- month(report_date)
day(sar_cpu[,"pDate"])   <- day(report_date)

# Build the plots ====
# There are multiple CPUs in the dataset, 0-x, all. We pull apart the 
# CPU entries and graph them separately.


cpu_list <- levels(sar_cpu$CPU)
cpu_plot <- list()

for (i in 1:length(cpu_list)){
  cpu <- cpu_list[i]
  print(cpu)
  cpu_subset <- subset(sar_cpu, sar_cpu$CPU==cpu)
  report_title <- paste("CPU", cpu)
  # myplot <- xyplot(sys ~ pDate, cpu_subset, type=c("l"), main=report_title)

  myplot <- ggplot(cpu_subset, aes(pDate, sys)) +
    geom_line() + 
    xlab("Time") + 
    ylab("System") +
    ggtitle(report_title)
  cpu_plot[[i]] <- myplot 
#  cpu_plot <- list(cpu_plot, list(myplot))
  
  print(paste("length cpu_plot=", length(cpu_plot)))
}
#cpu_plot <- list()
#for (cpu in cpu_list) {
#  print(cpu)
#  cpu_subset <- subset(sar_cpu, sar_cpu$CPU==cpu)
##  print(head(cpu_subset))
#  report_title <- paste("CPU", cpu)
#  myplot <- xyplot(sys ~ pDate, cpu_subset, type=c("l"), main=report_title)
#  cpu_plot <- list(cpu_plot, list(myplot))
#}

for (cpu in 1:length(cpu_plot)){
  print(cpu_plot[cpu])
}
# Plot the Results ====

# http://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange
library(gridExtra)
n <- length(cpu_plot)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(cpu_plot, ncol=nCol))



# Finally, graph it using the lattice graphics package...
# Here we plot the User vs pDate with data found in sar_cpu.

xyplot(usr ~ pDate,sar_cpu, type=c("l"))

# Add a title using the main= parameter, substituting the header from 
# above in sar_title


#sar_plot <- xyplot(usr ~ pDate,sar_cpu, type=c("l"), main=sar_title[1])

#print(sar_plot)

#plot1 <- xyplot(sys ~ pDate,sar_cpu, type=c("l"), main="System CPU")
#plot2 <- xyplot(usr ~ pDate,sar_cpu, type=c("l"), main="User CPU")
#plot3 <- xyplot(iowait ~ pDate,sar_cpu, type=c("l"), main="IOWait")

# grid.arrange(plot1, plot2, plot3)