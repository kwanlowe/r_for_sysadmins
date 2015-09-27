# The difference between these is the number of lines to read
numLines <- cpu_lastline - cpu_firstline

sar_cpu_raw <- sar_raw[cpu_firstline:cpu_lastline]

# Now sar_cpu_raw contains just the CPU entries from the datafile.
# Next, we need to filter out all the junk lines containing CPU
# headers and the Averages line.  

# To refresh your memory: 
# sar_delimiter is a vector containing lines matching "Averages"
# sar_cpu_headers is a vector with lines matching "CPU.*iowait" etc.


#sar_cpu <- read.table(text=sar_raw, 
#                      skip=tmp1-1,     # Read 1 less line than start 
#                      nrows=numLines,  # Read the lines in
#                      header=TRUE )

# rm("sar_raw")

# Rename the tables to make it look prettier when graphed.
# There are other ways of labeling but this works in a pinch.
# The main issue with using the names() function is that the input
# data order may change. Not a big worry here, but keep this in
# mind if you are reading in other types of data.
# To avoid this, you can explicitly rename the columns using the 
# data.table package.
#   library(data.table)
#   setnames(mydata, "newname", "oldname")


#names(sar_dat) <- c("Time", "MM", "CPU", "User", "Nice", "System", "IOWait", "Steal", "Idle")

# Here, add a new column pTime that's a combination of the Time and MM
# columns. This is so we can use this with the lubridate package.

#sar_dat <- within(sar_dat, pTime <- paste(Time, MM))

# Though the character vector for holding the date/time is perfectly
# readable for humans, to use it properly in R we need to convert
# it to a Posix date format. Here, we use dplyr::mutate to add
# another column to our data frame that contains the Posix date.

# Note that we use the %I to indicate a 12-hour format. If %H were 
# used, it would assume 24hr format and drop the %p specifier.

#sar_dat <- mutate(sar_dat, pDate = parse_date_time(pTime, "%I%M%S %p", tz="America/New_York"))

# By default, the posixct time stamps gets the current date and time.
# So we clean this up by using the date info we saved earlier:

##year(sar_dat[,"pDate"])  <- year(report_date)
#month(sar_dat[,"pDate"]) <- month(report_date)
##day(sar_dat[,"pDate"])   <- day(report_date)

# Now, say we're only interested in the CPU information. We can create 
# a new dataframe containing just that information:

#sar_cpu <- select(sar_dat, pDate, User, System)

# Finally, graph it using the lattice graphics package...
# Here we plot the User vs pDate with data found in sar_cpu.

#xyplot(User ~ pDate,sar_cpu, type=c("l"))

# Add a title using the main= parameter, substituting the header from 
# above in sar_title

#sar_plot <- xyplot(User ~ pDate,sar_cpu, type=c("l"), main=sar_title[1])

#print(sar_plot)

#plot1 <- xyplot(System ~ pDate,sar_cpu, type=c("l"), main="System CPU")
#plot2 <- xyplot(User ~ pDate,sar_cpu, type=c("l"), main="User CPU")
#grid.arrange(plot1, plot2)