library(lubridate)
library(lattice)
library(dplyr)
library(gridExtra)
# The datafile is created by running sar > sar.out.
# There are many options to sar but beyond the scope of this
# example report.

# Read the input into a raw file first
# This will facilitate dealing with the non-data lines
#    3 lines of header 
#    1 line of footer (Averages)
sar_raw <- readLines("/home/NA/108752/src/R/r_for_sysadmins/sar3.out")

# This section is here to illustrate one way of separating the
# character vector into a list to extract elements.  It also
# demonstrates the %>% operator which chains inputs, much like
# the pipe (|) char does on the command line.
# Once this code runs, I can reference the pieces of the title
# with sar_title[2] to get the second string in the list.

sar_title <- sar_raw[1] %>% strsplit("\t") %>% unlist()

# From the sar_title header, we can extract some useful bits
# such as the date of the report. This is important because the 
# date is only kept in the header.  To extract the date of the 
# report we use the lubridate::mdy function which extracts the
# time from a character string:

report_date <- mdy(sar_title[2])

# We can then pull the year, month, date, etc. with:
# year(report_date)
# [1] 2015

sar_dat <- read.table(text=sar_raw, skip=2, nrows=(length(sar_raw) - 4), header=TRUE )

# Delete the temporary sar_raw table
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


names(sar_dat) <- c("Time", "MM", "CPU", "User", "Nice", "System", "IOWait", "Steal", "Idle")

# Here, add a new column pTime that's a combination of the Time and MM
# columns. This is so we can use this with the lubridate package.

sar_dat <- within(sar_dat, pTime <- paste(Time, MM))

# Though the character vector for holding the date/time is perfectly
# readable for humans, to use it properly in R we need to convert
# it to a Posix date format. Here, we use dplyr::mutate to add
# another column to our data frame that contains the Posix date.

# Note that we use the %I to indicate a 12-hour format. If %H were 
# used, it would assume 24hr format and drop the %p specifier.

sar_dat <- mutate(sar_dat, pDate = parse_date_time(pTime, "%I%M%S %p", tz="America/New_York"))

# By default, the posixct time stamps gets the current date and time.
# So we clean this up by using the date info we saved earlier:

year(sar_dat[,"pDate"])  <- year(report_date)
month(sar_dat[,"pDate"]) <- month(report_date)
day(sar_dat[,"pDate"])   <- day(report_date)

# Now, say we're only interested in the CPU information. We can create 
# a new dataframe containing just that information:

sar_cpu <- select(sar_dat, pDate, User, System)

# Finally, graph it using the lattice graphics package...
# Here we plot the User vs pDate with data found in sar_cpu.

xyplot(User ~ pDate,sar_cpu, type=c("l"))

# Add a title using the main= parameter, substituting the header from 
# above in sar_title

sar_plot <- xyplot(User ~ pDate,sar_cpu, type=c("l"), main=sar_title[1])

print(sar_plot)

plot1 <- xyplot(System ~ pDate,sar_cpu, type=c("l"), main="System CPU")
plot2 <- xyplot(User ~ pDate,sar_cpu, type=c("l"), main="User CPU")
grid.arrange(plot1, plot2)