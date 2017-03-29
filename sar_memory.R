  library(lubridate)
  library(lattice)
  library(dplyr)
  library(gridExtra)
  library(ggplot2)
  library(gridExtra)
  
  # The datafile is created by running sar -A > sar_full.out.
  # There are many options to sar but beyond the scope of this
  # example report.
  # Read the input into a raw file first
  # This will facilitate dealing with the non-data lines
  #    3 lines of header 
  #    1 line of footer (Averages)
  sar_raw <- readLines("tmp/cerberus.sar")
  
  sar_title <- sar_raw[1] %>% strsplit("\t") %>% unlist()
  
  report_date <- mdy(sar_title[2])
  # Find all the "Averages" strings in the sar_raw. This
  # will serve as delimiters to each stanza
  sar_delimiter <- grep("Average", sar_raw)
  
  # Memory Section ====
  header_pattern_memory <- "kbmemfree.*kbmemused.*memused.*kbbuffers.*kbcached.*kbcommit.*commit.*kbactive.*kbinact.*kbdirty"
  sar_memory_headers <- grep(header_pattern_memory, sar_raw)
  memory_firstline <- sar_memory_headers[1]
  tmp1 <- sar_delimiter[] > sar_memory_headers[1]
  memory_lastline <- sar_delimiter[tmp1][1]
  numLines <- memory_lastline - memory_firstline
  sar_memory_raw <- sar_raw[memory_firstline:memory_lastline]
  sar_memory_headers_2 <- grep(header_pattern_memory, sar_memory_raw)

  sar_memory_raw <- sar_memory_raw[-(grep("Average", sar_memory_raw))]

  # Next we clean up all those blank lines in the dataset.
  # The which() function will return a vector of lines matching a query.
  # For example, which(sar_cpu_raw[] == "") contains blank lines. We 
  # inline this into the function:
  ## FIX THIS - Returning character(0)
  # sar_memory_raw <- sar_memory_raw[-(which(sar_memory_raw[] == ""))]
  

  sar_memory <- read.table(text=sar_memory_raw, header=TRUE )

  rm("sar_memory_raw")
  
  memory_titles <- names(sar_memory)
  memory_titles <- gsub("X.", "", memory_titles)
  memory_titles[1] <- "Time"
  names(sar_memory) <- memory_titles
  
  # Create posixct column ====
  # Here, add a new column pTime that's a combination of the Time and MM
  # columns. This is so we can use this with the lubridate package.
  
  # If sar is using 12-hr format + AM/PM, use this:
  # sar_cpu <- within(sar_cpu, pTime <- paste(Time, AM))
  
  # If sar is using 24-hr format, use this:
  sar_memory <- within(sar_memory, pTime <- paste(Time))
  
  # Though the character vector for holding the date/time is perfectly
  # readable for humans, to use it properly in R we need to convert
  # it to a Posix date format. Here, we use dplyr::mutate to add
  # another column to our data frame that contains the Posix date.
  
  # Note that we use the %I to indicate a 12-hour format. If %H were 
  # used, it would assume 24hr format and drop the %p specifier.
  
  sar_memory <- mutate(sar_memory, pDate = parse_date_time(pTime, "%H%M%S", tz="America/New_York"))
  
  # By default, the posixct time stamps gets the current date and time.
  # So we clean this up by using the date info we saved earlier:
  
  year(sar_memory[,"pDate"])  <- year(report_date)
  month(sar_memory[,"pDate"]) <- month(report_date)
  day(sar_memory[,"pDate"])   <- day(report_date)
  
  
  kbMemfree_plot <- ggplot(sar_memory, aes(pDate, kbmemfree)) + geom_line() + xlab("Time") + ylab("KBMemFree") + ggtitle("KBMemFree")
  kbMemUsed_plot <- ggplot(sar_memory, aes(pDate, kbmemused)) + geom_line() + xlab("Time") + ylab("KBMemUsed") + ggtitle("KBMemUsed")
  memUsedPct_plot <- ggplot(sar_memory, aes(pDate, memused)) + geom_line() + xlab("Time") + ylab("MemUsedPct") + ggtitle("MemUsedPct")
  kbCached_plot <- ggplot(sar_memory, aes(pDate, kbcached)) + geom_line() + xlab("Time") + ylab("KBCached") + ggtitle("KBCached")
  
  memory_plot <- list()
  memory_plot[[1]] <- kbMemfree_plot
  memory_plot[[2]] <- kbMemUsed_plot
  memory_plot[[3]] <- memUsedPct_plot
  memory_plot[[4]] <- kbCached_plot
  

  for (i in 1:length(memory_plot)){
    print(memory_plot[i])
  }
  
  # http://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange
  n <- length(memory_plot)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(memory_plot, ncol=nCol))
  
  
  xyplot(usr ~ pDate,sar_cpu, type=c("l"))
  
  