library(dplyr)
esxHosts <- read.csv("rvdata/RVTools_tabvHost.csv", header=TRUE)
esxHosts.df <- tbl_df(esxHosts)

LinuxHosts <- filter(esxHosts.df, Cluster=="LINUX-CLUSTER") %>% 
    select(Host, Datacenter, Speed, vRAM, X..Memory, VM.Used.memory, VM.Memory.Ballooned, X..VMs)


print(LinuxHosts)

print (sum(LinuxHosts$VM.Used.memory))

hostMemory <- sum(LinuxHosts$X..Memory)
vmMemory <- sum(LinuxHosts$VM.Used.memory)

print(paste0("Host Memory: ", hostMemory), justify="right")
print(paste0("VM Memory  : ", vmMemory  ), justify="right")
