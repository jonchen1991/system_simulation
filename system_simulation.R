# Simulation of DNA strands, how cases are bounced around different nodes and where all the bottlenecks are. Still really rough

# Set parameters
set.seed(1337)
pop_size = 1e3
time_step = 10

# Helper functions
next_dna <- function(dna_end){
  return(switch(dna_end, 
                # a: NIA, b: R Delinquency, c: TDI, d: Correspondence, e: R Secured, f: collected
                'a'= 'b',
                'b'= sample(c('c','d','e'), 1,prob = c(1,1,2)),
                'c'= sample(c('d','e'), 1, prob = c(1,2)),
                'd'= sample(c('b','c','e'), 1, prob = c(1,1,2)),
                'e'= sample(c('c','d','f'), 1, prob = c(1,1,2))))
}


# Step 1: Build DNA strands
print("Build DNA Strands")
start_time<- Sys.time()
dna_list <- list()
for (i in 1: pop_size){
  dna_list[[i]] <- 'a'
  while (dna_list[[i]][length(dna_list[[i]])] != 'f'){
    dna_list[[i]] <- c(dna_list[[i]], next_dna(dna_list[[i]][length(dna_list[[i]])]))
  }
#  dna_list[[i]] <- paste(unlist(dna_list[[i]]),collapse = '')
}
end_time <- Sys.time()
print(end_time - start_time)

# Step 2: From DNA strands, determine flow rates between nodes
print("Determine flow counts and rates to and from nodes.")
start_time <- Sys.time()
nodes = c('a','b','c','d','e','f')

# create flowrate matrix: source is row index, destination is column index
flowcounts <- as.data.frame(matrix(0,nrow=6, ncol=6, dimnames = list(nodes, nodes)))

# count how cases flow from one node to another from DNA strands
for (i in 1: pop_size){
  for (j in 1: length(dna_list[[i]])-1){
    flowcounts[dna_list[[i]][j],dna_list[[i]][j+1]] <- flowcounts[dna_list[[i]][j],dna_list[[i]][j+1]] + 1
  }
}

#drop a as a destination, f as a source
#flowcounts <- flowcounts[c('a','b','c','d','e'),c('b','c','d','e','f')]

# translate raw counts into flow rates. Each entry is divided by the total cases in the row.
flowrates <- flowcounts / pop_size

end_time <- Sys.time()
print(end_time-start_time)

# Step 3: Simulate how cases flow between nodes and determine where bottlenecks are. Macro level.
print("Simulate case movement to see where bottlenecks are.")
start_time <- Sys.time()

# list of limits for nodes
working_capacity <- list('a' = 10, 'b' = 5, 'c' = 5, 'd' = 5, 'e' = 5)
backlog_capacity <- list('a' = pop_size, 'b' = 100, 'c' = 100, 'd' = 100, 'e' = 100, 'f' = pop_size)

# creation of the table keeping track of the simulation
sim_table <- as.data.frame(matrix(0, nrow = (pop_size/time_step+1), ncol=6, dimnames = list(1:(pop_size/time_step+1), nodes)))
sim_table <- as.data.frame(matrix(0, nrow = 2, ncol=6, dimnames = list(c(1,2), nodes)))
sim_table[1, 'a'] <- pop_size
sources <- rownames(flowcounts)
destinations <- colnames(flowcounts)
i <- 1
# for (i in 1:19){
while (sim_table[nrow(sim_table),'f'] < (pop_size-1)){
  sim_table[i+1,] <- sim_table[i,]
  for (source in sources){
    if (source == 'f'){
      next
    }
    cases_count <- min(working_capacity[[source]], sim_table[i,source])
    case_split <- flowcounts[source,]/sum(flowcounts[source,])
#    print(source)
#    print(case_split)
    for (destination in destinations){
      if (backlog_capacity[[destination]] < sim_table[i,destination]){
        case_split[destination] <- 0
      }
    }
#    print(case_split)
    if (sum(case_split) == 0){
      next
    }
    case_split <- case_split/sum(case_split)
    sim_table[i+1, source] <- sim_table[i+1, source] - cases_count
    sim_table[i+1,] <- case_split * cases_count + sim_table[i+1,]
  }
#  print(i)
#  print(sum(sim_table[i,]))
  i <- i + 1
}
end_time <- Sys.time()
print(end_time-start_time)

sim_table %>%
  mutate(time = seq(1, nrow(sim_table), by=1)) %>%
  gather(key="program", value="count", "a":"f") %>%
  ggplot(aes(x=time, y=count, color=program)) +
  geom_line()
