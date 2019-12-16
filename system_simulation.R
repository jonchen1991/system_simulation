# Simulation of DNA strands, how cases are bounced around different nodes and where all the bottlenecks are. Still really rough

# Set parameters
set.seed(1337)
pop_size = 1e3

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

nodes = c('a','b','c','d','e','f')

# create flowrate matrix: source is row index, destination is column index
flowcounts <- as.data.frame(matrix(0,nrow=6, ncol=6, dimnames = list(nodes, nodes)))

# count how cases flow from one node to another from DNA strands
for (i in 1: pop_size){
  for (j in 1: length(dna_list[[i]])-1){
    flowcounts[dna_list[[i]][j],dna_list[[i]][j+1]] <- flowcounts[dna_list[[i]][j],dna_list[[i]][j+1]] + 1
  }
}

# translate raw counts into flow rates. Each entry is divided by the total cases in the row.
flowrates <- flowcounts
for (i in nodes){
  flowrates[i,] <- flowrates[i,]/sum(flowrates[i,])
}

#drop a as a destination, f as a source
#flowrates['f',] <- NULL
#flowrates[,'a'] <- NULL
# Step 3: Simulate how cases flow between nodes and determine where bottlenecks are.

