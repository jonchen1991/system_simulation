# Simulation of DNA strands, how cases are bounced around different nodes and where all the bottlenecks are. Still really rough

# Set parameters
set.seed(1337)
pop_size = 1e3

# Helper functions
next_dna <- function(dna_end){
  return(switch(dna_end, 
                # a: NIA, b: R Delinquency, c: TDI, d: Correspondence, e: R Secured
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
test<-as.data.frame(matrix(0,nrow=6, ncol=6, dimnames = list(c('a','b','c','d','e','f'),c('a','b','c','d','e', 'f'))))
# source is row index, destination is column index
for (i in 1: pop_size){
  for (j in 1: length(dna_list[[i]]-1)){
    
    
  }
}
# Step 3: Simulate how cases flow between nodes and determine where bottlenecks are.

