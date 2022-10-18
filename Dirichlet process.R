Process <- function(n, alpha){
  
  total_prob <- c() 
  configuration <- data.frame()
  
  dir_process <- function (root, n, alpha, prev){ #set four variables, root: the first cluster configuration, n, numbers of mutations, alpha: parameter α, prev: the probability of last iteration of configuration
    if (n == 1){
      total_prob <<- c(total_prob, prev) #declare a global variable to print all probability
      configuration <<- rbind(configuration,root)
      return(0) #leave iteration
    }
    
    table_ = as.data.frame(table(root)) 
    n = n - 1
    
    for (i in 1:(nrow(table_)+1)){
      if (i < nrow(table_)+1){ #Decide the probability of each cluster configuration when...
        curr = prev * table_[i,2] / (alpha + n) #The next mutation is in the same cluster
      }else{
        curr = prev * alpha / (alpha + n) #The next mutation is in different cluster
      }
      new = append(root, i-1) #Elongate the the cluster
      dir_process(new, n, alpha, curr) #Iterate again until n==1
    }
  }
  
  dir_process(c(0), n, alpha, 1) #Set prev as 1 because the first cluster configuration always has probability of 1 
  return (list(max(total_prob), configuration[which.max(total_prob),])) #First output: Max probability
                                                                        #Second output: Configurations
}



Process(4,50) #Please set your preferred n (numbers of mutations) and alpha(parameter α)


#To simplify the visualization of the cluster configuration. mutation A, B, C,... can been seen as the the column name of tamp[[2]]
#from left to right one by one, if the number is increase, it means the next mutation is allocated to a new cluster
#Take n=4 for example,
#[0, 0, 0, 0] means all of them are in the same cluster
#[0, 1, 0, 0] means B is allocated to a new cluster, while the other three mutations are in the same cluster
#[0, 1, 2, 2] means B and c are allocated to new clusters to themselves, while D is in the same cluster as C
#[0, 1, 2, 3] means all of them belong to different cluster
