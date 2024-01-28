
rm(list = ls())
library(ggplot2)
library(bitops)
set.seed(12345)

init_configuration_1 <- function(board_size = 8) {
  configuration <- data.frame(x = c(), y = c())
  queen_count <- 0
  for (i in 1:board_size) {
    for(j in 1:board_size) {
      isQueen <- sample(c(0, 1), 1)
      if (queen_count < board_size) {
        if (isQueen == 1){
          configuration <- rbind(configuration, c(i, j))
          queen_count <- queen_count + 1
        }
      }
    }
  }
  return(t(configuration))
}

# Second Encoding: binary encoding
# if board_size = 8, then the max n is 2^8 - 1 = 255
init_configuration_2 <- function(board_size = 8,max_try_count = 1000) {
  max_value <- 2^board_size - 1
  queen_count <- 0
  configuration <- rep(0, board_size)
  while(queen_count < board_size && max_try_count > 0) {
    number <- sample(0:max_value, 1)
    
    new_queen <- sum(as.numeric(intToBits(number)))
    if (queen_count + new_queen <= board_size && new_queen == 1) {      
      queen_count <- queen_count + new_queen
      configuration[queen_count] <- number
    }
    max_try_count <- max_try_count - 1
  }

  if(max_try_count == 0) {
    print("Error: cannot generate a configuration")
  }

  return(configuration)

}

# Third Encoding: vector position encoding
init_configuration_3 <- function(board_size = 8) {
  configuration <- sample(1:board_size, board_size)
  return(configuration)
}

# convert encodings to matrix
decode_config <- function(config, method = 1) {
  if (method == 1){#(x,y) encoding
    n <- dim(config)[2]
    mat <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
      mat[config[1,i], config[2,i]] <- 1
    }
  }else if (method == 2) {#binary encoding
      n <- length(config)
      mat <- matrix(0, nrow = n, ncol = n)
      for (j in 1:n) {
        number <- config[j]
        for (i in n:1) {
          mat[i, j] <- number %% 2
          number <- number %/% 2
        }
      }
  }else if (method == 3){#vector position encoding
    n <- length(config)
    mat <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
      mat[config[i],i] <- 1
    }
  }

  return(mat)
}

# convert  matrix to encodings
encode_config <- function(mat,method = 1) {
  n <- nrow(mat)[1]
  if (method == 1){#(x,y) encoding
    config <- data.frame(x = c(), y = c())
    for(i in 1:n){
      for(j in 1:n){
        if (mat[i,j] == 1){          
          config$x <- c(config$x,i)
          config$y <- c(config$y,j)          
        }
      }
    }
  }else if (method == 2) {#binary encoding 
    configuration <- rep(0, n)    
    # n rows
    for (i in 1:n) {
      for(j in 1:n) {
        config[i] <- config[i] + mat[i,j] * 2^(n - j)
      }      
    }
  }else if (method == 3){#vector position encoding
    configuration <- rep(0, n)    
    for(j in 1:n){
      for(i in 1:n){
        if (mat[i,j] == 1){          
          config[j] <- i
        }
      }
    }
  }

  return(config)
}

# print board common function
print_board <- function(configuration, method = 1) {
  config <- decode_config(configuration, method)

  n <- dim(config)[1]
  for(i in 1:n) {
    for(j in 1:n) {
      if(config[i, j] == 1) {
        if (j == 1) {
          cat("|Q|")
        }else if (j == n){
          cat("Q|", "\n")
        }else{
          cat("Q|")
        }
      } else {
        if (j == 1) {
          cat("|-|")
        }else if (j == n){
          cat("-|", "\n")
        }else{
          cat("-|")
        }
      }
    }
  }
}

board_size <- 8

print("Test Encoding 1")
configuration <- init_configuration_1(board_size)
configuration
print_board(configuration, 1)

print("Test Encoding 2")
configuration <- init_configuration_2(board_size)
configuration
print_board(configuration, 2)

print("Test Encoding 3")
configuration <- init_configuration_3(board_size)
configuration
print_board(configuration, 3)

########################## Crossover Function ############################
crossover <- function(config1, config2,method = 1, p = 4){
  if (method == 1){
    boardsize <- ncol(config1)
  }else{
    boardsize <- length(config1)
  }

  child_config <- config1
  if (method == 1){
    child_config[,(p+1):boardsize] = config2[,(p+1):boardsize]
  }else{
    for(i in (p+1):boardsize){     
      child_config[i] <- config2[i]
    }    
  }
  return(child_config)
}

config1 <- init_configuration_1(board_size)
config1
print_board(config1, 1)
config2 <- init_configuration_1(board_size)
config2
print_board(config2, 1)

print("Child encoding 1")
child_config <- crossover(config1, config2, 1, 4)
child_config
print_board(child_config, 1)


mutate <- function(config,method = 1) {
  if (method == 1){
    boardsize <- ncol(config)
  }else{
    boardsize <- length(config)
  }
  
  mutate_config <- config

  if (method == 1){    
    processed <- FALSE    
    mat <- decode_config(mutate_config, 1)
    while(!processed){
      col_to_mutate <- sample(1:boardsize, 1)
      # move the queen to lower position (y-1) if not occupied      
      if (mutate_config[2,col_to_mutate] + 1 <= boardsize &&
          mat[mutate_config[1,col_to_mutate],mutate_config[2,col_to_mutate] + 1] == 0){                    
        mutate_config[2,col_to_mutate] <-  mutate_config[2,col_to_mutate] + 1
        processed = TRUE        
      }else if (mat[mutate_config[1,col_to_mutate],1] == 0){
        mutate_config[2,col_to_mutate] <- 1
        processed <- TRUE            
      }
    }
  }else if (method == 2){    
    # current num * 2 or become 1 using bit operation
    col_to_mutate <- sample(1:boardsize, 1)
    queen_integer <- mutate_config[col_to_mutate]
    if (queen_integer == 1){
      queen_integer <- 2^(boardsize-1)
    }else{
      queen_integer <- bitwShiftR(queen_integer,1)
    }
    mutate_config[col_to_mutate] <- queen_integer
  }else if (method == 3){
    col_to_mutate <- sample(1:boardsize, 1)
    
    if (mutate_config[col_to_mutate] + 1 > boardsize){
      mutate_config[col_to_mutate] <- 1
    }else{
      mutate_config[col_to_mutate] <- mutate_config[col_to_mutate] + 1
    }
  }
  
  return(mutate_config)
}


print("Test Mutate encoding 3")
config1 <- init_configuration_3(board_size)
config1
mutate_config <- mutate(config1, 3)
mutate_config

########################## Common Function ############################
# common functions for fitness function
# check attack between queen position
is_attack <- function(queen1, queen2) {
  return(
    queen1[1] == queen2[1] || queen1[2] == queen2[2] || 
    abs(queen1[1] - queen2[1]) == abs(queen1[2] - queen2[2])
  )
}

########################## Fitness Function ############################
# fitness function to handle all the encodings, other encodings will be 
# converted to this encoding.(X,Y) Location Encoding 

# check whole config is valid or not and return 
# attacked queens number and unattacked queens number
fitness <- function(config) {
  queen_num <- nrow(config)
  attacked_queens <- c()

  for (i in 1:(queen_num - 1)) {
    for (j in (i + 1):queen_num) {
      if (is_attack(config[i,], config[j,])) {
        attacked_queens <- c(attacked_queens, i, j)
      }
    }
  }
  
  unattacked_queens <- setdiff(1:queen_num, unique(attacked_queens))
  num_unattacked_queens <- length(unattacked_queens)
  valid <- (length(unique(attacked_queens)) == 0)

  return(list(valid = valid,
              num_unattacked_queens = num_unattacked_queens, 
              num_attacked_queens = queen_num - num_unattacked_queens))
}

fitness1 <- function(config) {
  return(fitness(config))
}

fitness2 <- function(config) {
  # convert binary encoding to (x,y) encoding
  mat <- decode_config(config, 2)
  config <- encode_config(mat, 1)
  return(fitness(config))
}

fitness3 <- function(config) {
  # convert vector position encoding to (x,y) encoding
  mat <- decode_config(config, 3)
  config <- encode_config(mat, 1)
  return(fitness(config))
}

genetic_algorithm <- function(method = 1,board_size=8){
  if (method == 1){
    configuration_1 <- init_configuration_1(board_size)
    val_1 <- fitness1(configuration_1)
  } else if (method == 2){
    configuration_1 <- init_configuration_2(board_size)
    val_1 <- fitness2(configuration_1)
  }else{
    configuration_1 <- init_configuration_3(board_size)
    val_1 <- fitness3(configuration_1)
  }

  configuration_2 <- NULL

  max_steps <- 1000
  steps <- 0

  num_attacked_queens_vector <- c(val_1$num_attacked_queens)

  while(val_1$num_attacked_queens != 0 && steps <= max_steps ) {

    if (is.null(configuration_2)){
      if (method == 1){
        configuration_2 <- init_configuration_1(board_size)    
        val_2 <- fitness1(configuration_2)
      }else if(method == 2){
        configuration_2 <- init_configuration_2(board_size)    
        val_2 <- fitness2(configuration_2)
      }else{
        configuration_2 <- init_configuration_3(board_size)    
        val_2 <- fitness3(configuration_2)
      }
    }
    
    # cross over
    child_config <- crossover(configuration_1, 
                              configuration_2,  
                              p = p_val)
  
    # mutate
    if (method == 1){
      mutated_config <- mutate(child_config,method=1)
      val_child <- fitness1(mutated_config)

    } else if (method == 2){
      mutated_config <- mutate(child_config,method=2)
      val_child <- fitness2(mutated_config)
    } else {
      mutated_config <- mutate(child_config,method=3)
      val_child <- fitness3(mutated_config)
    }
        
    configs <- c(1,2,3)
    num_attacked_queens <- c(val_1$num_attacked_queens,
                      val_2$num_attacked_queens,
                      val_child$num_attacked_queens)
    df <- data.frame(config = configs, 
                    num_attacked_queens = num_attacked_queens)

    custom_order <- order(df$num_attacked_queens)
    sorted_df <- df[custom_order, ]
    
    # choose minial 2 values
    if (sorted_df$config[1] == 1){
      configuration_1 <- configuration_1
      val_1 <- val_1
    }else if(sorted_df$config[1] == 2){
      configuration_1 <- configuration_2
      val_1 <- val_2
    }else {
      configuration_1 <- mutated_config
      val_1 <- val_child
    }

    if (sorted_df$config[2] == 1){
      configuration_2 <- configuration_1
      val_2 <- val_1
    }else if(sorted_df$config[2] == 2){
      configuration_2 <- configuration_2
      val_2 <- val_2
    }else {
      configuration_2 <- mutated_config
      val_2 <- val_child
    }

    num_attacked_queens_vector <- c(num_attacked_queens_vector,val_1$num_attacked_queens)
    steps <- steps + 1
    
  }

  # print the queen position if found the solution
  if (val_1$num_attacked_queens == 0){
    print_board(configuration_1, 3)
  }else{
    print("not found the solution")
  }

  #df <- data.frame(steps = 1:length(num_attacked_queens_vector),
  #                  num_attacked_queens = num_attacked_queens_vector)
  #ggplot(data=df, aes(x = steps, y = num_attacked_queens)) + geom_line()

}

p_val <- 3
genetic_algorithm(method=3,board_size=8)


