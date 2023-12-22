# set random seed
set.seed(12345)
library(bitops)
library(ggplot2)
#DONE
crossover <- function(config1, config2, p = 4){
  ncol <- length(config1)
  child_config <- config1
  for(i in 1:(ncol-p)){
    child_config[p+i] = config2[i]
  }
  return(child_config)
}

# (x,y) encoding, change y only
mutate1 <- function(config) {
  ncol <- length(config)
  mutated_config <- config
  queen_to_mutate <- sample(1:ncol, 1)
  new_position <- mutated_config[queen_to_mutate]
  new_y <- sample(1:ncol, 1)
  new_position[2] <- new_y
  mutated_config[queen_to_mutate] <- new_position
  return(mutated_config)
}

# (binary) encoding, change one bit randomly
mutate2 <- function(config) {
  ncol <- length(config)
  mutated_config <- config
  queen_to_mutate <- sample(1:ncol, 1)
  queen_integer <- mutated_config[queen_to_mutate]

  num_bits <- log2(queen_integer) %/% 1 + 1
  
  # Randomly select a bit position to flip
  bit_position <- sample(0:(num_bits - 1), 1)
  
  # Use bitwise XOR to flip the selected bit
  flipped_integer <- bitwXor(queen_integer, 2^bit_position)
  
  return(flipped_integer)
}


# (col location) encoding, change col number only
mutate3 <- function(config) {
  ncol <- length(config)
  mutated_config <- config
  queen_to_mutate <- sample(1:ncol, 1)
  mutated_config[queen_to_mutate] <- sample(1:ncol, 1)
  return(mutated_config)
}

# encoding (X,Y) represent the position of the queen
fitness1 <- function(config) {
   return(fitness(config))
}


# encoding (X) represent a number whose binary string shows 
# the position of the queen in the current row
find_set_bits_indices <- function(n,board_size) {
  bit_vector <- rev(intToBits(n)[1:board_size])
  
  for(i in 1:length(bit_vector)){
    if (bit_vector[i] == 1){
      bit_indice <- i
      break
    }
  }
  return(bit_indice)
}

fitness2 <- function(config) {
  new_config <- data.frame(x = c(), y = c())
  row_number <- length(config)
  for(i in 1:row_number){
    number <- config[i]
    queen_pos <- find_set_bits_indices(number,row_number)
    new_config <- rbind(new_config, c(i,config[i]))
  }
  return(fitness(new_config))
}

# encoding using a vector of length N, where N is the number of rows or columns
# n-th element of the vector represents the column of the queen, since we assume
# that only queen can be placed on a row. 

# DONE
fitness3 <- function(config) {
  new_config <- data.frame(x = c(), y = c())
  queen_num <- length(config)
  for(i in 1:queen_num){
    new_config <- rbind(new_config, c(i,config[i]))
  }
  return(fitness(new_config))
}

# our fitness function to handle all the encodings, other encodings will be 
# converted to this encoding.(X,Y) Location Encoding 

# check attack between queen position
is_attack <- function(queen1, queen2) {
  return(
    queen1[1] == queen2[1] || queen1[2] == queen2[2] || 
    abs(queen1[1] - queen2[1]) == abs(queen1[2] - queen2[2])
  )
}

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

# (x,y) pair encoding
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
  return(configuration)
}

# binary encoding
# if board_size = 8, then the max n is 2^8 - 1 = 255
init_configuration_2 <- function(board_size = 8,max_try_count = 1000) {
  max_value <- 2^board_size - 1
  queen_count <- 0
  index <- 1
  configuration <- rep(0, board_size)
  max_try_count <- 1000
  while(queen_count < board_size && max_try > 0) {
    number <- sample(0:max_value, 1)
    
    new_queen <- sum(as.numeric(intToBits(number)))
    if (queen_count + new_queen <= board_size && new_queen == 1) {
      configuration[index] <- number
      index <- index + 1
      queen_count <- queen_count + new_queen
    }
    max_try_count <- max_try_count - 1
  }

  if(max_try_count == 0) {
    print("Error: cannot generate a configuration")
  }

  return(configuration)

}

# binary encoding
# if board_size = 8, then the max n is 2^8 - 1 = 255
init_configuration_2 <- function(board_size = 8,max_try_count = 1000) {
  max_value <- 2^board_size - 1
  queen_count <- 0
  index <- 1
  configuration <- rep(0, board_size)
  max_try_count <- 1000
  while(queen_count < board_size && max_try_count > 0) {
    number <- sample(0:max_value, 1)
    
    new_queen <- sum(as.numeric(intToBits(number)))
    if (queen_count + new_queen <= board_size && new_queen == 1) {
      configuration[index] <- number
      index <- index + 1
      queen_count <- queen_count + new_queen
    }
    max_try_count <- max_try_count - 1
  }

  if(max_try_count == 0) {
    print("Error: cannot generate a configuration")
  }

  return(configuration)

}

# vector position encoding
init_configuration_3 <- function(board_size = 8) {
  configuration <- sample(1:board_size, board_size)
  return(configuration)
}

# the parameter configuration is a matrix of size n x n
# if value of configuration[i,j] is 1, then there is a queen on this cell
# otherwise, there is no queen on this cell

print_board <- function(configuration, method = 1) {
  if(method == 1) {
    n <- dim(configuration)[1]
    config <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
      config[configuration[i, 1], configuration[i, 2]] <- 1
    }
  } else if (method == 2) {
    # convert the number vector to a matrix(binary encoding)
    n <- length(configuration)
    config <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
      number <- configuration[i]
      for (j in n:1) {
        config[i, j] <- number %% 2
        number <- number %/% 2
      }
    }
  } else {
    # method 3
    # convert a vector to a matrix(column encoding)
    n <- length(configuration)
    config <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
      config[i, configuration[i]] <- 1
    }
  } 

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

# this p value shuld less equal than board_size/2
p_val <- 4

configuration <- init_configuration_1(board_size)
configuration
print_board(configuration, 1)
fitness1(configuration)
mutate1(configuration)

configuration <- init_configuration_2(board_size)
configuration
print_board(configuration, 2)
fitness2(configuration)
mutate2(configuration)

configuration <- init_configuration_3(board_size)
configuration
print_board(configuration, 3)
fitness3(configuration)
mutate3(configuration)

# Method1

# Method2

# Method3

configuration3_1 <- init_configuration_3(board_size)
val_3_1 <- fitness3(configuration3_1)
configuration3_2 <- NULL

max_steps <- 100
steps <- 0
num_attacked_queens_vector <- c(val_3_1$num_attacked_queens)

while(val_3_1$num_attacked_queens != 0 && steps <= max_steps ) {

  if (is.null(configuration3_2)){
    configuration3_2 <- init_configuration_3(board_size)    
    val_3_2 <- fitness3(configuration3_2)
  }
  
  # cross over
  child_config <- crossover(configuration3_1, 
                            configuration3_2,  
                            p = p_val)
 
  # mutate
  mutated_config <- mutate3(child_config)
  
  val_child <- fitness3(mutated_config)
  configs <- c(1,2,3)
  num_attacked_queens <- c(val_3_1$num_attacked_queens,
                    val_3_2$num_attacked_queens,
                    val_child$num_attacked_queens)
  df <- data.frame(config = configs, 
                   num_attacked_queens = num_attacked_queens)

  custom_order <- order(df$num_attacked_queens)
  sorted_df <- df[custom_order, ]
  
  # choose minial 2 values
  if (sorted_df$config[1] == 1){
    configuration3_1 <- configuration3_1
    val_3_1 <- val_3_1
  }else if(sorted_df$config[1] == 2){
    configuration3_1 <- configuration3_2
    val_3_1 <- val_3_2
  }else {
    configuration3_1 <- mutated_config
    val_3_1 <- val_child
  }

  if (sorted_df$config[2] == 1){
    configuration3_2 <- configuration3_1
    val_3_2 <- val_3_1
  }else if(sorted_df$config[2] == 2){
    configuration3_2 <- configuration3_2
    val_3_2 <- val_3_2
  }else {
    configuration3_2 <- mutated_config
    val_3_2 <- val_child
  }

  num_attacked_queens_vector <- c(num_attacked_queens_vector,val_3_1$num_attacked_queens)
  steps <- steps + 1
  print(val_3_1$num_attacked_queens)
}
print_board(configuration3_1, 3)

df3 <- data.frame(steps = 1:length(num_attacked_queens_vector),
                  num_attacked_queens = num_attacked_queens_vector)
ggplot(data=df3, aes(x = steps, y = num_attacked_queens)) + geom_line()

