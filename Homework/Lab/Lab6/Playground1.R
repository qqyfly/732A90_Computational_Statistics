# set random seed
set.seed(12345)

crossover1 <- function(config1, config2, p = 4){
  ncol <- length(config1)
  child_config <- config1
  child_config[p + 1:ncol] <- config2[1:p]
  return(child_config)
}

mutate <- function(config) {

}

# encoding (X,Y) represent the position of the queen
fitness1 <- function(config) {
  # we will make a list of 
}


# encoding (X) represent a number whose binary string shows 
# the position of the queen in the current row
fitness2 <- function(config) {
  return(0)
}

# encoding using a vector of length N, where N is the number of rows or columns
# n-th element of the vector represents the column of the queen, since we assume
# that only queen can be placed on a row. 

fitness3 <- function(config) {
  return(fitness(config))
}

# our fitness function to handle all the encodings, other encodings will be 
# converted to this encoding.

fitness <- function(p) {
  ## more than one Q on a column?
  result <- sum(duplicated(p)) +
    ## more than one Q on a diagonal?
    sum(duplicated(p - seq_along(p))) +
    ## more than one Q on a reverse diagonal?
    sum(duplicated(p + seq_along(p)))
  return(result)
}

# (x,y) encoding
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
# TODO: need to fix
# if board_size = 8, then the max n is 2^8 - 1 = 255
init_configuration_2 <- function(board_size = 8) {
  max_value <- 2^board_size - 1
  queen_count <- 0
  index <- 1
  configuration <- rep(0, board_size)
  max_try <- 1000
  while(queen_count < board_size && max_try > 0) {
    number <- sample(0:max_value, 1)
    new_queen <- sum(as.numeric(intToBits(number)))
    if (queen_count + new_queen <= board_size && new_queen > 0) {
      configuration[index] <- number
      index <- index + 1
      queen_count <- queen_count + new_queen
    }
    max_try <- max_try - 1
  }

  if(max_try == 0) {
    print("Error: cannot generate a configuration")
  }
  print(queen_count)
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
      for (j in 1:n) {
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
p_val <- 6

configuration <- init_configuration_1(board_size)
configuration
print_board(configuration, 1)

configuration <- init_configuration_2(board_size)
configuration
print_board(configuration, 2)


configuration3_1 <- init_configuration_3(board_size)
configuration3_1

configuration3_2 <- init_configuration_3(board_size)
configuration3_2

min_val <- fitness3(configuration)
while(min_val != 0) {
  # cross over
  crossover_config <- crossover_configuration(configuration3_1, configuration3_2， p = p_val)
 
  val <- fitness3(configuration)
  if (val < min_val) {
    min_val <- val
    print(min_val)
  }
}
print_board(configuration, 3)
