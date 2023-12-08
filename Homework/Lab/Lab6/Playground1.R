

crossover <- function(config1, config2, p = 4){
  ncol <- length(config1)
  new_config <- config1
  new_config[p + 1:ncol] <- config2[1:p]
  return(new_config)
}

mutate <- function(config) {

}

fitness <- function(configuration){

}

attacks <- function(p) {
  ## more than one Q on a column?
  result <- sum(duplicated(p)) +
    ## more than one Q on a diagonal?
    sum(duplicated(p - seq_along(p))) +
    ## more than one Q on a reverse diagonal?
    sum(duplicated(p + seq_along(p)))
  return(result)
}

init_configuration <- function(board_size = 8, queen_size = 8){

}

board_size <- 8
queen_size <- 8
p <- 4


# define the board
board <- matrix(0, nrow = board_size, ncol = board_size)

board
