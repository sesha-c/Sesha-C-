
# get the coordinates of a specific grid number 
get_coordinates <- function(n, columns) { 
  j <- ((n-1) %/% columns) + 1
  i <- ((n-1) %% columns) + 1
  if (j %% 2 == 0) {  # this row goes right to left 
    i <- (columns + 1) - i # flip horizontally 
  } 
  coordinates <- c(i, j)
  return(coordinates)
}
  
show_board <- function(x) { 
  plot.new()
  
  # set the aspect ratio of the graph 

  if (x[[1]] > x[[2]]) { 
     plot.window(xlim = c(0, x[[1]]), ylim = c(0, x[[1]]), asp = 1)
  } else { 
    plot.window(xlim = c(0, x[[2]]), ylim = c(0, x[[2]]), asp = 1)
  }
  
  
  # for adding horizontal lines - rows
  for(r in 0:x[[1]]){ #x[1] is the number of rows 
    segments(x0 = 0, y0 = r, x1 = x[[2]], y1 = r)
  } 
  
  # for adding vertical lines - columns 
  for(c in 0:x[[2]]){ #x[2] is the number of columns
    segments(x0 = c, y0 = 0, x1 = c, y1 = x[[1]])
  }
  
  # numbering the board 
  c <- 1 
  for (a in 1:x[[1]]) {
    if (a %% 2 != 0) { # a is an odd row so label the numbers from left to right 
      for (b in 1:x[[2]])
      {
        text(b - 0.5, a - 0.5, c)
        c <- c + 1
      }
    } else { # a is an even row so label the numbers from right to left 
      b <- x[[2]]
      while(b >= 1) { 
        text(b - 0.5, a - 0.5, c)
        b <- b - 1
        c <- c + 1
      }
    }
  }

  #drawing the ladders
  i <- 1
  j <- 2 
  
  while(j <= length(x[[3]])) { 
    from <- get_coordinates(x[[3]][i], x[[2]])
    to <- get_coordinates(x[[3]][j], x[[2]])
    arrows(from[1] - 0.5, from[2] - 0.5, to[1] - 0.5, to[2] - 0.5, col = "green", lwd = 2)
    i <- i + 2
    j <- j + 2 
  }
  
  #drawing the chutes
  i <- 1
  j <- 2 
  
  while(j <= length(x[[4]])) { 
    from <- get_coordinates(x[[4]][i], x[[2]])
    to <- get_coordinates(x[[4]][j], x[[2]])
    arrows(from[1] - 0.5, from[2] - 0.5, to[1] - 0.5, to[2] - 0.5, col = "red", lwd = 2)
    i <- i + 2
    j <- j + 2 
  }
  
}


# random spin - function 
spin <- function() {
  sample(6, 1)
}


play_solo <- function(board, verbose = FALSE) { 
  
  # INITALIZATIONS
  
  # initialize the ladder tally 
  ladder_tally <- integer(length(board[[3]])/2)
  
  # intialize the chute tally 
  chute_tally <- integer(length(board[[4]])/2)
  
  # create a vector of all intital starting points for ladders 
  ladder_s_vec <- board[[3]][seq(1,length(board[[3]]),2)]
  ladder_starts <- ladder_s_vec[order(ladder_s_vec)]
  
  # create a vector of all corresponding ending points for ladders 
  ladder_e_vec <- board[[3]][seq(2,length(board[[3]]),2)]
  ladder_ends <- ladder_e_vec[order(ladder_s_vec)]
  
  # create a vector of all initial starting points for chutes 
  chute_s_vec <- board[[4]][seq(1,length(board[[4]]),2)]
  chute_starts <- chute_s_vec[order(chute_s_vec)]
  
  # create a vector of all initial starting points for chutes 
  chute_e_vec <- board[[4]][seq(2,length(board[[4]]),2)]
  chute_ends <- chute_e_vec[order(chute_s_vec)]
  
  # current - the digit value of the position on the board - starts at 0 
  current <- 0 
  # The turn we are on in the game
  turn <- 1 
  # create an empty vector for the move log
  move_log <- c()
  
  if(verbose == TRUE) { 
    while(current != 100) { # continue playing the game until the final score is exactly 100
      
      roll <- spin() # call spin to give a random number
      
      cat("Turn ", turn, "\n")
      cat("Start at ", current, "\n")
      cat("Spinner: ", roll, "\n")
      
      current <- current + roll 
      
      # check if its a ladder position
      if (current %in% ladder_starts) { 
        cat("Landed on: ", current, "\n")
        cat("Ladder!", "\n")
        index <- which(current == ladder_starts)
        current <- ladder_ends[index]
        # increase the ladder tally 
        ladder_tally[index] <- ladder_tally[index] + 1
      } else if (current %in% chute_starts) { # check if its a chute position
        cat("Landed on: ", current, "\n")
        cat("Chute!", "\n")
        index <- which(current == chute_starts)
        current <- chute_ends[index]
        # increase the chute tally
        chute_tally[index] <- chute_tally[index] + 1 
      }
      
      if(current > 100) {  # if current is above 100, record the same value 
        current <- current - roll 
        turn <- turn + 1 
      } else if (current == 100) { 
        current <- current 
      } else { 
        current <- current 
        turn <- turn + 1
      }
        cat("Turn ends at: ", current, "\n\n")
        move_log <- c(move_log, current)
        
    } # bracket for while function 
    } else { # bracket for if verbose is true 
      while(current != 100) { # continue playing the game until the final score is exactly 100
        
        roll <- spin() # call spin to give a random number
        
        current <- current + roll 
        
        # check if its a ladder position
        if (current %in% ladder_starts) { 
          index <- which(current == ladder_starts)
          current <- ladder_ends[index]
          # increase the ladder tally 
          ladder_tally[index] <- ladder_tally[index] + 1
        } else if (current %in% chute_starts) { # check if its a chute position
          index <- which(current == chute_starts)
          current <- chute_ends[index]
          # increase the chute tally
          chute_tally[index] <- chute_tally[index] + 1 
        }
        
        if(current > 100) {  # if current is above 100, record the same value 
          current <- current - roll 
          turn <- turn + 1 
        } else if (current == 100) { 
          current <- current 
        } else { 
          current <- current 
          turn <- turn + 1
        }
        
          move_log <- c(move_log, current)
      }
    } # else condition
  output_list <- list("turns" = turn, "chute_tally" = chute_tally, "ladder_tally" = ladder_tally, "move_log" = move_log)
  return(output_list)
} # bracket for whole function 
    
