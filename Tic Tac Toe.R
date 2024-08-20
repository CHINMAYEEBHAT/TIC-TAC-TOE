# Function to initialize the game board
initialize_board <- function() {
  board <- matrix(" ", nrow = 3, ncol = 3)
  rownames(board) <- c("1", "2", "3")
  colnames(board) <- c("A", "B", "C")
  return(board)
}

# Function to print the game board
print_board <- function(board) {
  print(board)
}

# Function to check if the move is valid
is_valid_move <- function(board, row, col) {
  if (row %in% 1:3 && col %in% c("A", "B", "C") && board[row, col] == " ") {
    return(TRUE)
  }
  return(FALSE)
}

# Function to make a move
make_move <- function(board, player, row, col) {
  board[row, col] <- player
  return(board)
}

# Function to check if the game is over
is_game_over <- function(board) {
  # Check rows
  for (i in 1:3) {
    if (board[i,1] != " " && board[i,1] == board[i,2] && board[i,1] == board[i,3]) {
      return(TRUE)
    }
  }
  
  # Check columns
  for (i in 1:3) {
    if (board[1,i] != " " && board[1,i] == board[2,i] && board[1,i] == board[3,i]) {
      return(TRUE)
    }
  }
  
  # Check diagonals
  if (board[1,1] != " " && board[1,1] == board[2,2] && board[1,1] == board[3,3]) {
    return(TRUE)
  }
  if (board[1,3] != " " && board[1,3] == board[2,2] && board[1,3] == board[3,1]) {
    return(TRUE)
  }
  
  # Check for draw
  if (!any(board == " ")) {
    return(TRUE)
  }
  
  return(FALSE)
}

# Function to determine the winner
get_winner <- function(board) {
  for (i in 1:3) {
    if (board[i,1] != " " && board[i,1] == board[i,2] && board[i,1] == board[i,3]) {
      return(board[i,1])
    }
  }
  
  for (i in 1:3) {
    if (board[1,i] != " " && board[1,i] == board[2,i] && board[1,i] == board[3,i]) {
      return(board[1,i])
    }
  }
  
  if (board[1,1] != " " && board[1,1] == board[2,2] && board[1,1] == board[3,3]) {
    return(board[1,1])
  }
  if (board[1,3] != " " && board[1,3] == board[2,2] && board[1,3] == board[3,1]) {
    return(board[1,3])
  }
  
  return(NULL)
}

# Function to play the game
play_game <- function() {
  board <- initialize_board()
  player <- "X"
  
  while (!is_game_over(board)) {
    print_board(board)
    cat(paste("Player ", player, ", enter your move (e.g., 1A): "))
    move <- readline()
    row <- as.numeric(substr(move, 1, 1))
    col <- substr(move, 2, 2)
    
    if (is_valid_move(board, row, col)) {
      board <- make_move(board, player, row, col)
      if (is_game_over(board)) {
        print_board(board)
        winner <- get_winner(board)
        if (!is.null(winner)) {
          cat(paste("Player ", winner, " wins!\n"))
        } else {
          cat("It's a draw!\n")
        }
      } else {
        player <- ifelse(player == "X", "O", "X")
      }
    } else {
      cat("Invalid move. Please try again.\n")
    }
  }
}

# Start the game
play_game()

