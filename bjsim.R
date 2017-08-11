# Blackjack Simulator
# This simulation plays a perfect blackjack game
# This implementation of blackjack uses the following rules:
# Dealer stands on soft 17
# 4 or more decks
# Surrender not allowed
# Dealer peeks for blackjack
# Split allowed on any pair
# Double after split allowed
# Split tens must be same card type
# Multiple draws after split aces (to be implemented to only draw once after splitting aces)
# Can only split up to two hands (up to four hands to be implemented)


# Initialize
# Create the decks to be used
cards <- c(rep(c("A","K","Q","J",10,9,8,7,6,5,4,3,2),4))

# How many decks to use: Choose 4 or more
decks_used <- 8
deck <- rep(cards, decks_used)

# This is the full deck
full_deck <- deck



# Functions
# values an individual card
value_card <- function(card)
{
  if (card == "A")
  {
    value <- 1 # assigned its soft value
  }
  else if (card == "K" || card == "Q" || card == "J")
  {
    value <- 10
  }
  else
  {
    value <- as.numeric(card)
  }
  return(value)
}

# values a hand of two or more cards
value_hand <- function(hand) 
{
  aces <- 0
  value <- 0
  blackjack <- FALSE
  # bust <- FALSE
  for (i in hand)
  {
    if (i == "A")
    {
      aces <- aces + 1
    }
    value <- value + value_card(i)
  }
  if (aces > 0 && value < 12)
  {
    soft <- TRUE
    value_soft <- value
    value <- value + 10
  }
  else {
    soft <- FALSE
    value_soft <- value
  }
  if (aces > 0 && value == 21 && length(hand) == 2)
  {
    blackjack <- TRUE # this is a natural blackjack only (i.e. AK, AQ, AJ, A10)
  }
  #if (value_soft > 21)
  #{
  #  bust <- TRUE
  #}
  output <- list(soft, value, value_soft, blackjack)
  return(output)
}

# determines whether an initial hand can be split for the player, i.e. the player has a pair
pairs <- function(hand)
{
  if (hand[1] == hand[2])
  {
    pair <- TRUE
    value <- value_card(hand[1])
  }
  else {
    pair <- FALSE
    value <- 0
  }
  output <- list(pair, value)
  return(output)
}

# draws cards and removes them from the deck
# if we have less than one deck of 52 left, to prevent counting cards, a new full deck is taken
draw_cards <- function(n) 
{
  if (length(deck) < 52) { # replace our deck if it has less than 52 cards
    deck <<- full_deck
  }
  cards <- sample(deck, n, replace=F)
  for (i in cards)
  {
    if (is.element(i, deck))
    {
      deck <<- deck[-match(i,deck)] # global
    }    
  }
  return(cards)
}

# dealer decision making function
d_decision <- function(hand)
{
  # get the current value of the hand
  value <- value_hand(hand)[[3]] # this is soft value
  value_hard <- value_hand(hand)[[2]] # hard value
  
  # dealer stands on soft 17 (and always on hard 17)
  while (value < 17 && value_hard < 17) {
    hand <- c(hand, draw_cards(1))
    value <- value_hand(hand)[[3]]
    value_hard <- value_hand(hand)[[2]]
  }
  
  # we want the final hard value
  value <- value_hand(hand)[[2]]
  output <- list(hand, value)
  return(output)
}

# player decision making function
p_decision <- function(hand,dealer_card)
{
  soft <- value_hand(hand)[[1]]
  value <- value_hand(hand)[[2]]
  double <- FALSE
  while (value < 22) {
    if (soft == TRUE) {
      # soft hand logic
      if (value > 19) {
        break
      } else if (value == 19) {
        # Oops, coded hit on hard 17 logic
        # if (dealer_card == 6) {
        #   double <- TRUE
        #   hand <- c(hand, draw_cards(1))
        #   # value <- value_hand(hand)[[2]]
        #   break
        #}
        #else {
        #  break
        #}
        break # correct soft 17 logic
      } else if (value == 18) {
        if (dealer_card < 7 && dealer_card > 2) {
          if (length(hand) == 2) {
          double <- TRUE
          hand <- c(hand, draw_cards(1))
          # value <- value_hand(hand)[[2]]
          break
          } else {
            break
          }
        } else if (dealer_card < 9) {
          break
        } else {
          hand <- c(hand, draw_cards(1))
          value <- value_hand(hand)[[2]]
        }
      } else if (dealer_card > 6 || (dealer_card < 5 && value == 13) || (dealer_card < 5 && value == 14) || 
                 (dealer_card < 4 && value == 15) || (dealer_card < 4 && value == 16) || (dealer_card < 3 && value == 17)){
        hand <- c(hand, draw_cards(1))
        value <- value_hand(hand)[[2]]
      } else {
        if (length(hand) == 2) {
          double <- TRUE
          hand <- c(hand, draw_cards(1))
          # value <- value_hand(hand)[[2]]
          break
        } else {
          hand <- c(hand, draw_cards(1))
          value <- value_hand(hand)[[2]]
        }
      }
      
    } else { # hard hand logic
      if (value > 16) {
        break
      } else if ((value > 11 && dealer_card > 6) || (value == 12 && dealer_card < 4)) {
        hand <- c(hand, draw_cards(1))
        value <- value_hand(hand)[[2]]
      } else if ((value == 11 && dealer_card < 11) || (value == 10 && dealer_card < 10) || (value == 9 && dealer_card < 7 && dealer_card > 2)) {
        
        if (length(hand) == 2) {
        double <- TRUE
        hand <- c(hand, draw_cards(1))
        break
        } else {
          hand <- c(hand, draw_cards(1))
          value <- value_hand(hand)[[2]] 
        }
        break        
      } else if (value < 11) {
        hand <- c(hand, draw_cards(1))
        value <- value_hand(hand)[[2]]        
      } else {
        break
      }
    }
  }
  
  # if (double == TRUE) {
  #   bet <<- bet*2
  #   cash <<- cash - bet
  # }
  value <- value_hand(hand)[[2]]       
  output <- list(hand, value, double)
  return(output)
}

game <- function(){
  p_hand <- draw_cards(2)
  d_hand <- draw_cards(2)
  dealer_card <- value_card(d_hand[1]) # we assume the first dealer card is visible to player and the second is hidden
  if (dealer_card == 1){
    dealer_card <- 11 # here we need to make aces worth 11 to make our logic work
  }
  
  # step 2: check for blackjack
  if (value_hand(p_hand)[[4]] == TRUE){ # player has blackjack
    if (value_hand(d_hand)[[4]] == TRUE){ # dealer also has blackjack
      p_value <- 21
      d_value <- 21
      p_final <- p_hand
      d_final <- d_hand
      p_doubled <- FALSE
      blackjack_win <- TRUE
      p_split <- FALSE
      s_final <- NULL
      s_value <- NULL
      s_doubled <- NULL
    }
    else {
      p_value <- 21
      p_final <- p_hand
      d_final <- d_hand
      p_doubled <- FALSE
      d_value <- value_hand(d_hand)[[2]]
      blackjack_win <- TRUE
      p_split <- FALSE
      s_final <- NULL
      s_value <- NULL
      s_doubled <- NULL
    }
  } else if (value_hand(d_hand)[[4]] == TRUE) { # dealer has blackjack
    d_value <- 21
    p_value <- value_hand(p_hand)[[2]]
    d_final <- d_hand
    p_final <- p_hand
    p_doubled <- FALSE
    blackjack_win <- TRUE
    p_split <- FALSE
    s_final <- NULL
    s_value <- NULL
    s_doubled <- NULL
  } else { # no one has blackjack
    
    # check for split possibility
    pair_check <- pairs(p_hand)
    p_split <- FALSE
    if (pair_check[[1]] == TRUE) { # player has a pair that can be split
      pair <- pair_check[[2]]
      if (pair == 1 || pair == 8) { # ace or 8
        p_split <- TRUE
      } else if (pair == 2 || pair == 3 || pair == 7) {
        if (dealer_card < 8) {
          p_split <- TRUE # always split
        }
      } else if (pair == 4) {
        if (dealer_card > 4 && dealer_card < 7) {
          p_split <- TRUE
        }
      } else if (pair == 6) {
        if (dealer_card < 7) {
          p_split <- TRUE
        }
      } else if (pair == 9) {
        if (dealer_card < 10 && dealer_card != 7) {
          p_split <- TRUE
        }
      }
    }
    
    if (p_split == TRUE) {
      p_hand <- c(p_hand[1], draw_cards(1))
      s_hand <- c(p_hand[1], draw_cards(1))
      
      p_dec <- p_decision(p_hand,dealer_card)
      s_dec <- p_decision(s_hand,dealer_card)
      d_dec <- d_decision(d_hand)
      p_value <- p_dec[[2]]
      s_value <- s_dec[[2]]
      d_value <- d_dec[[2]]
      p_doubled <- p_dec[[3]]
      s_doubled <- s_dec[[3]]
      p_final <- p_dec[[1]]
      s_final <- s_dec[[1]]
      d_final <- d_dec[[1]]
      blackjack_win <- FALSE
      
    } else {
    
    p_dec <- p_decision(p_hand,dealer_card)
    d_dec <- d_decision(d_hand)
    p_value <- p_dec[[2]]
    d_value <- d_dec[[2]]
    p_doubled <- p_dec[[3]]
    p_final <- p_dec[[1]]
    d_final <- d_dec[[1]]
    blackjack_win <- FALSE
    s_final <- NULL
    s_value <- NULL
    s_doubled <- NULL
    
    }
  }
  output <- list(p_final,p_value,p_doubled,d_final,d_value,blackjack_win,p_split,s_final,s_value,s_doubled)
  return(output)
}

# takes output from game() function and processes player winnings
winnings <- function(output){

  case <- 0
  case_2 <- 0
  
  # Check blackjack: Do we have a natural blackjack at start?
  if (output[[6]] == TRUE) {
    
    # messages pertaining to splits and doubles must not apply
    message_2 <- ""
    mult_2 <- ""
    message_split <- ""
    message_doubled_2 <- ""
    message_doubled <- ""
    
    if (output[[2]] == 21 && output[[5]] == 21) {
      mult <- 1 # push
      message <- "Push: Both players have blackjack"
      case <- 6
    } else if (output[[2]] == 21 && output[[5]] != 21) {
      mult <- 2.5 # p has blackjack
      message <- "Player wins: blackjack"
      case <- 2
    } else if (output[[2]] != 21 && output[[5]] == 21) {
      mult <- 0 # d has blackjack
      message <- "Dealer wins: blackjack"
      case <- 4
    }
  } 
  
  # If player opts to split
  else if (output[[7]] == TRUE) {
    
    # split messages here
    message_split <- "Player had a pair and split to two hands"
    
    # If the second hand busts
    if (output[[9]] > 21) { 
      message_2 <- "Dealer wins: Player bust on second hand"
      case_2 <- 5
      mult_2 <- 0
      if (output[[2]] > 21) { # first hand bust as well
        mult <- 0
        message <- "Dealer wins: Player bust on first hand"
        case <- 5
      } else if (output[[5]] > 21) {
        mult <- 2 # d busts
        message <- "Player wins on first hand: Dealer bust"
        case <- 1
      } else if (output[[5]] == output[[2]]) {
        mult <- 1 # push
        message <- "Push on first hand"
        case <- 3
      } else if (output[[5]] > output[[2]]) {
        mult <- 0 # d wins
        message <- "Dealer wins on first hand"
        case <- 4
      } else {
        mult <- 2 # p wins
        message <- "Player wins on first hand"
        case <- 1
      }
    } 
    
    # If the first hand busts
    else if (output[[2]] > 21) {
      message <- "Dealer wins: Player bust on first hand"
      case <- 5
      mult <- 0
      if (output[[5]] > 21) {
        mult_2 <- 2 # d busts
        message_2 <- "Player wins on second hand: Dealer bust"
        case_2 <- 1
      } else if (output[[5]] == output[[9]]) {
        mult_2 <- 1 # push
        message_2 <- "Push on second hand"
        case_2 <- 3
      } else if (output[[5]] > output[[9]]) {
        mult_2 <- 0 # d wins
        message_2 <- "Dealer wins on second hand"
        case_2 <- 4
      } else {
        mult_2 <- 2 # p wins
        message_2 <- "Player wins on second hand"
        case_2 <- 1
      }
    } 
    
    # If neither hand busts
    else {
      
      # If the dealer busted
      if (output[[5]] > 21) {
        mult <- 2
        mult_2 <- 2
        message <- "Player wins on first hand: Dealer bust"
        message_2 <- "Player wins on second hand: Dealer bust"
        case <- 1
        case_2 <- 1
      } 
      
      # If the dealer didn't bust
      else {
      
        # Check each hand separately
        
        # Check the first hand
        if (output[[5]] == output[[2]]) {
          mult <- 1 # push
          message <- "Push on first hand"
          case <- 3
        } else if (output[[5]] > output[[2]]) {
          mult <- 0 # d wins
          message <- "Dealer wins on first hand"
          case <- 4
        } else {
          mult <- 2 # p wins
          message <- "Player wins on first hand"
          case <- 1
        }
        
        
        # Check the second hand
        if (output[[5]] == output[[9]]) {
          mult_2 <- 1 # push
          message_2 <- "Push on second hand"
          case <- 3
        } else if (output[[5]] > output[[9]]) {
          mult_2 <- 0 # d wins
          message_2 <- "Dealer wins on second hand"
          case <- 4
        } else {
          mult_2 <- 2 # p wins
          message_2 <- "Player wins on second hand"
          case <- 1
        }
      } # end of the "if dealer didn't bust"
    } # end of the "if neither hand busted"
    
    
    # Player allowed to double after split: check doubling
    # Check if they doubled on first hand
    if (output[[3]] == TRUE) {
      mult <- 2*mult
      message_doubled <- "Player doubled bet on first hand"
    } else {
      message_doubled <- ""
    }
    
    # Check if they doubled on second hand
    if (output[[10]] == TRUE) { # doubled
      mult_2 <- 2*mult_2
      message_doubled_2 <- "Player doubled bet on second hand"
    } else {
      message_doubled_2 <- ""
    }
    
  } # end of the player spitting scenarios
  
  # Assuming the player does not split
  else { 
    
    # the output messages relating to splitting only must be nulled
    message_2 <- ""
    mult_2 <- ""
    message_split <- ""
    message_doubled_2 <- ""
    
    # Player's hand busted
    if (output[[2]] > 21) {
      mult <- 0
      message <- "Dealer wins: Player bust"
      case <- 5
    } 
    
    # Dealer's hand busted
    else if (output[[5]] > 21) {
      mult <- 2 # d busts
      message <- "Player wins: Dealer bust"
      case <- 1
    } 
    
    # Player and dealer push
    else if (output[[5]] == output[[2]]) {
      mult <- 1 # push
      message <- "Push"
      case <- 3
    } 
    
    # Dealer is higher
    else if (output[[5]] > output[[2]]) {
      mult <- 0 # d wins
      message <- "Dealer wins"
      case <- 4
    } 
    
    # Player is higher
    else {
      mult <- 2 # p wins
      message <- "Player wins"
      case <- 1
    }
    
    # We check if the hand was doubled
    if (output[[3]] == TRUE) { # doubled
      mult <- 2*mult
      message_doubled <- "Player doubled bet"
    } else {
      message_doubled <- ""
    } # end of doubling
  } # end of no split
  
  result <- c(mult,message,message_doubled,message_split,mult_2,message_2,message_doubled_2,case,case_2)
  return(result)
}

# this runs the simulation
# n is the number of hands to play
# bet is the bet size per hand (uniform bet size)
simulation <- function(n,bet) {
 cash <- 0
 balance <- cash
 
 win_count <- 0
 bj_count <- 0
 push_count <- 0
 lose_count <- 0
 bust_count <- 0
 both_bj_count <- 0
 double_count <- 0
 double_win <- 0
 double_lose <- 0
 double_push <- 0

 for (i in 1:n) {
   print("====================================================")
   print(paste("Game",i,"| Bet size:",bet,sep=" "))
   cash <- cash - bet
   game_step <- game()
   
   # No split
   if (game_step[[7]] == FALSE) {
   print(paste("Player's hand:",paste(game_step[[1]], collapse=','),"| Value",game_step[[2]],sep=" "))
   } 
   
   # Player split
   else {
     print(paste("Player's first hand:",paste(game_step[[1]], collapse=','),"| Value",game_step[[2]],sep=" "))
     print(paste("Player's second hand:",paste(game_step[[8]], collapse=','),"| Value",game_step[[9]],sep=" "))
     }
   
   print(paste("Dealer's hand:",paste(game_step[[4]], collapse=','),"| Value",game_step[[5]],sep=" "))
   
   # calculate winnings
   winner <- winnings(game_step)
   
   if (winner[[8]] == 1) {
     win_count <- win_count + 1
   } else if (winner[[8]] == 2) {
     win_count <- win_count + 1
     bj_count <- bj_count + 1
   } else if (winner[[8]] == 3) {
     push_count <- push_count + 1
   } else if (winner[[8]] == 4) {
     lose_count <- lose_count + 1
   } else if (winner[[8]] == 5) {
     bust_count <- bust_count + 1
     lose_count <- lose_count + 1
   } else if (winner[[8]] == 6) {
     both_bj_count <- both_bj_count + 1
     push_count <- push_count + 1
   }
   
   if (winner[[9]] == 1) {
     win_count <- win_count + 1
   } else if (winner[[9]] == 2) {
     win_count <- win_count + 1
     bj_count <- bj_count + 1
   } else if (winner[[9]] == 3) {
     push_count <- push_count + 1
   } else if (winner[[9]] == 4) {
     lose_count <- lose_count + 1
   } else if (winner[[9]] == 5) {
     bust_count <- bust_count + 1
     lose_count <- lose_count + 1
   } else if (winner[[9]] == 6) { # this scenario is not possible but let's keep it
     both_bj_count <- both_bj_count + 1
     push_count <- push_count + 1
   }
   
   # No split
   if (game_step[[7]] == FALSE) {
     payout_mult <- as.numeric(winner[[1]])
     print(winner[[2]])
     if (winner[[3]] != "")
     {
       print(winner[[3]])
       cash <- cash - bet
       double_count <- double_count + 1
       if (winner[[8]] == 1 || winner[[8]] == 2) {
         double_win <- double_win + 1
       } else if (winner[[8]] == 3 || winner[[8]] == 6) {
         double_push <- double_push + 1
       } else if (winner[[8]] == 4 || winner[[8]] == 5) {
         double_lose <- double_lose + 1
       }
     }
     cash <- cash + payout_mult*bet
     balance <- c(balance,cash)
     print(paste("Player bet:",
                 if (winner[[3]] != ""){
                   paste(bet,"doubled to",bet*2,sep=" ")
                 } else{bet},
                 "| Earned:",payout_mult*bet,"| New balance:",cash,sep=" "))
   } 
   
   # Player split
   else {
     cash <- cash - bet # split incurs extra bet
     # print("%%%%%%%%%%%%%%%%%%%%%% SPLIT ALERT %%%%%%%%%%%%%%%%%%%%%%")
     payout_mult <- as.numeric(winner[[1]])
     payout_mult_2 <- as.numeric(winner[[5]])
     print(winner[[2]])
     print(winner[[6]])
     if (winner[[3]] != "")
     {
       print(winner[[3]])
       cash <- cash - bet
       double_count <- double_count + 1
       if (winner[[8]] == 1 || winner[[8]] == 2) {
         double_win <- double_win + 1
       } else if (winner[[8]] == 3 || winner[[8]] == 6) {
         double_push <- double_push + 1
       } else if (winner[[8]] == 4 || winner[[8]] == 5) {
         double_lose <- double_lose + 1
       }
     }
     if (winner[[7]] != "")
     {
       print(winner[[7]])
       cash <- cash - bet
       double_count <- double_count + 1
       if (winner[[9]] == 1 || winner[[9]] == 2) {
         double_win <- double_win + 1
       } else if (winner[[9]] == 3 || winner[[9]] == 6) {
         double_push <- double_push + 1
       } else if (winner[[9]] == 4 || winner[[9]] == 5) {
         double_lose <- double_lose + 1
       }
     }
     cash <- cash + payout_mult*bet + payout_mult_2*bet
     balance <- c(balance,cash)
     if (winner[[3]] != "" && winner[[7]] != "") {
       custom_msg <- "increased by split and then doubled twice to"
       end_bet <- bet*4
     } else if (winner[[3]] != "" || winner[[7]] != "") {
       custom_msg <- "increased by split and then doubled once to"
       end_bet <- bet*3
     } else {
       custom_msg <- "increased by split to"
       end_bet <- bet*2
     }
     print(paste("Player bet:",bet,custom_msg,end_bet,
                 "| Earned:",(payout_mult+payout_mult_2)*bet,"| New balance:",cash,sep=" "))
     
   }
 }
 print("====================================================")
 print(paste0("You played ",n," rounds, each with an initial bet of $",bet,"."))
 print(paste0("Ending profit/loss: $",tail(balance,n=1)))
 print(paste0("Winning hands: ",win_count,", including ",bj_count," blackjacks"))
 print(paste0("Losing hands: ",lose_count,", including ",bust_count," bust hands"))
 print(paste0("Tied hands: ",push_count,", including ",both_bj_count," tied blackjack hands"))
 print(paste0("You doubled ",double_count," hands, won ",double_win," of them, lost ",double_lose," of them, and tied ",double_push," of them."))
 print("All summary statistics above count split hands as separate hands")
 print("====================================================")
 print("How your balance fluctuated is output below:")
 return(balance)
}


# Run the simulation
# The first variable changes the number of rounds to play
# The second variable changes the initial bet size for each round
n <- 1000
bet_size <- 100
# simulation(n,bet_size)

# Let's run multiple simulations and plot them
number_of_simulations <- 200

# Plot the line charts
# plot(0,type="n",ylim=c(-100*bet_size,100*bet_size), xlim=c(0, n+1), xlab="Round", ylab="Profit/loss")
# title(paste("Blackjack sim,",n,"hands played,","bet size",bet_size,sep=" "))
# colors <- rainbow(number_of_simulations)
# for (i in 1:number_of_simulations) {
#    invisible(capture.output(balances <- simulation(n,bet_size))) # I suppress the printed messages
#    lines(balances, col=colors[i],lwd=2,lty=1)
# }

# Or, plot a histogram/density plot of ending profit/loss
histogram <- NULL
for (i in 1:number_of_simulations) {
    invisible(capture.output(balances <- simulation(n,bet_size)))
    histogram <- c(histogram,tail(balances,n=1))# I suppress the printed messages
    print(paste("Step",i))
}

# Histogram
# hist(histogram, breaks=20, main=paste("Histogram of",number_of_simulations,"simulations"), col="gold", xlab="Ending Profit/Loss")

# Or a density plot
plot(density(histogram), main="Blackjack simulation density plot", col="gold", lwd=2, xlab="Profit/Loss", ylab="Density")
polygon(density(histogram), col=rgb(1,215/255,0,0.2), border="gold")
abline(v=0, lty=2)
