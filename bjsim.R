cards <- c(rep(c("A","K","Q","J",10,9,8,7,6,5,4,3,2),4))
decks_used <- 8
deck <- rep(cards, decks_used)
full_deck <- deck

value_card <- function(card) # values an individual card
{
  if (card == "A")
  {
    value <- 1
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

value_hand <- function(hand) # values a hand
{
  aces <- 0
  value <- 0
  blackjack <- FALSE
  bust <- FALSE
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
    blackjack <- TRUE # this is a natural blackjack only 
  }
  if (value_soft > 21)
  {
    bust <- TRUE
  }
  output <- list(soft, value, value_soft, blackjack, bust)
  return(output)
}

pairs <- function(hand) # determines whether an initial hand can be split for the player
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

draw_cards <- function(n) # draws cards and removes them from the deck
{
  if (length(deck) < 53) {
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

d_decision <- function(hand)
{
  value <- value_hand(hand)[[3]]
  value_hard <- value_hand(hand)[[2]]
  while (value < 17 && value_hard < 17) {
    hand <- c(hand, draw_cards(1))
    value <- value_hand(hand)[[3]]
    value_hard <- value_hand(hand)[[2]]
  }
  value <- value_hand(hand)[[2]]
  output <- list(hand, value)
  return(output)
}

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
        if (dealer_card == 6) {
          double <- TRUE
          hand <- c(hand, draw_cards(1))
          # value <- value_hand(hand)[[2]]
          break
        }
        else {
          break
        }
      } else if (value == 18) {
        if (dealer_card < 7) {
          double <- TRUE
          hand <- c(hand, draw_cards(1))
          # value <- value_hand(hand)[[2]]
          break
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
        double <- TRUE
        hand <- c(hand, draw_cards(1))
        # value <- value_hand(hand)[[2]]
        break
      }
      
    } else { # hard hand logic
      if (value > 16) {
        break
      } else if ((value > 11 && dealer_card > 6) || (value == 12 && dealer_card < 4)) {
        hand <- c(hand, draw_cards(1))
        value <- value_hand(hand)[[2]]
      } else if ((value == 11) || (value == 10 && dealer_card < 10) || (value == 9 && dealer_card < 7 && dealer_card > 2)) {
        double <- TRUE
        hand <- c(hand, draw_cards(1))
        # value <- value_hand(hand)[[2]]
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

# step 1: both sides draw cards
bet <- 10
cash <- cash - bet

game <- function(){
  p_hand <- draw_cards(2)
  d_hand <- draw_cards(2)
  dealer_card <- value_card(d_hand[1])
  if (dealer_card == 1){
    dealer_card <- 11
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
    }
    else {
      p_value <- 21
      p_final <- p_hand
      d_final <- d_hand
      p_doubled <- FALSE
      d_value <- value_hand(d_hand)[[2]]
      blackjack_win <- TRUE
      p_split <- FALSE
    }
  } else if (value_hand(d_hand)[[4]] == TRUE) { # dealer has blackjack
    d_value <- 21
    p_value <- value_hand(p_hand)[[2]]
    d_final <- d_hand
    p_final <- p_hand
    p_doubled <- FALSE
    blackjack_win <- TRUE
    p_split <- FALSE
  } else { # no one has blackjack
    
    # # check for split possibility
    # p_split = FALSE
    # if (pairs(p_hand)[[1]] == TRUE) { # player has a pair that can be split
    #   pair <- pairs(p_hand)[[2]]
    #   if (pair == 1 || pair == 8) { # ace or 8 
    #     p_split = TRUE
    #   } else if (pair == 2 || pair == 3 || pair == 7) {
    #     if (dealer_card < 8) {
    #       p_split = TRUE
    #     }
    #   } else if (pair == 4) {
    #     if (dealer_card > 4 && dealer_card < 7) {
    #       p_split = TRUE
    #     }
    #   } else if (pair == 6) {
    #     if (dealer_card < 7) {
    #       p_split = TRUE
    #     }
    #   } else if (pair == 9) {
    #     if (dealer_card < 10 && dealer_card != 7) {
    #       p_split = TRUE
    #     }
    #   }
    # }
    # if (p_split = TRUE) {
    #   p_hand <- p_hand[1]
    #   p_hand_s <- p_hand[2]
    # }
    
    p_dec <- p_decision(p_hand,dealer_card)
    d_dec <- d_decision(d_hand)
    p_value <- p_dec[[2]]
    d_value <- d_dec[[2]]
    p_doubled <- p_dec[[3]]
    p_final <- p_dec[[1]]
    d_final <- d_dec[[1]]
    blackjack_win <- FALSE
  }
  output <- list(p_final,p_value,p_doubled,d_final,d_value,blackjack_win) #to add s_final, s_value, s_doubled
  return(output)
}

winnings <- function(output){
  if (output[[6]] == TRUE) {
    if (output[[2]] == 21 && output[[5]] == 21) {
      mult <- 1 # push
      message <- "Push: Both players have blackjack"
    } else if (output[[2]] == 21 && output[[5]] != 21) {
      mult <- 2.5 # p has blackjack
      message <- "Player wins: blackjack"
    } else if (output[[2]] != 21 && output[[5]] == 21) {
      mult <- 0 # d has blackjack
      message <- "Dealer wins: blackjack"
    }
  } else {
    if (output[[2]] > 21) {
      mult <- 0 # p busts
      message <- "Dealer wins: Player bust"
    } else if (output[[5]] > 21) {
      mult <- 2 # d busts
      message <- "Player wins: Dealer bust"
    } else if (output[[5]] == output[[2]]) {
      mult <- 1 # push
      message <- "Push"
    } else if (output[[5]] > output[[2]]) {
      mult <- 0 # d wins
      message <- "Dealer wins"
    } else {
      mult <- 2 # p wins
      message <- "Player wins"
    }
  }
  if (output[[3]] == TRUE) { # doubled
    mult <- 2*mult
    message_doubled <- "Player doubled bet"
  } else {
    message_doubled <- ""
  }
  result <- c(mult,message,message_doubled)
  return(result)
}

simulation <- function(n,bet) {
 cash <- 1000
 balance <- cash
 for (i in 1:n) {
   print("====================================================")
   print(paste("Game",i,"| Bet size:",bet,sep=" "))
   cash <- cash - bet
   game_step <- game()
   # print(game_step)
   print(paste("Player hand:",paste(game_step[[1]], collapse=','),"| Value",game_step[[2]],sep=" "))
   print(paste("Dealer hand:",paste(game_step[[4]], collapse=','),"| Value",game_step[[5]],sep=" "))
   winner <- winnings(game_step)
   payout_mult <- as.numeric(winner[[1]])
   print(winner[[2]])
   if (winner[[3]] != "")
   {
     print(winner[[3]])
     cash <- cash - bet
   }
   cash <- cash + payout_mult*bet
   balance <- c(balance,cash)
   print(paste("Player bet:",
               if (winner[[3]] != ""){
                 paste(bet,"doubled to",bet*2,sep=" ")
                 } else{bet},
               "| Earned:",payout_mult*bet,"| New balance:",cash,sep=" "))
 }
 return(balance)
}

title(paste("Blackjack simulation, no split allowed"),paste(n,"hands played","bet size",bet_size,sep=" "))
n <- 1000
bet_size <- 100
sims <- 5
plot(0,type="n",ylim=c(-7000, 7000), xlim=c(0, 1000))
for (i in 1:sims) {
  balances <- simulation(n,bet_size)
  lines(balances)
}