library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
# Continous Double Auction

open_market <- function(){
bid_agent <<- c() 
ask_agent <<- c() 
redemption_value_index <<- c() 
cost_index <<- c() 
rv_realized <<- c()
cost_realized <<- c() 
bids_accepted <<- c()
asks_accepted <<- c()
price <<- c()
bid_index <<- c()
ask_index <<- c()
q_bid_index <<- c() 
q_ask_index <<- c() 
q_bid <<- c() 
q_ask <<- c()
quantity <<- c() 
price_quantity <<- c()
pq_value_traded <<- c() 
q_bid_accepted <<- c()
q_ask_accepted <<-c()  
}
open_market()


ZI_buy <- function () {
  buyer_bot <<- sample(1:5, 1, replace = TRUE)
  budget <<- 200
  redemption_value <<- sample(1:budget, size = 1, replace = TRUE)
  range_b <- 1:redemption_value 
amount_to_buy <- sample(1:1, size = 1, replace = TRUE)
bid_value <-  sample(range_b, size = 1, replace = TRUE)
if(bid_value*amount_to_buy <= budget){
  bid_index <<- append(bid_index,bid_value)
  q_bid_index <<- append(q_bid_index, amount_to_buy)
  q_bid <<- tail(q_bid_index, 1) 
  bid <<- tail(bid_index, 1) 

} else{
 ZI_buy()
}
}
ZI_buy() 




ZI_sell <- function() {
  seller_bot <<- sample(1:5, 1, replace = TRUE)
  amount_max <- 1
  cost <<- sample(1:100, size = 1, replace = TRUE)
  range_s <- cost:200 
  amount_to_sell <- sample(1:amount_max, size = 1, replace = TRUE)
  ask_value <-  sample(range_s, size = 1, replace = TRUE)
  if(ask_value*amount_to_sell){
  ask_index <<- append(ask_index, ask_value)
  ask <<- tail(ask_index, 1)
  q_ask_index <<- append(q_ask_index, amount_to_sell) 
  q_ask <<- tail(q_ask_index, 1) 

  } else{
    ZI_sell()
  }
}
ZI_sell()

order <- function(){
    open_outcry <- sample(0:1, size = 1, replace = TRUE)
    if(open_outcry == 1){
      #for(i in 1:3)
   ZI_buy()
      
    } else {
   ZI_sell()
    }
    if(is.null(q_bid) | is.null(q_ask)){
      order()
    }
    }
order()


####################### trade ######

trade <- function(){
  k <- 0
  n <- 0
  open_market()
  while(n < 100) {
    order()
    match <- bid >= ask
    q_match <<- q_bid <= q_ask
    k <- k + 1
    if(match){
      n <- n + 1
      transaction_number <<- c(1:n)
      spread_price <- (bid + ask) / 2
      bids_accepted <<- append(bids_accepted, bid) 
      asks_accepted <<- append(asks_accepted, ask) 
      price <<- append(price, spread_price) 
      rv_realized <<- append(rv_realized, redemption_value)
      cost_realized <<- append(cost_realized, cost)
      bid_agent <<- append(bid_agent, buyer_bot)
      ask_agent <<- append(ask_agent, seller_bot)
      if(q_match) {
        quantity <<- append(quantity, q_bid)
        q_bid_accepted <<- append(q_bid_accepted, q_bid) 
 } else {
      quantity<<- append(quantity, q_ask)
      q_ask_accepted <<- append(q_ask_accepted, q_ask) 
 }
      price_quantity <<- tail(price, 1) * tail(quantity, 1)
      pq_value_traded <<- append(pq_value_traded, price_quantity) 
    } 
  }
  rounds <- c(k)
  number_of_transactions <- c(n)
  transactions_per_round <<- c(number_of_transactions)/c(rounds)
  #
    market_transaction_index <<- data.frame(transaction_number = c(transaction_number), price = c(price), ask = c(asks_accepted), bid = c(bids_accepted), quantity = c(quantity), pq_value_traded = c(pq_value_traded), ask_agent, bid_agent)
    #plot 
   Price_Plot <<-  ggplot(data = market_transaction_index, aes(x = transaction_number)) +geom_line(aes(y=price)) 

}

trade()

######################################## Reporting #################################

Reporting <- function(){

  mean_market_price <<- mean(price)

  market_quantity <<- cumsum(quantity) 
  sorted_rv_realized <<- sort(rv_realized, decreasing = TRUE)
  sorted_cost_realized <<- sort(cost_realized)
  
  for(i in transaction_number){
    if(sorted_rv_realized[i] <= sorted_cost_realized[i]){
      print("equilibrium quantity:")
      truncate <- i - 1
      print(truncate)
      equilibrium_price <- (sorted_rv_realized[i - 1]+sorted_cost_realized[i])/2
      print("equilibrium price")
      print(equilibrium_price)
      break
    }
  }
  MP_DF <<- data.frame(sorted_rv_realized, sorted_cost_realized, transaction_number, quantity, market_quantity)

  MP_Truncated <<- head(MP_DF, truncate)
  Marshallian_path <<- sum(MP_Truncated$sorted_rv_realized - MP_Truncated$sorted_cost_realized)
  print("Marshallian Path:")
  print(Marshallian_path)
  payoffs_realized <<- (rv_realized - cost_realized)
  sum_payoffs <<- sum(payoffs_realized)
  print("payoffs_realized")
  print(sum_payoffs)
  allocative_efficiency <<- sum_payoffs/Marshallian_path
  print("Efficiency:")
  print(allocative_efficiency)
  RV_COST_PLOT <- ggplot(data = MP_DF, aes(x = market_quantity)) +
    geom_line(aes(y=sorted_rv_realized)) +
    geom_line(aes(y=sorted_cost_realized)) 
  
  
  print(RV_COST_PLOT + labs(y="Value or Cost", x = "Quantity"))
 # require(gridExtra)
  #plot1 <- qplot(1)
  #plot2 <- qplot(1)
  grid.arrange(Price_Plot, RV_COST_PLOT,  nrow=2)
}
Reporting()




  