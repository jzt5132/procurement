closeAllConnections()
rm(list=ls())
gc()
library(ggplot2)
library(knitr)
library(dplyr)
library(magrittr)
library(evd)
require(tsoutliers)
library(forecast)
#set.seed(19888)
set.seed(1988)
#set.seed(1)

N_ = 10000
# supply_seed = c(500, 490, 505, 520, 202, 301, 380, 450, 500, 510)
#random_gumble = rgumbel(100, loc = 10, scale = 10)
random_gumble = rgumbel(N_, loc = 10, scale = 10)
# hist(random_gumble)
supply_seed = 113 - random_gumble
# hist(supply_seed)
supply_seed = as.integer(supply_seed)   # rbinom must take integer as parameter
bid_price_loop = list()
bid_quantity_loop = list()
b_e_loop = list()
bid_open = list()
bid_close = list()
S_loop = list()
transaction_loop = list()
number_encounter = list()
number_transaction = list()
unsatisfied_demand = list()
unsatisfied_ratio = list()
total_demand_loop = list()
total_supply_loop = list()
HHI_loop = list()


Z_loop = list()
Z_3_loop = list()
D_loop = list()

K_ = 10 #number of suppliers
I_ = 100
WC_loop = rweibull(I_*K_*N_, shape = 1, scale = 1)
WC_loop = matrix(data = WC_loop, nrow = K_*I_, ncol = N_) #each column corresponds to a particualr loop
stc = rnorm(K_, 10, 5)


for (loop in 1:N_){
  local_supply_seed = supply_seed[loop]
  
  print(loop)
  a = 100 #parameter in search cost sc= a * EZ^2 * s_k
  b = 50
  K = 1:K_
  I = 1:I_
  
  
  D_ = rnorm(I_, 10, 3) + 0.2
  S_ = rnorm(K_, supply_seed[loop], 1)
  S_ = abs(S_)
  
  Z_loop[[loop]] = sum(D_) - sum(S_)
  D_loop[[loop]] = sum(D_)
  
  #D_ = D_ + D_carriedover
  c = rnorm(I_, 100,  5)
  
  #S_ = S_ + S_carriedover
  D = D_
  S = S_
  Z = sum(D) - sum(S)
  Z_3_loop[[loop]] = Z
  HHI =  sum((S/sum(S))^2)
  HHI_loop[[loop]] = HHI
  
  
  ################################################
  ############## Competitive Mkt  ################
  ################################################
  
  #Equilibrium Price Calculation Function----
  b_e_fn = function(D, S, c){
    WTP = c
    most_want_to_less_want_order = order( - WTP)
    if (sum(D) < sum(S)){
      b_e = min(c)
    }
    else{
      tmp = 0
      d_tmp = 0
      while (d_tmp - sum(S) < 0){
        tmp = tmp + 1 
       # print(tmp)
        d_tmp = d_tmp + D[most_want_to_less_want_order[tmp]]
      }
      b_e = WTP[most_want_to_less_want_order[tmp]]
    }
    return(b_e)
  }
  
  b_e = b_e_fn(D_, S_, c)
  b_e_loop[[loop]] = b_e
  
  b_e_tilda = mean(unlist(b_e_loop))
  
  ################################################
  ############## Auction Setting  ################
  ################################################
  
  # WC_raw = rweibull(I_*K_, shape = 1, scale = 1)
  WC_raw = WC_loop[,loop]
  if (length(unique(WC_raw)) < 1000){
    WC_raw[duplicated(WC_raw)] = WC_raw[duplicated(WC_raw)] + 0.0000001
  }
  
  WC = matrix(data = WC_raw, nrow = K_, ncol = I_) #each column corresponds to a buyer
  event = matrix(data =  as.integer(rank(WC_raw)), nrow = K_, ncol = I_)
  event = t(event)
  
  WC_est = rexp(K_, rate = abs(Z)/b)
  
  #check 
  for (i in 1:(I_*K_)){
    supplier = which(event == i) %/% I_ + 1 #integerpart + 1
    buyer = which(event == i) %% I_ 
   # print(c(supplier, buyer))
  }
  ################################
  #Buyer's side
  
  buyers_visit_order = sapply(1:I_, function(i) {rank(WC[,i])}) #each column corresponds to a buyer
  buyers_visit_order = t(buyers_visit_order)
  WC = t(WC)
  
  optimal_bid_fn = function(buyers_visit_order, i, S, a){
    order_local = buyers_visit_order[i,]
    Z_tilda_local = sapply(1:K_, function(k){sum(D_) - K_ * mean(S[order_local][1:k])})
    Z_tilda_local = max(Z_tilda_local, 0)
    search_cost_local = Z_tilda_local^2 / (a * S[order_local])
    bid_local = (K_-2)/(K_-1) + search_cost_local  + c[i]
    return(c(bid_local,search_cost_local))
  }
  c(1:K_)
  bid_= sapply(1:I_, function(i){optimal_bid_fn(buyers_visit_order, i , S_, a)})
  bid = t(bid_[c(1:K_),]) #bid
  bid_seq = bid
  bid = t(sapply(1:I_, function(i)bid[i,][buyers_visit_order[i,]]))
  search_cost = t(bid_[-c(1:K_),]) #search cost
  search_cost = t(sapply(1:I_, function(i)search_cost[i,][buyers_visit_order[i,]]))
  #############################
  #supplier
  
  ################################################
  ############## Acceptance Rule  ################
  ################################################
  # #acceptance rule 1
  B_1 = function(b_ik, supplier, est_WC){
    b_k_est = sort(bid[,supplier], decreasing =  T)[10] 
    return( (b_ik >= b_k_est - est_WC) &
              (b_ik >= (b_e_tilda - stc[supplier] - est_WC[supplier])) )
  }
  
  
  # 
  # #acceptance rule 4
  
  
  
  B_4 = function(b_ik, supplier, est_WC){
    return( (b_ik >= (b_e - est_WC[supplier])) & 
              (b_ik >= (b_e_tilda - stc[supplier] - est_WC[supplier])) )
  }
  
  
  
  ##########################################
  ############## Mkt Price  ################
  ##########################################
  #bid_est = rep(NULL, K_)
  bid_accepted_p = list(list(), list(), list(),list(), list(),
                        list(), list(), list(),list(), list()) #list of K_ lists!!!
  bid_accepted_q = bid_accepted_p
  bid_open_close = list()
  
  i = 1
  encounter = 0
  while (i <= (I_*K_) & length(K) > 0){
    #print(i)
    supplier = which(event == i) %/% I_ + 1
    buyer = which(event == i) %% I_ 
   # print(paste("Buyer", buyer, "is SUPPOSEDLY visiting Supplier", supplier))
    if (supplier %in% K & buyer %in% I){
      encounter  = encounter + 1
      q_ik = D[buyer] #bid quantity
      b_ik = bid[buyer, supplier] #bid price
     # print(paste("Buyer", buyer, "is visiting Supplier",supplier))
     # print(paste("Buyer's bid price - quantity pair is (", b_ik, ", ", q_ik, ")"))
     # print(paste("Supplier's Supply is ", S[supplier]))
      
      q_ik_a = 0
      
      
      if(B_4(b_ik, supplier, WC_est)){  #Depends on accpeted rule
        #print("bid accepted") 
        q_ik_a = min(S[supplier], q_ik)
        bid_accepted_p[[supplier]] = append(bid_accepted_p[[supplier]], b_ik) #double [[]]!
        bid_accepted_q[[supplier]] = append(bid_accepted_q[[supplier]], q_ik_a)
        bid_open_close = append(bid_open_close, b_ik)
      }
      else{#print("bid rejected")
      }
      S[supplier] = S[supplier] - q_ik_a
      D[buyer] = D[buyer] - q_ik_a
      
      if(S[supplier] == 0){
        K = K[!K == supplier]  #K = K[-which(K == supplier)]
       # print(paste("Supplier", supplier, "is out of mkt."))
      }
      if(D[buyer] == 0){
        I = I[!I == buyer] #I = I[-which(I == buyer)]
        #print(paste("Buyer", buyer, "is out of mkt."))
      }
      #print(paste("Supplier's Supply after decision is now ", S[supplier]))
      
    }
    else{#print("Pass")
        }
    i = i + 1
  }
  
  #Final Result (bid_accpeted is a Class 3 object)
  #Contains all information
  bid_accepted = list(supplier_1 = NULL, supplier_2 = NULL, supplier_3 = NULL)
  for (i in 1:K_){ #note K is empty set right now!!
    #print(i)
    bid_accepted[[i]] = data.frame(cbind(unlist(bid_accepted_p[[i]]), unlist(bid_accepted_q[[i]])))
  }
  
  #######################################################
  #######################################################
  bid_accepted_price = data.frame(
    price = unlist(
      lapply(1:K_, function(i)unlist(bid_accepted_p[[i]]))
    )
  ) #double list==>double unlist
  #number of buyers at each supplier
  number_buyer_severed = lapply(1:K_, function(i)length(bid_accepted_p[[i]])) 
  # construct a factor indicating supplier matching price column
  bid_accepted_price$supplier = unlist(lapply(1:K_, function(i)rep(i, number_buyer_severed[i])))
  bid_accepted_price$supplier = factor(bid_accepted_price$supplier)     
  
  
  bid_accepted_qunatity = data.frame(
    quantity = unlist(
      lapply(1:K_, function(i)unlist(bid_accepted_q[[i]]))
    )
  ) #double list==>double unlist
  #number of buyers at each supplier
  # construct a factor indicating supplier matching price column
  bid_accepted_qunatity$supplier = unlist(lapply(1:K_, function(i)rep(i, number_buyer_severed[i])))
  bid_accepted_qunatity$supplier = factor(bid_accepted_qunatity$supplier)   
  
  bid_price_loop[[loop]] = bid_accepted_price
  bid_quantity_loop[[loop]] = bid_accepted_qunatity
  bid_open[[loop]] = bid_open_close[[1]]
  bid_close[[loop]] = bid_open_close[[length(bid_open_close)]]
  S_loop[[loop]] = S_ 
  transaction_loop[[loop]] = S_ - S
  number_transaction[[loop]] = sum(unlist(number_buyer_severed))
  number_encounter[[loop]] = encounter
  # D_carriedover = D
  #S_carriedover = S
  unsatisfied_demand[[loop]] = sum(D)
  unsatisfied_ratio[[loop]] = sum(D) / sum(D_)
  total_demand_loop[[loop]] = sum(D_)
  total_supply_loop[[loop]] = sum(S_)
}


#######################################################
######################   GGplot    ####################
#######################################################
price_loop_plot = lapply(1:length(supply_seed), function(i){bid_price_loop[[i]]$price})
number_loop_plot = lapply(1:length(supply_seed), function(i){dim(bid_price_loop[[i]])[1]})
price_loop_plot = data.frame(price = unlist(price_loop_plot))
price_loop_plot$ts = unlist(lapply(1:length(supply_seed), function(i){
  rep(i, number_loop_plot[i])  
}))
price_loop_plot$ts = as.factor(price_loop_plot$ts)


quantity_loop_plot = lapply(1:length(supply_seed), function(i){bid_quantity_loop[[i]]$quantity})

quantity_loop_plot = data.frame(quantity= unlist(quantity_loop_plot))
quantity_loop_plot$ts = unlist(lapply(1:length(supply_seed), function(i){
  rep(i, number_loop_plot[i])  
}))
quantity_loop_plot$ts = as.factor(quantity_loop_plot$ts)


#Trading volume-------
trading_volume= lapply(1:length(supply_seed), function(i){sum(transaction_loop[[i]])})
trading_volume = unlist(trading_volume)



#price_mean = sapply(1:length(supply_seed), function(i){
#  weighted.mean(price_loop_plot[price_loop_plot$ts==i, 1], 
#                quantity_loop_plot[quantity_loop_plot$ts==i, 1])
#})

price_max = 
  price_loop_plot%>%
  group_by(ts)%>%
  summarise_each(., funs(max))

price_min = 
  price_loop_plot%>%
  group_by(ts)%>%
  summarise_each(., funs(min))

price_open = unlist(bid_open)
price_close = unlist(bid_close)
price_equil = unlist(b_e_loop)


number_encounter = unlist(number_encounter)
number_transaction = unlist(number_transaction)
unsatisfied_demand = unlist(unsatisfied_demand)
unsatisfied_ratio = unlist(unsatisfied_ratio) * 100
excess_demand = unlist(total_demand_loop) - unlist(total_supply_loop)
jump_metric1 = (price_mean - price_equil)/price_equil * 100
jump_metric2 = (price_min[,2] - price_equil)/price_equil * 100
jump_metric3 = (price_close - price_equil)/price_equil * 100

demand_gen = unlist(D_loop)
excess_demand_gen = unlist(Z_loop)
excess_demand_actual = unlist(Z_3_loop)

other_df = data.frame(cbind(price_mean, price_open, price_close, price_equil,
                            number_encounter,
                            number_transaction,
                            unsatisfied_demand,
                            unsatisfied_ratio,
                            jump_metric1,
                            jump_metric2,
                            jump_metric3,
                            trading_volume,
                            excess_demand,
                            demand_gen,
                            excess_demand_gen,
                            excess_demand_actual))
candle_chart_df = full_join(price_min, price_max, by = "ts")
candle_chart_df%<>%
  bind_cols(., other_df)

candle_chart_df%<>%
  mutate(., price_upper = pmax(price_open, price_close))%>%
  mutate(., price_lower = pmin(price_open, price_close))%>%
  mutate(., type = as.numeric((price_open == price_lower)))



arima_fit <- forecast::auto.arima(x = price_close, allowdrift = FALSE, ic = "bic")
pars <- coefs2poly(arima_fit)
resid <- residuals(arima_fit)
oo<-locate.outliers(resid,pars)
print(dim(oo)[1])


arima_fit <- forecast::auto.arima(x = price_equil, allowdrift = FALSE, ic = "bic")
pars <- coefs2poly(arima_fit)
resid <- residuals(arima_fit)
oo1<-locate.outliers(resid,pars)
print(dim(oo1)[1])

realized_vol = sqrt(sum(diff(log(price_close))^2)); realized_vol
max(jump_metric3)
mean(trading_volume)
max(trading_volume)
mean(unlist(HHI_loop))
