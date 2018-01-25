closeAllConnections()
rm(list=ls())
gc()
library(ggplot2)
library(knitr)
library(dplyr)
library(magrittr)
library(evd)
set.seed(19888)
#set.seed(1988)
#set.seed(1)


# supply_seed = c(500, 490, 505, 520, 202, 301, 380, 450, 500, 510)
#random_gumble = rgumbel(100, loc = 10, scale = 10)
random_gumble = rgumbel(100, loc = 10, scale = 10)
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

Z_loop = list()
Z_3_loop = list()
D_loop = list()

K_ = 10 #number of suppliers
I_ = 100
WC_loop = rweibull(I_*K_*100, shape = 1, scale = 1)
WC_loop = matrix(data = WC_loop, nrow = K_*I_, ncol = 100) #each column corresponds to a particualr loop
stc = rnorm(K_, 10, 5)


for (loop in 1:length(supply_seed)){
  local_supply_seed = supply_seed[loop]
  
  
  a = 100 #parameter in search cost sc= a * EZ^2 * s_k
  b = 200
  K = 1:K_
  I = 1:I_
  
  
  D_ = rnorm(I_, 10, 3) + 0.2
  S_ = rnorm(K_, supply_seed[loop], 1)
  
  Z_loop[[loop]] = sum(D_) - sum(S_)
  D_loop[[loop]] = sum(D_)
  
  #D_ = D_ + D_carriedover
  c = rnorm(I_, 100,  5)
  
  #S_ = S_ + S_carriedover
  D = D_
  S = S_
  Z = sum(D) - sum(S)
  Z_3_loop[[loop]] = Z
  
  ################################################
  ############## Competitive Mkt  ################
  ################################################
  # # SC_competitive = sapply(1:I_, function(i){mean(Z ^ 2 /(a * S_))}) #WTP = marginal sc
  # # WTP = SC_competitive + c
  # WTP = c
  # most_want_to_less_want_order = order( - WTP)
  # #check 
  # #SC_competitive[most_want_to_less_want_order]
  # 
  # if (sum(D) < sum(S)){
  #   b_e = min(c)
  # }
  # else{
  #     tmp = 0
  #     d_tmp = 0
  #     while (d_tmp - sum(S) < 0){
  #       tmp = tmp + 1 
  #       print(tmp)
  #       d_tmp = d_tmp + D[most_want_to_less_want_order[tmp]]
  #     }
  #     b_e = WTP[most_want_to_less_want_order[tmp]]
  #   }
  
  
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
        print(tmp)
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
  
  WC = matrix(data = WC_raw, nrow = K_, ncol = I_) #each column corresponds to a buyer
  event = matrix(data = rank(WC_raw), nrow = K_, ncol = I_)
  event = t(event)
  
  WC_est = rexp(K_, rate = abs(Z)/b)
  
  #check 
  for (i in 1:(I_*K_)){
    supplier = which(event == i) %/% I_ + 1 #integerpart + 1
    buyer = which(event == i) %% I_ 
    print(c(supplier, buyer))
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
    print(i)
    supplier = which(event == i) %/% I_ + 1
    buyer = which(event == i) %% I_ 
    print(paste("Buyer", buyer, "is SUPPOSEDLY visiting Supplier", supplier))
    if (supplier %in% K & buyer %in% I){
      encounter  = encounter + 1
      q_ik = D[buyer] #bid quantity
      b_ik = bid[buyer, supplier] #bid price
      print(paste("Buyer", buyer, "is visiting Supplier",supplier))
      print(paste("Buyer's bid price - quantity pair is (", b_ik, ", ", q_ik, ")"))
      print(paste("Supplier's Supply is ", S[supplier]))
      
      q_ik_a = 0
      
      
      if(B_4(b_ik, supplier, WC_est)){  #Depends on accpeted rule
        print("bid accepted") 
        q_ik_a = min(S[supplier], q_ik)
        bid_accepted_p[[supplier]] = append(bid_accepted_p[[supplier]], b_ik) #double [[]]!
        bid_accepted_q[[supplier]] = append(bid_accepted_q[[supplier]], q_ik_a)
        bid_open_close = append(bid_open_close, b_ik)
      }
      else{print("bid rejected")
      }
      S[supplier] = S[supplier] - q_ik_a
      D[buyer] = D[buyer] - q_ik_a
      
      if(S[supplier] == 0){
        K = K[!K == supplier]  #K = K[-which(K == supplier)]
        print(paste("Supplier", supplier, "is out of mkt."))
      }
      if(D[buyer] == 0){
        I = I[!I == buyer] #I = I[-which(I == buyer)]
        print(paste("Buyer", buyer, "is out of mkt."))
      }
      print(paste("Supplier's Supply after decision is now ", S[supplier]))
      
    }
    else{print("Pass")}
    i = i + 1
  }
  
  #Final Result (bid_accpeted is a Class 3 object)
  #Contains all information
  bid_accepted = list(supplier_1 = NULL, supplier_2 = NULL, supplier_3 = NULL)
  for (i in 1:K_){ #note K is empty set right now!!
    print(i)
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



price_mean = sapply(1:length(supply_seed), function(i){
  weighted.mean(price_loop_plot[price_loop_plot$ts==i, 1], 
                quantity_loop_plot[quantity_loop_plot$ts==i, 1])
})

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

black.plain.angle30.size1.text <- element_text(angle = 90)

#ggplot-----
ggplot(candle_chart_df, aes(x = ts))+
  geom_boxplot(aes(
    fill = factor(type),
    ymin = price.x,
    lower = price_lower,
    middle = price_mean,
    upper = price_upper,
    ymax = price.y,
    width = trading_volume/1200), 
    fatten = 0.7,
    #
    stat = "identity",
    size = 0.4)+
  # geom_point(aes(x = ts, y = price_mean), color = "#FF9933")+
  # geom_line(aes(x = as.numeric(ts), y = price_mean), 
  #           linetype = 2,
  #           size = .3,
  #           color = "#FF9933")+
  geom_point(aes(x = ts, y = price_equil, color = "1"))+
  geom_line(aes(x = as.numeric(ts), y = price_equil,  color = "1"), 
            linetype = 2,
            size = .3)+
  # geom_point(aes(x = ts, y = number_encounter, color = "2")) +
  # geom_line(aes(x = as.numeric(ts), y = number_encounter, color = "2"),
  #           linetype = 2,
  #           size = .3)+
  # geom_point(aes(x = ts, y = number_transaction, color = "3")) +
  # geom_line(aes(x = as.numeric(ts), y = number_transaction, color = "3"),
  #           linetype = 2,
  #           size = .3)+
  # geom_point(aes(x = ts, y = 100 * unsatisfied_ratio, color = "4")) +
  # geom_line(aes(x = as.numeric(ts), y = 100 * unsatisfied_ratio, color = "4"),
  #           linetype = 2,
#           size = .3)+
# geom_point(aes(x = ts, y = jump_metric, color = "5")) +
# geom_line(aes(x = as.numeric(ts), y = jump_metric, color = "5"),
#           linetype = 2,
#           size = .3)+
xlab("Trading Session")+
  ylab("Transaction Prices")+
  # ggtitle("Transaction Prices in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  scale_color_manual(name='Legend', 
                     labels = c("Equilibrium Price", 
                                "# Encounter",
                                "# Transaction", 
                                "Unfulfilled Ratio"),
                     values = c("#0066FF", "blue", 
                                "#FF0000","green"))+
  scale_fill_manual(name='Session Type', 
                    labels = c("rise", "fall"),
                    values=c("#17FF00", "red"))+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10), 
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  scale_y_continuous(labels = c("100", "120", "140", "160", "180"),
                     breaks = c(100, 120, 140, 160, 180))+
  theme(axis.text.x = black.plain.angle30.size1.text, 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position	= "bottom",
        legend.box = "horizontal"
  )

ggsave("TS1_HighWC_Candle.pdf", width = 11.69, height = 8.27, units = "in")

# 

ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = number_encounter, color = "# Encounters")) +
  geom_line(aes(x = as.numeric(ts), y = number_encounter, color = "# Encounters"))+
  geom_point(aes(x = ts, y = number_transaction, color = "# Transactions")) +
  geom_line(aes(x = as.numeric(ts), y = number_transaction, color = "# Transactions"),linetype = "dashed")+
  xlab("Trading Session")+
  ylab("Number")+
  #  ggtitle("Number of Encounters across Trading Sessions (10 Suppliers & 100 Buyers)")+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  scale_color_manual(name = "Legend",
                     labels = c("# Encounters", "# Transactions"),
                     values = c("red", "blue"))+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position	= "bottom"
  )

ggsave("TS1_HighWC_Encounter.pdf", width = 8, height = 6, units = "in")

ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = jump_metric1, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = jump_metric1, color = "1"))+
  geom_point(aes(x = ts, y = jump_metric2, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = jump_metric2, color = "2"))+
  geom_point(aes(x = ts, y = jump_metric3, color = "3")) +
  geom_line(aes(x = as.numeric(ts), y = jump_metric3, color = "3"))+
  scale_color_manual(name = "Legend",
                     values = c("red", "green", "blue"),
                     labels = c("Weighted Average Bid Price",
                                "Minimal Bid Price",
                                "Closing Bid Price"
                     ))+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Jump Size (%)")+
  # ggtitle("Jump Size across Trading Sessions (using Three Metrics)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom")

ggsave("TS1_HighWC_Jump_All.pdf", width = 8, height = 6, units = "in")

ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = jump_metric1), color = "red") +
  geom_line(aes(x = as.numeric(ts), y = jump_metric1), color = 'red')+
  xlab("Trading Session")+
  ylab("Jump Size (%)")+
  #  ggtitle("Jump Size across Trading Sessions (Weighted Average Bid Price)")+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                  breaks = c(20, 40, 60))+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1)
  )

ggsave("TS1_HighWC_Jump_1.pdf", width = 8, height = 6, units = "in")


ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = jump_metric2), color = "red") +
  geom_line(aes(x = as.numeric(ts), y = jump_metric2), color = 'red')+
  xlab("Trading Session")+
  ylab("Jump Size (%)")+
  # ggtitle("Jump Size across Trading Sessions (Minimal Bid Price)")+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1)
  )

ggsave("TS1_HighWC_Jump_2.pdf", width = 8, height = 6, units = "in")

ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = jump_metric3), color = "red") +
  geom_line(aes(x = as.numeric(ts), y = jump_metric3), color = 'red')+
  xlab("Trading Session")+
  ylab("Jump Size (%)")+
  #  ggtitle("Jump Size across Trading Sessions (Closing Bid Price)")+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1)
  )

ggsave("TS1_HighWC_Jump_3.pdf", width = 8, height = 6, units = "in")

ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = unsatisfied_ratio), color = "red") +
  geom_line(aes(x = as.numeric(ts), y = unsatisfied_ratio), color = 'red')+
  xlab("Trading Session")+
  ylab("Unsatisfied Demand Ratio (%)")+
  # ggtitle("Unsatisfied Demand Ratio across Trading Sessions (10 Suppliers & 100 Buyers)")+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  #scale_y_continuous(labels = c("50%", "60%", "70%", "80%"),
  #                     breaks = c(50, 60, 70, 80))+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1)
  )

ggsave("TS1_HighWC_Unsatisfied_Demand.pdf", width = 8, height = 6, units = "in")


ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = excess_demand), color = "red") +
  geom_line(aes(x = as.numeric(ts), y = excess_demand), color = 'red')+
  xlab("Trading Session")+
  ylab("Excess Demand")+
  # ggtitle("Excess Demand across Trading Sessions (10 Suppliers & 100 Buyers)")+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1)
  )

ggsave("TS1_HighWC_Excess_Demand.pdf", width = 8, height = 6, units = "in")


ggplot(candle_chart_df)+
  geom_point(aes(x = ts, y = excess_demand_gen, color = "# Encounters")) +
  geom_line(aes(x = as.numeric(ts), y = excess_demand_gen, color = "# Encounters"))+
  geom_point(aes(x = ts, y = excess_demand_actual, color = "# Transactions")) +
  geom_line(aes(x = as.numeric(ts), y = excess_demand_actual, color = "# Transactions"),linetype = "dashed")+
  xlab("Trading Session")+
  ylab("Number")+
  #  ggtitle("Excess Demand across Trading Sessions (10 Suppliers & 100 Buyers)")+
  scale_x_discrete(labels= seq(0, length(supply_seed) - 10, 10),
                   breaks = seq(0, length(supply_seed) - 10, 10))+
  scale_color_manual(name = "Legend",
                     labels = c("Excess Denamd Gen", "Excess Demand Actual"),
                     values = c("red", "blue"))+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position	= "bottom"
  )

ggsave("TS1_HighWC_Excess_Demand_All.pdf", width = 8, height = 6, units = "in")

# values=c("#0066FF", "#0033CC", "#33FFFF", "#17FF00", "#99CC00", "#FFCC66", "#FF9933", "#FF99FF", "#9933FF","#FF0000","#560000")
