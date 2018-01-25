#------------------------------------------------
#Simulation
#------------------------------------------------
#This is the simulation code for chapter 3
library(ggplot2)
library(dplyr)
library(magrittr)

options(digit = 4)
set.seed(1986)

#------------------------------------------------
#Market Initialization
#------------------------------------------------

I = 100 #number of buyers
K = 10 #number of suppliers



#------------------------------------------------
#Buyer Side
#------------------------------------------------
#Each column is a buyer.
WC_raw = rweibull(I*K, shape = 1, scale = 1)
WC = data.frame(matrix(data = WC_raw, nrow = K, ncol = I)) #each column corresponds to a buyer




WC_timeline = cbind(as.numeric(WC_raw), as.vector(sapply(1:I, function(i) rep(i, K))), rep(1:K, I))
colnames(WC_timeline)= c("Waiting_Cost", "Buyer", "Seller")
WC_timeline = data.frame(WC_timeline)

encounter = paste("E", WC_timeline[,2], ",", WC_timeline[,3],  sep = "")

WC_timeline = cbind(WC_timeline, encounter)
WC_timeline = WC_timeline[order(WC_timeline[,1]),]
WC_timeline = cbind(WC_timeline, rep(0.5, 1000))

colnames(WC_timeline)= c("Waiting_Cost", "Buyer", "Seller", "Encounter", "disloc")
WC_timeline$Waiting_Cost = as.numeric(WC_timeline$Waiting_Cost)


sample_ = c(995, 996, 998, 999, 1000)
sample_ = unique(sample_)
WC_timeline_plot = WC_timeline[sample_,]

WC_timeline_supplier = WC_timeline[WC_timeline$Seller == 9,][c(1, 30, 60, 90, 99, 100),]
WC_timeline_buyer = WC_timeline[WC_timeline$Buyer == 6,][c(1, 5, 8, 9, 10),]

WC_timeline_plot%<>%
  bind_rows(., WC_timeline_buyer)%>%
  bind_rows(., WC_timeline_supplier)

WC_timeline_buyer$label = paste("Supplier", WC_timeline_buyer$Seller)
WC_timeline_supplier$label = paste("Buyer",WC_timeline_supplier$Buyer)

WC_timeline_plot = WC_timeline_plot[-c(2, 4, 16),]

#Plot
ggplot()+
  geom_segment(aes(x = -0.1,y = 0,xend = max(WC_raw)+0.5,yend = 0),data=WC_timeline_plot,
               arrow = arrow(length = unit(x = 0.2,units = 'cm'),type = 'closed'))+
  
  geom_segment(aes(x = Waiting_Cost, y = disloc, xend = Waiting_Cost), 
               data = WC_timeline_plot,  yend= 0,  color = "#0033CC", show.legend = TRUE)+
  
  geom_text(aes(x = Waiting_Cost,y = disloc,label = Encounter),
            data=WC_timeline_plot, hjust = 0,vjust = -1, parse = FALSE, size = 3,angle = 35, color = "#0033CC")+
  
  geom_text(aes(x = 3.3, y = 0.7, label = "Sequence of Encounters"), size = 3.5, color = "#0033CC")+
  
  geom_point(aes(x = Waiting_Cost,y = disloc),data=WC_timeline_plot, color = "#0033CC") +
  
  ################################ 
  geom_segment(aes(x = Waiting_Cost, y = rep(-0.4, 5), xend = Waiting_Cost), 
               data = WC_timeline_buyer, yend= 0,  color = "#FF0000", linetype = "dashed")+
  
  geom_text(aes(x = Waiting_Cost,y = rep(-0.4, 5),label = label),
            data=WC_timeline_buyer,
            hjust = -.1,vjust = -.1,parse = FALSE, size = 3.5,angle = 35, color = "#FF0000")+
  
  geom_point(aes(x = Waiting_Cost,y = rep(-0.4, 5)),data=WC_timeline_buyer, color = "#FF0000") +
  
  geom_text(aes(x = 5.45, y = -0.3, label = "Buyer 6's Visiting Sequence"), size = 3.5, color = "#FF0000")+
  ###################################  
  geom_segment(aes(x = Waiting_Cost, y = rep(-0.8, 6), xend = Waiting_Cost), 
               data = WC_timeline_supplier, yend= 0, color = "#99CC00", linetype = "dashed")+
  
  geom_text(aes(x = Waiting_Cost,y = rep(-0.8, 6),label = label), 
            data = WC_timeline_supplier,
            hjust = -.1,vjust = -.1,parse = FALSE, size = 3.5,angle = 35,  color = "#99CC00")+ 
  
  geom_text(aes(x = 5.7, y = -0.55, label = "Supplier 9's Customer Arrival Sequence"), size = 3.5, color = "#99CC00")+
  
  geom_point(aes(x = Waiting_Cost,y = rep(-0.8, 6)),data=WC_timeline_supplier, color = "#99CC00") +
  
  xlab("Time since the First Encounter")+
  
  ylab("Events")+
  
  scale_x_continuous(expand = c(0, 0.1))+
  
  theme_bw() 


ggsave("Encounter_plot.pdf", width = 11, height = 5, units = "in")




