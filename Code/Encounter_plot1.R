#------------------------------------------------
#Simulation
#------------------------------------------------
#This is the simulation code for chapter 3
library(ggplot2)
library(dplyr)
library(magrittr)
library(cowplot)


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
WC_timeline = cbind(WC_timeline, rep(0.3, 1000))

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
encounter_plot = 
ggplot()+
  geom_segment(aes(x = -0.1,y = 0,xend = max(WC_raw)+0.5,yend = 0),data=WC_timeline_plot,
               arrow = arrow(length = unit(x = 0.2,units = 'cm'),type = 'closed'))+
  
  geom_segment(aes(x = Waiting_Cost, y = disloc, xend = Waiting_Cost), 
               data = WC_timeline_plot,  yend= 0,  color = "#0033CC", show.legend = TRUE)+
  
  geom_text(aes(x = Waiting_Cost,y = disloc,label = Encounter),
            data=WC_timeline_plot, hjust = 0,vjust = -1, parse = FALSE, size = 3,angle = 35, color = "#0033CC")+
  
  geom_text(aes(x = 3.3, y = 0.5, label = "Sequence of All Encounters"), size = 3.5, color = "#0033CC")+
  
  geom_point(aes(x = Waiting_Cost,y = disloc),data=WC_timeline_plot, color = "#0033CC") +
  
 # xlab("Time since the First Encounter")+
  
  ylab("Events")+
  
  scale_x_continuous(expand = c(0, 0.1))+
  
  scale_y_continuous(expand = c(0.01, 0.05), breaks = NULL)+
  
  
  theme_bw()+
  theme(axis.title.x = element_blank())
################################ 

Buyer_plot = 
  ggplot()+
  
  geom_segment(aes(x = -0.1,y = 0,xend = max(WC_raw)+0.5,yend = 0),data=WC_timeline_plot,
               arrow = arrow(length = unit(x = 0.2,units = 'cm'),type = 'closed'))+
  
  geom_segment(aes(x = Waiting_Cost, y = rep(0.3, 5), xend = Waiting_Cost), 
               data = WC_timeline_buyer, yend= 0,  color = "#FF0000")+
  
  geom_text(aes(x = Waiting_Cost,y = rep(0.3, 5),label = label),
            data=WC_timeline_buyer,
            hjust = -.1,vjust = -.1,parse = FALSE, size = 3.5,angle = 35, color = "#FF0000")+
  
  geom_point(aes(x = Waiting_Cost,y = rep(0.3, 5)),data=WC_timeline_buyer, color = "#FF0000") +
  
  geom_text(aes(x = 3.3, y = 0.5, label = "Suppliers encountered by Buyer 6"), size = 3.5, color = "#FF0000")+
  
  
  #xlab("Time since the First Encounter")+
  
  ylab("Events")+
  
  scale_x_continuous(expand = c(0, 0.1))+
  
  
  scale_y_continuous(expand = c(0.01, 0.05), breaks = NULL)+
  
  theme_bw()+
  theme(axis.title.x = element_blank())

  ###################################  


Supplier_plot = 
  ggplot()+
  
  geom_segment(aes(x = -0.1,y = 0,xend = max(WC_raw)+0.5,yend = 0),data=WC_timeline_plot,
               arrow = arrow(length = unit(x = 0.2,units = 'cm'),type = 'closed'))+
  
  geom_segment(aes(x = Waiting_Cost, y =  c(rep(0.3, 3), 0.25, 0.25, 0.3), xend = Waiting_Cost), 
               data = WC_timeline_supplier, yend= 0, color = "#99CC00")+
  
  geom_text(aes(x = Waiting_Cost,y =  c(rep(0.3, 3), 0.25, 0.25, 0.3),label = label), 
            data = WC_timeline_supplier,
            hjust = -.1,vjust = -.1,parse = FALSE, size = 3.5,angle = 35,  color = "#99CC00")+ 
  
  geom_text(aes(x = 3.3, y = 0.5, label = "Buyers that show up at supplier 9"), size = 3.5, color = "#99CC00")+
  
  geom_point(aes(x = Waiting_Cost,y = c(rep(0.3, 3), 0.25, 0.25, 0.3)),data=WC_timeline_supplier, color = "#99CC00") +
  
  xlab("Time since the First Encounter")+
  
  ylab("Events")+
  
  scale_x_continuous(expand = c(0, 0.1))+
  
  
  scale_y_continuous(expand = c(0.01, 0.05), breaks = NULL)+
  
  theme_bw() 


#ggsave("Encounter_plot.pdf", width = 11, height = 5, units = "in")


encounter_plot
Buyer_plot
Supplier_plot
panel_border() # and a border around each panel
# plot.mpt and plot.diamonds were defined earlier
ggdraw() +
  draw_plot(encounter_plot, 0, 0.68, 1, 0.32) +
  draw_plot(Buyer_plot, 0, 0.36, 1, 0.32) +
  draw_plot(Supplier_plot, 0, 0, 1, 0.36) +
  draw_plot_label(c("A", "B", "C"), c(0, 0, 0), c(1, 0.68, 0.36), size = 15)

