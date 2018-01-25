
candle_chart_df_0$price_equil;candle_chart_df_1$price_equil
for_comp = cbind(candle_chart_df_0$ts,
                 candle_chart_df_0$price_close, 
                 candle_chart_df_1$price_close,
                 candle_chart_df_0$price_mean, 
                 candle_chart_df_1$price_mean,
                 candle_chart_df_0$jump_metric1,
                 candle_chart_df_1$jump_metric1,
                 candle_chart_df_0$jump_metric3,
                 candle_chart_df_1$jump_metric3)
for_comp = data.frame(for_comp)
colnames(for_comp) = c("ts", "TS1_close", "TS3_close",
                       "TS1_avg", "TS3_avg", 
                       "TS1_jump1", "TS3_jump1",
                       "TS1_jump3", "TS3_jump3")

#Comparion of close price-----
ggplot(for_comp)+
  geom_point(aes(x = ts, y = TS1_close, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = TS1_close, color = "1"))+
  geom_point(aes(x = ts, y = TS3_avg, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = TS3_avg, color = "2"),linetype = "dashed")+
  scale_color_manual(name = "Legend",
                     values = c("red", "blue"),
                     labels = c("Close Prices when supply is about equally distributed across 10 suppliers",
                                "Close Prices when about half of the total supply is hold by 3 largest suppliers")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10),
                     expand = c(0,0))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Close Price")+
  #ggtitle("Close Prices in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom",
        legend.direction = "vertical")
ggsave("Comp_Effect_Dist_Supply_Close_Price.pdf", width = 8, height = 6, units = "in")
#Comparion of Avg price-----
ggplot(for_comp)+
  geom_point(aes(x = ts, y = TS1_avg, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = TS1_avg, color = "1"))+
  geom_point(aes(x = ts, y = TS3_avg, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = TS3_avg, color = "2"), linetype = "dashed")+
  scale_color_manual(name = "Legend",
                     values = c("red", "blue"),
                     labels = c("Weighted Average Prices when supply is about equally distributed across 10 suppliers",
                                "Weighted Average Prices when about half of the total supply is hold by 3 largest suppliers")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10),
                     expand = c(0, 0))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Weighted Average Price")+
  #ggtitle("Average Price in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom",
        legend.direction = "vertical")
ggsave("Comp_Effect_Dist_Supply_Avg_Price.pdf", width = 8, height = 6, units = "in")


#Comparion of Jump size using Jump metric 1-----
ggplot(for_comp)+
  geom_point(aes(x = ts, y = TS1_jump1, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = TS1_jump1, color = "1"))+
  geom_point(aes(x = ts, y = TS3_jump1, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = TS3_jump1, color = "2"), linetype = "dashed")+
  scale_color_manual(name = "Legend",
                     values = c("red", "blue"),
                     labels = c("Jump Size when supply is about equally distributed across 10 suppliers",
                                "Jumps Size when about half of the total supply is hold by 3 largest suppliers")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10),
                     expand = c(0,0))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Close Price")+
 # ggtitle("Jump Size (Jump Metric 1) in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom",
        legend.direction = "vertical")
ggsave("Comp_Effect_Dist_Supply_Jump1.pdf", width = 8, height = 6, units = "in")

#Comparion of Jump size using Jump metric 3-----
ggplot(for_comp)+
  geom_point(aes(x = ts, y = TS1_jump3, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = TS1_jump3, color = "1"))+
  geom_point(aes(x = ts, y = TS3_jump3, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = TS3_jump3, color = "2"), linetype = "dashed")+
  scale_color_manual(name = "Legend",
                     values = c("red",  "blue"),
                     labels = c("Jump Size when supply is about equally distributed across 10 suppliers",
                                "Jumps Size when about half of the total supply is hold by 3 largest suppliers")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10),
                     expand = c(0,0))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Close Price")+
 # ggtitle("Jump Size (Jump Metric 3) in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom",
        legend.direction = "vertical")
ggsave("Comp_Effect_Dist_Supply_Jump3.pdf", width = 8, height = 6, units = "in")


#Example of disrtibution of supply across suppliers----



S_comp = data.frame(cbind(S1,S2))
S_comp = gather(S_comp, "variable", "value", 1:2)
data.frame(S1)
ggplot(S_comp, aes(x = value, group = variable,  fill = variable))+
  geom_histogram(alpha = 0.6, bins = 40)+
  #scale_y_discrete(breaks = c(0, 1, 2),
  #                 labels = c(0, 1, 2))+
  #scale_x_continuous(breaks =
   # expand = c(0,0))+
  xlab("Supply Quantity")+
  ylab("Number of Suppliers")+
  scale_fill_manual(name = "Legend",
                    values = c("red", "blue"),
                    labels  = c("Supply about equally distributed across 10 suppliers",
                                "About half of the total supply hold by 3 largest suppliers")
                    )+
  #scale_fill_brewer(palette = "BuGn")+
  #scale_fill_grey(start = 0.1, end = .6)+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom",
        legend.direction = "vertical")

ggsave("Comp_Effect_Dist_Supply_Ex_Supply.pdf", width = 8, height = 6, units = "in")
