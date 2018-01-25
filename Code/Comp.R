
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
                     labels = c("Close Prices in type 1 Trading Session",
                                "Close Prices in type 3 Trading Session")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Close Price")+
  ggtitle("Close Prices in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom")

#Comparion of Avg price-----
ggplot(for_comp)+
  geom_point(aes(x = ts, y = TS1_avg, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = TS1_avg, color = "1"))+
  geom_point(aes(x = ts, y = TS3_avg, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = TS3_avg, color = "2"), linetype = "dashed")+
  scale_color_manual(name = "Legend",
                     values = c("red", "blue"),
                     labels = c("Average Prices in type 1 Trading Session",
                                "Average Prices in type 3 Trading Session")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Close Price")+
  ggtitle("Average Price in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom")



#Comparion of Jump size using Jump metric 1-----
ggplot(for_comp)+
  geom_point(aes(x = ts, y = TS1_jump1, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = TS1_jump1, color = "1"))+
  geom_point(aes(x = ts, y = TS3_jump1, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = TS3_jump1, color = "2"), linetype = "dashed")+
  scale_color_manual(name = "Legend",
                     values = c("red", "blue"),
                     labels = c("Jump Metric 1 in type 1 Trading Session",
                                "Jump Metric 1 in type 3 Trading Session")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Close Price")+
  ggtitle("Jump Size (Jump Metric 1) in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom")


#Comparion of Jump size using Jump metric 3-----
ggplot(for_comp)+
  geom_point(aes(x = ts, y = TS1_jump3, color = "1")) +
  geom_line(aes(x = as.numeric(ts), y = TS1_jump3, color = "1"))+
  geom_point(aes(x = ts, y = TS3_jump3, color = "2")) +
  geom_line(aes(x = as.numeric(ts), y = TS3_jump3, color = "2"), linetype = "dashed")+
  scale_color_manual(name = "Legend",
                     values = c("red",  "blue"),
                     labels = c("Jump Metric 3 in type 1 Trading Session",
                                "Jump Metric 3 in type 3 Trading Session")
  )+
  scale_x_continuous(labels= seq(0, length(supply_seed) - 10, 10),
                     breaks = seq(0, length(supply_seed) - 10, 10))+
  # scale_y_continuous(labels = c("20%", "40%", "60%"),
  #                    breaks = c(20, 40, 60))+
  xlab("Trading Session")+
  ylab("Close Price")+
  ggtitle("Jump Size (Jump Metric 3) in Type 1 Trading Session vs in Type 3 Trading Sessions (10 Suppliers & 100 Buyers)")+
  theme(axis.text.x = black.plain.angle30.size1.text,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position = "bottom")

