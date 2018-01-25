library(tseries)
library(timeSeries)
library(ggplot2)
library(xlsx)
require(tidyr)
require(dplyr)

###############Read Data###############
print("read data")
data=read.csv(file="data.csv", header=T)
outliers = read.xlsx(file = "JUMPS.xlsx", sheetName = "Sheet1")
data_ts=ts(data[,2:8],start=c(2000,1),frequency=52)
colnames(data_ts)

#################Clean Data################
data[,1] = as.Date(data[,1], format = "%d-%b-%y")
colnames(data)[2:8] = c("Italy Corn",
                        "Italy Wheat",
                        "Italy Soybeans",
                        "US Corn",
                        "US Wheat",
                        "US Soybeans",
                        "Brent Blend")


###############Print Data###############
#plot(data,main = "US and EU commodity prices")
plot(data_ts, plot.type="single", col = 1:ncol(data_ts),main = "US and EU commodity prices",xlab="Date",ylab="Prices (Euro/ton)")
legend("bottomleft", colnames(data_ts), col=1:ncol(data_ts), lty=1, cex=.65)
dev.off()


#############ggplot2######################
base_date_index = which(data$Date == "2008-01-04")
data_plot = 
  data%>%
  select(., 2:8)%>%
  mutate_each(.,funs(./.[base_date_index]))
data_plot$Date = data$Date
data_plot = data_plot[,c(8, 1:7)]
data_plot_2= gather(data_plot, "variable", "value", 2:8)

data_plot_1 = gather(data, "variable", "value", 2:8)

black.plain.angle30.size1.text <- element_text(angle =90)


ggplot(data_plot_1, aes(x = Date, y = value, color= variable, group = variable))+
#  geom_point(size = .3)+
  geom_line(size = .5)+
  xlab('Date (Weekly)') +
  ylab('Price') +
  ggtitle('US and Italy Soft Commodity Prices')+
  scale_x_date(breaks = 
                 data$Date[c(1, 1+52, 1+52*2, 1 + 52*3,
                             1 + 52*4, 1+52*4+53, 1 +52*5+53,
                             (1 + 52*6 + 53), (1 + 52*7 + 53),
                             (1 + 52*8 + 53),(1 + 52*9 + 53),
                             (1 + 52*10 + 53))],
               limits = data$Date[c(1, (1+ 52*10 +53))],
        #       labels = c(2000: 2011),
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_color_manual(name = "Price Series",
                    values=c("#0066FF",  "#33FFFF", 
                             "#99CC00","#FF0000", "#FFCC66", 
                             "#FF9933", "#FF99FF", "#9933FF",
                             "#99CC00","#560000"))+
  theme(axis.text.x = black.plain.angle30.size1.text, 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey", size = 0.8),
        panel.grid.major = element_line(colour = "grey", size = 0.1),
        legend.position	= "bottom",
        legend.box = "horizontal"
  )  


data$Date[c(1, 1+52, 1+52*2, 1 + 52*3,
            1 + 52*4, 1+52*4+53, 1 +52*5+53,
            (1 + 52*6 + 53), (1 + 52*7 + 53),
            (1 + 52*8 + 53),(1 + 52*9 + 53),
            (1 + 52*10 + 53))]

data$Date[1]
data$Date[1 + 52]; data$Date[1 + 52 + 52]
data$Date[(1 + 52*3)];data$Date[(1 + 52*4)]; 
data$Date[(1 + 52*4 + 53)]; data$Date[(1 + 52*5 + 53)];
data$Date[(1 + 52*6 + 53)];data$Date[(1 + 52*7 + 53)];
data$Date[(1 + 52*8 + 53)];data$Date[(1 + 52*9 + 53)];
data$Date[(1 + 52*10 + 53)];
# summary(data)
# ggplot()+
# geom_line(data = data, aes(x = data[,1], y = data[,2], colour = 'Italy Corn'))+ 
# geom_line(data = data, aes(x = data[,1], y = data[,3], colour = 'Italy Wheat'))+ 
# geom_line(data = data, aes(x = data[,1], y = data[,4], colour = 'Italy Soybeans'))+ 
# geom_line(data = data, aes(x = data[,1], y = data[,5], colour = 'US Corn'))+
# geom_line(data = data, aes(x = data[,1], y = data[,6], colour = 'US Wheat'))+
# geom_line(data = data, aes(x = data[,1], y = data[,7], colour = 'US Soybeans'))+
# geom_line(data = data, aes(x = data[,1], y = data[,8], colour = 'Brent Blend'))+
# xlab('Date') +
# ylab('Price (Euro/ton)') +
# ggtitle('US and Italy Soft Commodity Prices')+
# labs(colour = "Legend")


##############Plot Jumps in the first series#################

data1 =  data[outliers[,1][!is.na(outliers[,1])]-1,]

ggplot(data = data, aes(x = data[,1], y = data[,2], colour = 'IT corn'))+
geom_line(colour = 'Blue')+
geom_point(data = data1, aes(x = data1[,1], y = data1[,2], colour = 'Jumps'), size = 2, shape = 3, stroke = 2, fill = "Black")+
xlab('Date') +
ylab('Price (Euro/ton)') +
ggtitle(paste(colnames(data)[i+1], "Price and Jumps", sep = " "))+
labs(colour = "Legend")  


###############Plot all jumps in 7 series ####################
pdf("Jumps.pdf", width = 12, height = 7) 

for (i in 1:7){
    data1 =  data[outliers[,i][!is.na(outliers[,i])]-1,]
    p = ggplot(data = data, aes(x = data[,1], y = data[,i+1]))+
        geom_line(colour = 3)+
        geom_point(data = data1, aes(x = data1[,1], y = data1[,i+1], colour = 'Jumps'), size = 2, shape = 3, stroke = 1)+
        xlab('Date') +
        ylab('Price (Euro/ton)') +
        ggtitle(paste(colnames(data)[i+1], "Price and Jumps", sep = " "))+
        labs(colour = "Legend")  
    print(p)
}
dev.off()

