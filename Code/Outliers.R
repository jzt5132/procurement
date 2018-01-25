install.packages('tsoutliers')
library('tsoutliers')

set.seed(123)
y <- arima.sim(model = list(ar = 0.7, ma = -0.4), n = 120)
y[15] <- -4
y[45] <- 5
y[80:120] <- y[80:120] + 5
y <- round(y, 2)
plot(y)

fit <- forecast::auto.arima(x = y, allowdrift = FALSE, ic = "bic")
pars <- coefs2poly(fit)
resid <- residuals(fit)

otypes <- c("AO", "LS", "TC")
mo0 <- locate.outliers(resid, pars, types = otypes)
mo0
mo1 <- locate.outliers.iloop(resid, pars, types = otypes)
mo1
mo2 <- locate.outliers.oloop(y, fit, types = otypes)
mo2$iter
mo2$outliers


yo = tso(y, types = c("AO","LS","TC"),maxit.iloop=1000)
plot(yo)

dat = as.data.frame(cbind(c(1871:1970), as.numeric(Nile)))
ggplot(data = dat, aes(x = dat[,1], y = dat[,2], colour = 'Nile'))+geom_line()
       
nile_ol = tso(Nile,types = c("AO","LS","TC"))
plot(nile_ol)

ooo = outliers("AO", 20, weight = 1)
ooo
