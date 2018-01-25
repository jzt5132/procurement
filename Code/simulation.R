gc()
library(ggplot2)
library(fdrtool)
library(truncnorm)
library(rootSolve)
########## Go = norm(0,10), c = 20############################
set.seed(1311122231)
#set.seed(2454543)
c <- 20
#G_0 <- function(n) rnorm(n, 0, 3) #different personal have different G_0
#e_G = 0
#G_0 <- function(n) rgamma(n, 2, 2)
#e_G = 1
#G_0 <- function(n) runif(n, 0, 1)
#e_G = 1/2
#G_0 = function(n) rbeta(n, 2 , 5)
#e_G = 2/7
#theta = 3 * sqrt(pi/2)
#G_0 = function(n) rhalfnorm(n, theta)
#e_G = 1 / theta
G_0 = function(n) rtruncnorm(n, a=-0, b=Inf, mean = 0, sd = 3)
e_G = etruncnorm(a=0, b=10, mean=0, sd=3)  #E_Y
v_G = vtruncnorm(a=0, b=10, mean=0, sd=3)  #Var_Y
e_2_G = v_G + (e_G) ^ 2  # E_Y^2

#check
etruncnorm(a= -1, b=Inf, mean=0, sd=3) ^ 2 + vtruncnorm(a=-1, b=Inf, mean=0, sd=3)

n <- 100
n_buyers = n * 10
b <- rbeta(n, 1, c)
p <- numeric(n)
p[1] <- b[1]
p[2:n] <- sapply(2:n, function(i) b[i] * prod(1 - b[1:(i-1)]))
y <- G_0(n)
s <- sample(y, prob = p, replace = TRUE)  ##s_k local supply
#hist(y)
#hist(s)
total_supply = sum(s)

#calcualte personal estimate of s
est_s = function(e_G, k, s){
  result = (c * e_G + sum(s[1:k]) ) / (c + k)
  return(result)
}


e_s = sapply(1:n, function(i) est_s(e_G, i, s))   #expectation_G0 = e_G

est_s_square = function(e_2_G, k, s){
  result = (c * e_2_G + sum(s[1:k] ^ 2) ) / (c + k)
}

e_s_square = sapply(1:n, function(i) est_s_square(e_2_G, i, s))   #expectation_G0 = e_2_G

#caculate estimate of Z
est_z = function(e_s, k, s){
  result = sum(s[1:k]) + (n - k) * e_s[k]
  return(result)
}


e_Supply = sapply(1:n, function(i) est_z(e_s, i, s)) 


D = n_buyers * 10
e_z = D - e_Supply
#plot(e_z)


###########check########
est_z_square = function(e_s_square, k, s){
  result = sum(s[1:k] ^ 2) + (n - k) * e_s_square[k]
  return(result)
}

est_Supply_square = sapply(1:n, function(i) est_z_square(e_s_square, i, s))

e_z_square = D^2  - 2*D*e_Supply + est_Supply_square
#plot(e_z_square)





#k = 1
#s_k = s[k]
#e_z_k = e_z[k]
#bid_f = function(x){
#  exp(x) + K * x - K * (lambda + s_k * e_z_k - 1)
#}
#b_k = uniroot.all(bid_f, c(0, 500))


K = n_buyers * exp(15)
lambda = 10
bid = rep(-10, n)
for (i in 1: n){
  k = i
  e_z_k = e_z_square[k]
  bid_f = function(x){
    exp(x) + K * x - K * (lambda + e_z_k - 1)
  }
  b_k = uniroot.all(bid_f, c(0, 500))
  bid[i] = b_k
}
#plot(bid)


################true bid w/ complete info###############
bid_star = function(x){
  exp(x) + K * x - K * (lambda + (D - total_supply) ^ 2 - 1)
}
bid_star = uniroot.all(bid_star, c(0, 500))


#####plot##########
ggplot() +
geom_line(data = as.data.frame(cbind(1:n, bid)), aes(x = 1:n, y = bid, color = 'bid'))+
geom_line(data = as.data.frame(cbind(1:n, rep(bid_star, n))), aes(x = 1:n, y = rep(bid_star, n), color = 'bid_info'))+
#geom_line(data = as.data.frame(cbind(1:n, e_s * n)), aes(x = 1:n, y = e_s * n, color = 'est_s'))+
#geom_line(data = as.data.frame(cbind(1:n, e_z/max(e_z))), aes(x = 1:n, y = e_z/max(e_z), color = 'est_z'))+
xlab('Observation') +
ylab('estimate of s_{K+1}') +
ggtitle('simulated personal estimate of s_{k+1} given s1,...,sk') +
labs(colour = "Legend")



