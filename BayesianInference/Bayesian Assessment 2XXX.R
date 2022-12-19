# Part 1 - Plot the density function
# Define the density function f(x) and E[f(x)]
density_func <- function(t){
  (dbeta(t,1,5)+dbeta(t,3,5)+dbeta(t,10,5))/3
}
E_density_func <- function(t){
  t*density_func(t)
}


x <- seq(0, 1, length=100)
plot(x,density_func(x),col="blue",lwd=1, main="Part 1 - Plot Density Function",
     ylab="Denisty f(x)",xlab='x value',type='l')


# Part 2 - Implement an accept/reject algorithm
# Set the seed
set.seed(1234)

# find the value of K and its position
K_lower_EP <- density_func(0)
K_local_max <- optimize(density_func, interval=c(0, 1), maximum=TRUE)
K_upper_EP <- density_func(1)
K <- max(c(K_lower_EP,K_local_max[[2]],K_upper_EP))
K_index <- which.max(c(K_lower_EP,K_local_max[[2]],K_upper_EP))
if (K_index==1) {
  K_x = 0
} else if (K_index==2) {
  K_x = K_local_max[[1]]
  } else if (K_index==3) {
    K_x = 1
} 

# Simulate uniform distribution
N=10000
theta  <- runif(N)

# Define the acceptance probability
acc_prob <- density_func(theta) / K
             
# Accept or reject simulated values
accept <- runif(N) < acc_prob


# Part 3 - Compute the acceptance rate and compare it with the theoretical
sum(accept)/N
1/K

# Part 4 - Implement an Importance sampling algorithm
# Use the same values
w = density_func(theta) #/dunif(theta)
# Compute the importance weights
W=w/sum(w)


# Part 5 - Plot to compare density, accept/reject & importance sampling
# Plot histogram
hist(theta[accept],probability=T,xlab=expression(theta),
     ylab="Density",main="Part 5",xlim=c(0,1),ylim=c(0,K))
lines(th,density_func(th),col="blue",lwd=2)
d=density(theta,weights=W,from=0,to=1)
lines(d,col="cyan",lwd=2,lty=2)


# Part 6 - Which algorithm is better


# Compute quantiles for accept/reject
quantile(theta[accept])

# Compute quantiles for importance sampling
theta_IS <- sample(theta,size=N,prob=W,replace=T)
lines(density(theta_IS),col="orange")
quantile(theta_IS)

# Compute MC Error graph
M <- round(seq(10,sum(accept),length=100))
mth <- c() 
ic_l <- c()
ic_u <- c()
for(j in 1:100)
{
  mean(theta[accept][1:M[j]])
  mth[j] <- mean(theta[accept][1:M[j]])
  ic_l[j] <- mth[j] - 1.96 * sqrt(var(theta[accept][1:M[j]])/M[j])
  ic_u[j] <- mth[j] + 1.96 * sqrt(var(theta[accept][1:M[j]])/M[j])
}
expected = integrate(E_density_func,0,1)

plot(M,mth,type="l")
lines(M,ic_l,lty=2)
lines(M,ic_u,lty=2)
abline(h=expected[[1]],col="red")

nth <- c() 
ic_l <- c()
ic_u <- c()
for(k in 1:100)
{
  nth[k] <- sum(w[1:M[k]]*theta[1:M[k]])/sum(w[1:M[k]])
  ic_l[j] <- mth[j] - 1.96 * sqrt(var(theta[accept][1:M[j]])/M[j])
  ic_u[j] <- mth[j] + 1.96 * sqrt(var(theta[accept][1:M[j]])/M[j])
}

plot(M,nth,type="l")
lines(M,ic_l,lty=2)
lines(M,ic_u,lty=2)
abline(h=expected[[1]],col="red")
