source('scripts/utils.R')
library(pracma)

x <- seird(log(2)/2.6,S0=19e6)

x <- x[date>as.Date('2020-01-20') & date<as.Date('2020-06-01')]

a <- min(x$rt)
b <- max(x$rt)
c <- min(x$D)
d <- max(x$D)

par(mfrow=c(3,1))

# plotyy(xlab = 'Date',ylab='I(t)',col.y2='black',x1=x$day,y1=x$new_confirmed,
#        x2=x$day,y2=x$D,lwd=2,main='Simulated SEIR Epidemic')

plot(x$date,x$I/max(x$I),type='l',col='red',lwd=2,xlab='Time',ylab='Relative Value',main='SEIR epidemic')
lines(x$date,x$D/max(x$D),type='l',col='black',lwd=2)
lines(x$date,(x$rt-min(x$rt))/(max(x$rt)-min(x$rt)),col='magenta',lwd=2)
legend('left',col=c('magenta','red','black'),legend = c('r(t)','I(t)','D(t)'),lty=c(1,1,1),lwd=c(2,2,2))

plot(x$date,(x$rt-a)/(b-a),type='l',col='magenta',lwd=5,ylim=c(0,1),xlab='Time',ylab='Relative value')
lines(x$date,(x$D-c)/d,lwd=3)
lines(x$date,(shift(x$D,19,type='lead')-c)/d,lwd=3,lty=2)
lines(x$date,(shift(x$D,32,type='lead')-c)/d,lwd=3,lty=3)
legend('right',legend=c('r(t)','D(t)','D(t+tau+1/a)','D(t+32)'),
       col=c('magenta','black','black','black'),lwd=3,lty=c(1,1,2,3))

plot(x$D,x$rt,lwd=3,type='l',xlab='Cumulative Deaths',ylab='r(t)')
lines(shift(x$D,19,type='lead'),x$rt,lwd=3,lty=2)
lines(shift(x$D,32,type='lead'),x$rt,lwd=3,lty=3)
abline(h=0)



par(mfrow=c(1,1))

plot(19e6-x$S,shift(x$D,19,type='lead'),type='l',ylim=c(0,2e4),xlim=c(0,2e4/0.002))


beta <- unique(x[!is.na(beta)]$beta)
mu=4e-4
gamma=1/9
N=19e6
a=1/3
c <- a/(log(2)/3+gamma+mu)
xx <- x$D
yy <- log(2)/2.6-beta*(1+gamma/mu)
yy <- log(2)/2.6-.7*beta*(1+gamma/mu)*x$D
# yy <- log(2)/2.6-(beta^2*N)/mu*x$D


plot(shift(x$D,19,type='lead'),x$rt,lwd=3,lty=2,
     xlab='D(t+tau+1/a)',ylab='r(t)',main='Approximate Linearity of r(D) for r>0')
lines(shift(x$D,18,type='lead'),x$rt,lwd=3,lty=1)
lines(shift(x$D,20,type='lead'),x$rt,lwd=3,lty=3)
lines(xx,yy)
abline(h=0)

date <- x$date
I <- x$I
d <- x$D
D <- c(0,diff(x$D))

par(mfrow=c(2,1))
plot(date,I,type='l',lwd=2,col='red')
abline(v=date[which.max(I)])
plot(date,D,type='l',lwd=2)
lines(date,shift(D,16,type='lead'),lwd=2,lty=2)
abline(v=date[which.max(shift(D,17,type='lead'))])


plot(shift(d,16,type='lead'),I,type='l')
