#pract:- 11set.seed(123)
true_lam=0.5
pc=0.2
cens_lam=pc*true_lam/(1-pc)

x=rexp(300,rate=true_lam)
cens_x=rexp(300,rate=cens_lam)

times=pmin(x,cens_x)
delta=as.numeric(x<=cens_x)

lam_hat=sum(delta)/sum(times)
lam_hat

times
ord=order(times)
order_x=times[ord]
status=delta[ord]

risk=rev(cumsum(rev(rep(1,300))))
risk1=300:1
d=status
haz_line=d/risk
cum_haz=cumsum(haz_line)

plot(order_x,cum_haz,col="blue",main="cum hazard vs time",lwd=2,type="s",xlab="time",ylab="cum hazard")

abline(0,lam_hat,col="green",lty=2,lwd=2)
abline(0,true_lam,col="red",lty=2,lwd=2)
legend("topleft",col=c("blue","green","red"),legend=c("Nelson-Aelon" ,"mle_lam","true_lam"),lty=c(1,2,3),lwd=2)
