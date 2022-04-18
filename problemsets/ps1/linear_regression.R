set.seed(8675309)
zlibrary(MASS)
xz<-mvrnorm(10000,mu=rep(0,2),Sigma=diag(1,2))
x<-xz[,1]
z<-xz[,2]
y<-.5*x+.7*z+rnorm(length(x))
df<-data.frame(x=x,z=z,y=y)

m<-lm(y~x+z,df) #note that here we are observing z!! different than what we previously considered here. https://github.com/ben-domingue/educ452/blob/main/regression_example.R



mat<-list()
for (rho in seq(0,.9,by=.1)) {
  t1er_EST<-numeric(); t2er_EST<-numeric()
  for (i in 1:500) {
    xz<-mvrnorm(10000,rep(0,2),
                Sigma=matrix(c(1,rho,rho,1),2,2))
    x<-xz[,1]
    z<-xz[,2]
    y<-.5*x+.7*z+rnorm(length(x))   
    df<-data.frame(x=x,z=z,y=y)
    m<-lm(y~x+z,df)
    t1er_EST[i]<-summary(m)$coef[2,2]
    t2er_EST[i]<-summary(m)$coef[3,2]
  }
  mat[[as.character(rho)]]<-c(rho,mean(t1er_EST),mean(t2er_EST))
}
data <- do.call("rbind",mat)

par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(data[,1],data[,2],type="l",col="red",
     xlab="cor(x,z)",ylab="standard error of x")
plot(data[,1],data[,3],type="l",col="green",
     xlab="cor(x,z)",ylab="standard error of z")
