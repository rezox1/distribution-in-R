#нормальное распределение

gaus<-rnorm(100, mean = 0.83, sd = sqrt(1.77))

d<-density(gaus)
plot(d,main="График плотности вероятности\nэкспериментального распределения",
     xlab = "", ylab="")

xfit<-seq(min(gaus), max(gaus), length=100)
yfit<-dnorm(xfit, mean=mean(gaus), sd=sd(gaus))
plot(xfit,yfit,main="График плотности вероятности\nнормального распределения",
     xlab = "", ylab="", type="l",lwd=1)

#распределение Пуассона

puasson<-rpois(100, lambda = 2)

numnum<-sort(unique(puasson))
numnum1<-rep(0,length(numnum))
for (i in 1:length(numnum)){
  for(g in 1:length(puasson)){
    if ((puasson[g]==numnum[i]))
      numnum1[i]<-numnum1[i]+(1/length(puasson));
  }
}
plot(numnum, numnum1,
     main="График функции вероятности\nэкспериментального распределения",
     xlab = "", ylab="",type="l",lwd=1)

xfit1<-sort(puasson)
yfit1<-dpois(xfit1, lambda = 2)
plot(xfit1,yfit1,main="График функции вероятности\nраспределения Пуассона",
     xlab = "", ylab="", type="l",lwd=1)

#экспоненциальное распределение

expon<-rexp(n = 100, rate = 0.83)

exxx<-density(expon)
plot(exxx,main="График плотности вероятности\nэкспериментального распределения",
     xlab = "", ylab="")

xfit2<-seq(min(expon), max(expon), length=100)
yfit2<-dexp(xfit2, rate =0.83)
plot(xfit2,yfit2,
     main="График плотности вероятности\nэкспоненциального распределения",
     xlab = "", ylab="", type="l",lwd=1)
