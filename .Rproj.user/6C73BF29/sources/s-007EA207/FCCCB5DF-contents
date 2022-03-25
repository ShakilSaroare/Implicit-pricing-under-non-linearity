dat<-read.csv("wake_exercise.csv",header=T, fileEncoding="UTF-8-BOM")
head(dat)

attach(dat)

d<-dat[,c("living_area","baths","age","fireplace","garage_area","lakedist")]
funs <- list(Max = max, Mean = mean, Min = min, Std_dev = sd)
sapply(funs, function(x) sapply(d, x))


par(mfrow=c(2,2))
options(repr.plot.width=8,repr.plot.height=8)
hist(lprice)
hist(living_area)
hist(baths)
hist(age)


intercept<-rep(1,length(lprice))
x<-as.matrix(cbind(intercept,living_area,baths,age,fireplace,garage_area,condA,condB,condC))
y<-lprice

ols_summary<- function(x, y){
  beta <- solve(t(x)%*%x)%*%(t(x)%*%y)
  res<-y-x%*%beta
  s2hat<-as.numeric((t(res)%*%res)/(length(res)-dim(x)[2]))
  cov.mat<-s2hat[1]*solve(t(x)%*%x)
  std.err<-sqrt(diag(cov.mat))
  tval<-beta/std.err
  names(tval)="tval"
  pval<-2*pt(-abs(tval),length(res)-dim(x)[2])
  summ.mod<-data.frame(cbind(beta,std.err,tval,pval))
  names(summ.mod)<-c("Estimates","Stdard_err","Critical t-value","p-value")
  return(summ.mod)
}

result<- ols_summary(x, y)
result

imp_pr<-result$Estimates[2]*price
options(repr.plot.width=4,repr.plot.height=4)
hist(imp_pr,main="Implicit Price Distribution",xlab="Implicit Price for living area")


dat["log.lakedist"]<- ifelse(lakedist<=1, 0, log(lakedist))

x1<-as.matrix(cbind(intercept,living_area,baths,age,fireplace,garage_area,condA,condB,condC,lakedist,dat["log.lakedist"]))
y<-lprice

result1<- ols_summary(x1, y)
result1

imp_pr1<-mean(result1$Estimates[10]*price)+result1$Estimates[11]*mean((price/lakedist))
print(paste0("Marginal WTP for lake access= ",imp_pr1))

x2<-as.matrix(cbind(intercept,living_area,baths,age,fireplace,garage_area,lakedist,log.lakedist))
y<-lprice

result2<- ols_summary(x2, y)
result2

imp_pr2<-mean(result2$Estimates[07]*price)+mean(result2$Estimates[08]*(price/lakedist))
print(paste0("Marginal WTP for lake access= ",imp_pr2))


x3<-as.matrix(cbind(intercept,living_area,baths,age,fireplace,garage_area,condA,condB,condC,lakedist,
                    log.lakedist,median_commute))
y<-lprice

result3<- ols_summary(x3, y)
result3

imp_pr3<-mean(result3$Estimates[10]*price)+mean(result3$Estimates[11]*(price/lakedist))
print(paste0("Implicit price for lake access= ",imp_pr3))

x4<-as.matrix(cbind(intercept,living_area,baths,age,fireplace,garage_area,condA,condB,condC,lakedist,log.lakedist,
                    median_commute,zone1,zone2,zone3,zone4,zone5,zone6,zone7,zone8,zone9,zone10,zone11,zone12,zone13,
                    zone14,zone15,zone16,zone17,zone18))
y<-lprice

result4<- ols_summary(x4, y)
result4

imp_pr4<-mean(result4$Estimates[10]*price)+mean(result4$Estimates[11]*(price/lakedist))
print(paste0("Implicit price for lake access= ",imp_pr4))

tr_dist<-c()
for (i in 1:length(y)){
  tr_dist[i]<-max(1-(lakedist[i]/10)^0.5,0)}
x5<-as.matrix(cbind(intercept,living_area,baths,age,fireplace,garage_area,condA,condB,condC,lakedist,tr_dist,median_commute,
                    zone1,zone2,zone3,zone4,zone5,zone6,zone7,zone8,zone9,zone10,zone11,zone12,zone13,
                    zone14,zone15,zone16,zone17,zone18))
y<-lprice

result5<- ols_summary(x5, y)
result5

imp_p5<-c()
for (i in 1:length(y)){
  if (tr_dist[i]==0)imp_p5[i]=price[i]*result5$Estimates[10]
  else imp_p5[i]=price[i]*result5$Estimates[10]-((price[i]*result5$Estimates[10])/(2*sqrt(10)*sqrt(lakedist[i])))
}
imp_pr5<-mean(imp_p5)
print(paste0("Implicit price for lake access= ",imp_pr5))