
require(survival)
  #글꼴설정
windowsFonts(A = windowsFont("Times New Roman"))

#오즈비 도출 함수 정의
ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result
}


dstrat_sep3_result
dstrat_sep4_result

table3


#오즈비 도출 후 데이터 저장
table1<-ORtable(dstrat_all_result)
table2<-ORtable(dstrat_sep_result)
table3<-ORtable(dstrat_sep1_result)
table4<-ORtable(dstrat_sep2_result)
table5<-ORtable(dstrat_sep3_result)
table6<-ORtable(dstrat_sep4_result)
table7<-ORtable(dstrat_sep_result_5)


dstrat_sep3_result

write.csv(table1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table1.csv")
write.csv(table2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table2-1-1.csv")
write.csv(table3,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table3.csv")
write.csv(table4,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table4-1.csv")
write.csv(table5,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table5-1.csv")
write.csv(table6,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table6-1.csv")

write.csv(table7,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table7.csv")


table2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table2-1-1.csv")
table4<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table4-1.csv")
table5<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table5-1.csv")
table6<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table6-1.csv")

table7<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/table7.csv")


table7

table1

table1
table2

table1<-column_to_rownames(table1,var='X')
table2<-column_to_rownames(table2,var='X')
table3<-column_to_rownames(table3,var='X')
table4<-column_to_rownames(table4,var='X')
table5<-column_to_rownames(table5,var='X')
table6<-column_to_rownames(table6,var='X')

utable4

table1<-table1[-1,]
table2<-table2[-1,]
table3<-table3[-1,]
table4<-table4[-1,]
table5<-table5[-1,]
table6<-table6[-1,]

utable1<-utable1[-12,]

windows(width=80, height=60, rescale="R", title="R")


#오즈비 plot 새로운 함수
ORplot.sub=function(result,type=1,xlab="",ylab="",show.OR=TRUE,show.CI=FALSE,
                    sig.level=1,cex=1.2,lwd=2,pch=18,col=NULL,...){
  result=result[result[[4]]<=sig.level,]
  count=length(result[,1])
  
  if(count<1) {
    cat("No variable to be plotted found")
    return(invisible())
  }
  if(is.null(col) | (length(col)!=2) ) {
    if(type==3) {col1="salmon";col2="darkturquoise"}
    else {col1="firebrick2";col2="dodgerblue3"}
  } else {
    col1=col[1];col2=col[2]
  }
  max=1.5
  min=0.7
  x=log10(result[,1])*5/log10(max)
  x1=log10(result[,2])*5/log10(max)
  x2=log10(result[,3])*5/log10(max)
  #opar<-par(no.readonly=TRUE)
  if(show.CI) par(mar=c(5,16,4,10))
  else par(mar=c(5,8,4,2))
  if(xlab=="") xlab=ifelse(colnames(result)[1]=="OR","Odds Ratios","Harzard Ratios")
  plot(result[,1],count:1,type="n",axes=FALSE,ylim=c(0.5,count+0.5),
       xlim=c(log10(min)*5/log10(max),5),xlab="",ylab=ylab,...)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border="darkgray",
       col = "white")
  abline(h=count:1,col="white",lwd=2)
  abline(v=0,lty=2,col="white",lwd=2)
  range=seq(0.75,1.25,0.25)
  range2=seq(0.75+0.25,1.5,0.25)
  abline(v=log10(0.75+0.25)*5/log10(2),col="black",lwd=1.5)
  yscale=par("usr")[3:4]
  xscale=par("usr")[1:2]
  text(x=log10(range)*5/log10(max),y=par("usr")[3],range,pos=1,cex=0.8,xpd=TRUE,family="A")
  if(length(range2)<4) text(x=log10(range2)*5/log10(max),y=par("usr")[3],range2,pos=1,cex=0.8,xpd=TRUE,family="A")
  text(y=count:1,par("usr")[1],labels=rownames(result),pos=2,cex=1.15,xpd=TRUE,family="A")
  #text(x=5,y=par("usr")[3],xlab,pos=1,cex=1,xpd=TRUE)
  text(x=mean(range(xscale)),y=par("usr")[3],xlab,pos=1,cex=0.6,offset=2,xpd=TRUE,family="A")
  p=c()
  for(i in 1:count){
    if(is.nan(result[i,4])) p[i]=sprintf("%.2f",result[i,1])
    else if(result[i,4]<0.001) p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")***")
    else if(result[i,4]<0.01) p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")**")
    else if(result[i,4]<0.05) p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")*")
    else p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")")
  }
  
  if(type<3){
    segments(x1,count:1,x2,count:1,lty=1,
             col=ifelse(x>0,col1,col2),lwd=lwd)
    if(show.OR) text(x,count:1,p,pos=3,offset=200,cex=0.8)
    if(type==2) {
      segments(x1,(count:1)-0.1,x1,(count:1)+0.1,lty=1,
               col=ifelse(x>0,col1,col2),lwd=lwd)
      segments(x2,(count:1)-0.1,x2,(count:1)+0.1,lty=1,
               col=ifelse(x>0,col1,col2),lwd=lwd)
      points(x,count:1,col=ifelse(x>0,col1,col2),pch=pch,cex=cex)
    } else{
      points(x,count:1,col=ifelse(x>0,col1,col2),pch=17,cex=cex)
    }
    
  }
  else{
    left=ifelse(x>0,0,x)
    right=ifelse(x>0,x,0)
    height=0.25
    rect(left,(count:1)+height,right,(count:1)-height,
         col=ifelse(x>0,col1,col2))
    segments(x1,count:1,x2,count:1,lty=1,
             col="black",lwd=0.5)
    segments(x1,(count:1)-0.1,x1,(count:1)+0.1,lty=1,
             col="black",lwd=0.5)
    segments(x2,(count:1)-0.1,x2,(count:1)+0.1,lty=1,
             col="black",lwd=0.5)
    points(x,count:1,col="black",pch=20,cex=cex)
    if(show.OR) text(0,count:1,p,pos=ifelse(x>0,2,4),offset=ifelse(x>0,5,40),cex=1,family="A")
  }
  if(show.CI){
    text(y=count:1,par("usr")[2],
         paste(p),
         pos=4,xpd=TRUE,family="A")
    CI.title=paste(colnames(result)[1],"(95% CI)")
    text(y=max(yscale),par("usr")[2],CI.title,pos=4,xpd=TRUE,family = "A")
  }
  #par(opar)
}

#overestimate 함수
ORplot.sub2=function(result,type=1,xlab="",ylab="",show.OR=TRUE,show.CI=FALSE,
                    sig.level=1,cex=1.2,lwd=2,pch=18,col=NULL,...){
  result=result[result[[4]]<=sig.level,]
  count=length(result[,1])
  
  if(count<1) {
    cat("No variable to be plotted found")
    return(invisible())
  }
  if(is.null(col) | (length(col)!=2) ) {
    if(type==3) {col1="salmon";col2="darkturquoise"}
    else {col1="firebrick2";col2="dodgerblue3"}
  } else {
    col1=col[1];col2=col[2]
  }
  max=1.5
  min=0.7
  x=log10(result[,1])*5/log10(max)
  x1=log10(result[,2])*5/log10(max)
  x2=log10(result[,3])*5/log10(max)
  #opar<-par(no.readonly=TRUE)
  if(show.CI) par(mar=c(5,16,4,10))
  else par(mar=c(5,8,4,2))
  if(xlab=="") xlab=ifelse(colnames(result)[1]=="OR","Odds Ratios","Harzard Ratios")
  plot(result[,1],count:1,type="n",axes=FALSE,ylim=c(0.5,count+0.5),
       xlim=c(log10(min)*5/log10(max),5),xlab="",ylab=ylab,...)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border="darkgray",
       col = "white")
  abline(h=count:1,col="white",lwd=2)
  abline(v=0,lty=2,col="white",lwd=2)
  range=seq(0.75,1.25,0.25)
  range2=seq(0.75+0.25,1.5,0.25)
  abline(v=log10(0.75+0.25)*5/log10(2),col="black",lwd=1.5)
  yscale=par("usr")[3:4]
  xscale=par("usr")[1:2]
  text(x=log10(range)*5/log10(max),y=par("usr")[3],range,pos=1,cex=0.8,xpd=TRUE,family="A")
  if(length(range2)<4) text(x=log10(range2)*5/log10(max),y=par("usr")[3],range2,pos=1,cex=0.8,xpd=TRUE,family="A")
  text(y=count:1,par("usr")[1],labels="",pos=2,cex=1.15,xpd=TRUE,family="A")
  #text(x=5,y=par("usr")[3],xlab,pos=1,cex=1,xpd=TRUE)
  text(x=mean(range(xscale)),y=par("usr")[3],xlab,pos=1,cex=0.6,offset=2,xpd=TRUE,family="A")
  p=c()
  for(i in 1:count){
    if(is.nan(result[i,4])) p[i]=sprintf("%.2f",result[i,1])
    else if(result[i,4]<0.001) p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")***")
    else if(result[i,4]<0.01) p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")**")
    else if(result[i,4]<0.05) p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")*")
    else p[i]=paste(sprintf("%.2f",result[i,1])," (",sprintf("%.2f",result[i,2]),"-",sprintf("%.2f",result[i,3]),")")
  }
  if(type<3){
    segments(x1,count:1,x2,count:1,lty=1,
             col=ifelse(x>0,col1,col2),lwd=lwd)
    if(show.OR) text(x,count:1,p,pos=3,offset=200,cex=0.8)
    if(type==2) {
      segments(x1,(count:1)-0.1,x1,(count:1)+0.1,lty=1,
               col=ifelse(x>0,col1,col2),lwd=lwd)
      segments(x2,(count:1)-0.1,x2,(count:1)+0.1,lty=1,
               col=ifelse(x>0,col1,col2),lwd=lwd)
      points(x,count:1,col=ifelse(x>0,col1,col2),pch=pch,cex=cex)
    } else{
      points(x,count:1,col=ifelse(x>0,col1,col2),pch=17,cex=cex)
    }
    
  }
  else{
    left=ifelse(x>0,0,x)
    right=ifelse(x>0,x,0)
    height=0.25
    rect(left,(count:1)+height,right,(count:1)-height,
         col=ifelse(x>0,col1,col2))
    segments(x1,count:1,x2,count:1,lty=1,
             col="black",lwd=0.5)
    segments(x1,(count:1)-0.1,x1,(count:1)+0.1,lty=1,
             col="black",lwd=0.5)
    segments(x2,(count:1)-0.1,x2,(count:1)+0.1,lty=1,
             col="black",lwd=0.5)
    points(x,count:1,col="black",pch=20,cex=cex)
    if(show.OR) text(0,count:1,p,pos=ifelse(x>0,2,4),offset=ifelse(x>0,5,40),cex=1,family="A")
  }
  if(show.CI){
    text(y=count:1,par("usr")[2],
         paste(p),
         pos=4,xpd=TRUE,family="A")
    CI.title=paste(colnames(result)[1],"(95% CI)")
    text(y=max(yscale),par("usr")[2],CI.title,pos=4,xpd=TRUE,family = "A")
  }
  #par(opar)
}



par(mfrow = c(4, 2))
par(mfrow = c(2, 2))
par(mfrow = c(1, 2))

#최종 오즈비 plot 도출(overestimation)
par(mfrow = c(1, 2))

ORplot.sub(utable1, type=1,main="Plot for Odds Ratios(For all ages) - Underestimated Group",
            show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(table1, type=1,main="Plot for Odds Ratios(For all ages) - Overestimated Group",
           show.OR = TRUE,show.CI = TRUE,family="A")



par(mfrow = c(1, 2))

ORplot.sub(utable2, type=1,main="Plot for Odds Ratios(For all ages) - Underestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(table2, type=1,main="Plot for Odds Ratios(For all ages) - Overestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")

par(mfrow = c(1, 2))

ORplot.sub(table3, type=1,main="Plot for Odds Ratios(Under age 19) - Overestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")

par(mfrow = c(1, 2))

ORplot.sub(utable4, type=1,main="Plot for Odds Ratios(Between age 19 to 45) - Underestimated Group", show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(table4, type=1,main="Plot for Odds Ratios(Between age 19 to 45) - Overestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")

par(mfrow = c(1, 2))

ORplot.sub(utable5, type=1,main="Plot for Odds Ratios(Between age 46 to 59) - Underestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(table5, type=1,main="Plot for Odds Ratios(Between age 46 to 59) - Overestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")

par(mfrow = c(1, 2))

ORplot.sub(utable6, type=1,main="Plot for Odds Ratios(Above age 59) - Underestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(table6, type=1,main="Plot for Odds Ratios(Above age 59) - Overestimated Group",show.OR = TRUE,show.CI = TRUE,family="A")


#최종 오즈비 plot 도출(underestimation)

ORplot.sub(table6, type=1,main="Plot for Odds Ratios(For all ages)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")

install.packages("tidyverse")
library(tibble)
library(tidyverse)
install.packages("metan")
library(metan)

utable2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/under1/table2.csv")
utable4<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/under1/table4.csv")
utable5<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/under1/table5.csv")
utable6<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/under1/table6.csv")

utable1<-column_to_rownames(utable1,var='X')
utable2<-column_to_rownames(utable2,var='X')
utable4<-column_to_rownames(utable4,var='X')
utable5<-column_to_rownames(utable5,var='X')
utable6<-column_to_rownames(utable6,var='X')


ORplot.sub(table4, type=1,main="Plot for Odds Ratios(For all ages)",xlab="                           ",
           show.OR = TRUE,show.CI = TRUE,family="A")

#
ORplot.sub(utable1, type=1,main="Plot for Odds Ratios(For all ages)",xlab="                           ",
           show.OR = TRUE,show.CI = TRUE,family="A")


ORplot.sub2(table1, type=1,main="Plot for Odds Ratios(For all ages)",xlab="                           ",
            show.OR = TRUE,show.CI = TRUE,family="A") 


ORplot.sub(utable2, type=1,main="Plot for Odds Ratios(For all ages2)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(table2, type=1,main="Plot for Odds Ratios(For all ages2)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub(table3, type=1,main="Plot for Odds Ratios(Age under 18)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(utable3, type=1,main="Plot for Odds Ratios(Age under 18)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")

par(mfrow = c(1, 2))

ORplot.sub(utable4, type=1,main="Plot for Odds Ratios(Between age 19 - 45)",xlab="                                                       ",
            show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub2(table4, type=1,main="Plot for Odds Ratios(Between age 19 - 45)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")


ORplot.sub(utable5, type=1,main="Plot for Odds Ratios(Between age 46 - 59)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")


ORplot.sub2(table5, type=1,main="Plot for Odds Ratios(Between age 46 - 59)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")


ORplot.sub(table6, type=1,main="Plot for Odds Ratios(Above age 60)",xlab="                                                       ",
           show.OR = TRUE,show.CI = TRUE,family="A")










#2


#ORplot.sub2=function(result,type=1,xlab="",ylab="",show.OR=TRUE,show.CI=FALSE,
                    #sig.level=1,cex=1.2,lwd=2,pch=18,col=NULL,...){
  result=result[result[[4]]<=sig.level,]
  windows(width=80, height=60, rescale="R", title="R")
  count=length(result[,1])
  if(count<1) {
    cat("No variable to be plotted found")
    return(invisible())
  }
  if(is.null(col) | (length(col)!=2) ) {
    if(type==3) {col1="salmon";col2="darkturquoise"}
    else {col1="black";col2="black"}
  } else {
    col1=col[1];col2=col[2]
  }
  result=result[order(result[[1]],decreasing=TRUE),]
  
  max=2
  min=0.6
  x=log10(result[,1])*5/log10(max)
  x1=log10(result[,2])*5/log10(max)
  x2=log10(result[,3])*5/log10(max)
  #opar<-par(no.readonly=TRUE)
  if(show.CI) par(mar=c(5,16,4,10))
  else par(mar=c(5,8,4,2))
  if(xlab=="") xlab=ifelse(colnames(result)[1]=="OR","Odds Ratios","Harzard Ratios")
  plot(result[,1],count:1,type="n",axes=FALSE,ylim=c(0.5,count+0.5),
       xlim=c(log10(min)*5/log10(max),5),xlab="",ylab=ylab,...)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border="darkgray",
       col = "white")
  abline(h=count:1,col="white",lwd=2)
  abline(v=0,lty=2,col="white",lwd=2)
  range=seq(0.75,1.75,0.5)
  range2=seq(0.75+0.25,2,0.5)
  abline(v=log10(0.75+0.25)*5/log10(2),col="black",lwd=1.5)
  yscale=par("usr")[3:4]
  xscale=par("usr")[1:2]
  text(x=log10(range)*5/log10(max),y=par("usr")[3],range,pos=1,cex=0.8,xpd=TRUE,family="A")
  if(length(range2)<4) text(x=log10(range2)*5/log10(max),y=par("usr")[3],range2,pos=1,cex=0.8,xpd=TRUE,family="A")
  text(y=count:1,par("usr")[1],labels=rownames(result),pos=2,cex=1.15,xpd=TRUE,family="A")
  #text(x=5,y=par("usr")[3],xlab,pos=1,cex=1,xpd=TRUE)
  text(x=mean(range(xscale)),y=par("usr")[3],xlab,pos=1,cex=0.6,offset=2,xpd=TRUE,family="A")
  p=c()
  for(i in 1:count){
    if(is.nan(result[i,4])) p[i]=result[i,1]
    else if(result[i,4]<0.001) p[i]=paste(result[i,1],sep="")
    else if(result[i,4]<0.01) p[i]=paste(result[i,1],sep="")
    else if(result[i,4]<0.05) p[i]=paste(result[i,1],sep="")
    else p[i]=paste(result[i,1],"",sep="")
  }
  if(type<3){
    segments(x1,count:1,x2,count:1,lty=1,
             col=ifelse(x>0,col1,col2),lwd=lwd)
    if(show.OR) text(x,count:1,p,pos=3,offset=200,cex=0.8)
    if(type==2) {
      segments(x1,(count:1)-0.1,x1,(count:1)+0.1,lty=1,
               col=ifelse(x>0,col1,col2),lwd=lwd)
      segments(x2,(count:1)-0.1,x2,(count:1)+0.1,lty=1,
               col=ifelse(x>0,col1,col2),lwd=lwd)
      points(x,count:1,col=ifelse(x>0,col1,col2),pch=pch,cex=cex)
    } else{
      points(x,count:1,col=ifelse(x>0,col1,col2),pch=17,cex=cex)
    }
    
  }
  else{
    left=ifelse(x>0,0,x)
    right=ifelse(x>0,x,0)
    height=0.25
    rect(left,(count:1)+height,right,(count:1)-height,
         col=ifelse(x>0,col1,col2))
    segments(x1,count:1,x2,count:1,lty=1,
             col="black",lwd=0.5)
    segments(x1,(count:1)-0.1,x1,(count:1)+0.1,lty=1,
             col="black",lwd=0.5)
    segments(x2,(count:1)-0.1,x2,(count:1)+0.1,lty=1,
             col="black",lwd=0.5)
    points(x,count:1,col="black",pch=20,cex=cex)
    if(show.OR) text(0,count:1,p,pos=ifelse(x>0,2,4),offset=ifelse(x>0,5,40),cex=1,family="A")
  }
  if(show.CI){
    text(y=count:1,par("usr")[2],
         paste(p," (",sprintf("%.2f",result[,2])," - ",result[,3],")",sep=""),
         pos=4,xpd=TRUE,family="A")
    CI.title=paste(colnames(result)[1],"(95% C.I.)")
    text(y=max(yscale),par("usr")[2],CI.title,pos=4,xpd=TRUE,family = "A")
  }
  #par(opar)
}


library(ggplot2)
library(ggpubr)






#새롭게

atable1<-ORtable(dstrat_sep_result_1)
atable2<-ORtable(dstrat_sep_result_2)
atable3<-ORtable(dstrat_sep_result_3)
atable4<-ORtable(dstrat_sep_result_4)

write.csv(atable1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable1.csv")
write.csv(atable2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable2.csv")
write.csv(atable3,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable3.csv")
write.csv(atable4,"C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable4.csv")

atable1<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable1.csv")
atable2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable2.csv")
atable3<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable3.csv")
atable4<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/over/atable4.csv")

atable1<-column_to_rownames(atable1,var='X')
atable2<-column_to_rownames(atable2,var='X')
atable3<-column_to_rownames(atable3,var='X')
atable4<-column_to_rownames(atable4,var='X')
table7<-column_to_rownames(table7,var='X')


atable1


#이제 이 테이블들로 그래프 그리면 됨 ㅋㅋ해방!

par(mfrow = c(1, 2))


ORplot.sub(atable1, type=1,main="Plot for Odds Ratios(For all ages) - All Groups",show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub(table7, type=1,main="Plot for Odds Ratios(For all ages) - All Groups",show.OR = TRUE,show.CI = TRUE,family="A")

ORplot.sub(atable2, type=1,main="Plot for Odds Ratios(Between age 19 to 45) - All Groups", show.OR = TRUE,show.CI = TRUE,family="A")


ORplot.sub(atable3, type=1,main="Plot for Odds Ratios(Between age 46 to 59) - All Groups",show.OR = TRUE,show.CI = TRUE,family="A")


ORplot.sub(atable4, type=1,main="Plot for Odds Ratios(Above age 59) - All Groups",show.OR = TRUE,show.CI = TRUE,family="A")

par(mfrow = c(1, 2))





#figure 1
Figure1a <- read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/Underestimation.csv",header=T,
                     stringsAsFactors = F)
Figure1b <- read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/Overestimation.csv",header=T,
                     stringsAsFactors = F)

names(Figure1a) <- c("Group","Body.image.perception","Count")
names(Figure1b) <- c("Group","Body.image.perception","Count")

Figure1a

F1a<-ggbarplot(Figure1a,x="Group",y="Count",fill="Body.image.perception",palette = c("white", "black"))
F1a

F1b<-ggbarplot(Figure1b,x="Group",y="Count",fill="Body.image.perception",palette = c("white", "black"))
F1b

F1a<-F1a+scale_x_discrete(limits=c("Under 18", "Between 19 - 45", "Between 46 - 59","Above 60")) +
  ggtitle("Underestimation") + 
  theme_classic()+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 13, color = "black")) +
  theme(text=element_text(size=10,  family="A"))
F1a

F1b<-F1b+scale_x_discrete(limits=c("Under 18", "Between 19 - 45", "Between 46 - 59","Above 60")) +
  ggtitle("Overestimation") + 
  theme_classic()+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 13, color = "black")) +
  theme(text=element_text(size=10,  family="A"))
F1b

ggarrange(F1a,F1b,ncol = 2, nrow = 1)



library(plyr)

Figure1aa <- read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/All underestimation.csv",header=T,
                      stringsAsFactors = F)
Figure1bb <- read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/All Overestimation.csv",header=T,
                      stringsAsFactors = F)

names(Figure1aa) <- c("Group","Body.image.perception","Count")
names(Figure1bb) <- c("Group","Body.image.perception","Count")

Figure1bb

F1aa<-ggbarplot(Figure1aa,x="Group",y="Count",fill="Body.image.perception",palette = c("#00AFBB", "#FC4E07"))
F1aa

F1bb<-ggbarplot(Figure1bb,x="Group",y="Count",fill="Body.image.perception",palette = c("#00AFBB", "#FC4E07"))
F1bb


F1aa<-F1aa+scale_x_discrete(limits=c("Teens",
                                     "Twenties",
                                     "Thirties",
                                     "Forties",
                                     "Fifties",
                                     "Sixties",
                                     "Seventies(Over)")) +
  ggtitle("Underestimation") + 
  theme_classic()+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 13, color = "black")) +
  theme(text=element_text(size=10,  family="A"))


F1bb<-F1bb+scale_x_discrete(limits=c("Teens",
                                     "Twenties",
                                     "Thirties",
                                     "Forties",
                                     "Fifties",
                                     "Sixties",
                                     "Seventies(Over)")) +
  ggtitle("Overestimation") + 
  theme_classic()+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 13, color = "black")) +
  theme(text=element_text(size=10,  family="A"))

F1bb

ggarrange(F1aa,F1bb,ncol = 2, nrow = 1)

install.packages( "gcookbook")
library(gcookbook)
library(cowplot)
library(grid)
grid.newpage()
grid.draw(as_grob(abc))
plot_grid(aba,abc,ncol = 2, nrow = 1)




#비율그래프

be <- ddply(Figure1aa, "Group", transform, Count=Count/sum(Count)*100)
ggplot(ce, aes(x=Group, y=Count, fill=Body.image.perception),palette = c("#00AFBB", "#FC4E07"))+geom_bar(stat="identity",width=0.5) + 
  scale_x_discrete(limits=c("Teens", "Twenties",
                            "Thirties",
                            "Forties",
                            "Fifties",
                            "Sixties",
                            "Seventies(Over)"))+  ggtitle("Underestimation") + 
  theme_classic()+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 13, color = "black")) +
  theme(text=element_text(size=10,  family="A"))

ce <- ddply(Figure1bb, "Group", transform, Count=Count/sum(Count)*100)
ggplot(ce, aes(x=Group, y=Count, fill=Body.image.perception))+geom_bar(stat="identity") + 
  scale_x_discrete(limits=c("Teens", "Twenties",
                            "Thirties",
                            "Forties",
                            "Fifties",
                            "Sixties",
                            "Seventies(Over)"))+  ggtitle("Overestimation") + 
  theme_classic()+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 13, color = "black")) +
  theme(text=element_text(size=10,  family="A"))

#4그룹

names(Figure1a) <- c("Group","Body.image.perception","Count")
names(Figure1b) <- c("Group","Body.image.perception","Count")


be1 <- ddply(Figure1a, "Group", transform, Count=Count/sum(Count)*100)

ce1 <- ddply(Figure1b, "Group", transform, Count=Count/sum(Count)*100)

be1
F1a<-ggbarplot(be1,x="Group",y="Count",fill="Body.image.perception",palette = c("dimgray", "lightgray"),width=0.4)
F1a

F1b<-ggbarplot(ce1,x="Group",y="Count",fill="Body.image.perception",palette = c("dimgray", "lightgray"),width=0.4)
F1b

F1a<-F1a+scale_x_discrete(limits=c("Between 19 - 45", "Between 46 - 59","Above 60")) +
  ggtitle("Underestimated group") + 
  theme_classic()+
  #geom_bar(stat="identity",width=0.5) +
  coord_fixed(ratio = 0.025)+
  theme(legend.position = "none",legend.title=element_blank(),plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black"),
        axis.title.x=element_blank(),) +
  ylab("Proportion (%)")+
  theme(text=element_text(size=18,  family="A"))
F1a

F1b<-F1b+scale_x_discrete(limits=c("Between 19 - 45", "Between 46 - 59","Above 60")) +
  ggtitle("Overestimated group") + 
  theme_classic()+
  #geom_bar(stat="identity",width=0.5) +
  coord_fixed(ratio = 0.025)+
  theme(legend.position = c(1.1, 0.8),legend.title=element_blank(),plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black"),
        axis.title.x=element_blank(),) +
  ylab("Proportion (%)")+
  theme(text=element_text(size=18,  family="A"))
F1b

F1b<-F1b+scale_x_discrete(limits=c("Under 18", "Between 19 - 45", "Between 46 - 59","Above 60")) +
  ggtitle("Overestimation") + 
  theme_classic()+
  coord_fixed(ratio = 0.025)+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 14, color = "black")) +
  theme(text=element_text(size=1,  family="A"))
F1b

ggarrange(F1a,F1b,ncol = 3, nrow = 1)
ggarrange(F1c,ncol = 3, nrow = 1)  


#new

Figure1cc <- read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/Unoverall.csv",header=T,
                      stringsAsFactors = F)

names(Figure1cc) <- c("Group","Body.image.perception","Count")
de1 <- ddply(Figure1cc, "Group", transform, Count=Count/sum(Count)*100)


F1c<-ggbarplot(de1,x="Group",y="Count",fill="Body.image.perception",palette = c("white", "lightgray","dimgray"),width=0.4) 
F1c

F1c<-F1c+scale_x_discrete(limits=c("Between 19 - 45", "Between 46 - 59","Above 60")) +
  ggtitle("Total groups") + 
  theme_classic()+
  #geom_bar(stat="identity",width=0.5) +
  coord_fixed(ratio = 0.025)+
  theme(legend.position = c(1.15, 0.8),legend.title=element_blank(),plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black"),
        axis.title.x=element_blank(),) +
  ylab("Proportion (%)")+
  theme(text=element_text(size=18,  family="A"))
F1c

#F1c<-F1c+scale_x_discrete(limits=c("Under 18", "Between 19 - 45", "Between 46 - 59","Above 60")) +
ggtitle("Total") + 
  theme_classic()+
  coord_fixed(ratio = 0.05)+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  theme(text=element_text(size=16,  family="A"))
#F1c


Mingong1<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/대통합1군.csv", sep=",", head=TRUE, encoding="UTF-8")
Mingong2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/대통합2군.csv", sep=",", head=TRUE, encoding="UTF-8")
Mingong3<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/대통합3군.csv", sep=",", head=TRUE, encoding="UTF-8")
Mingong4<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/대통합4군.csv", sep=",", head=TRUE, encoding="UTF-8")


table(Mingong1$결혼여부)
table(Mingong2$결혼여부)
table(Mingong3$결혼여부)
table(Mingong4$결혼여부)

2694+6312


#여기서 부터 시작

Figure1dd <- read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/Forgraph.csv",header=T,
                      stringsAsFactors = F)

names(Figure1dd) <- c("Year","Body.image.perception","Count")
#de1 <- ddply(Figure1cc, "Year", transform, Count=Count/sum(Count)*100)

library(ggplot2)

Figure1dd

Figure1dd$Year<-as.character(Figure1dd$Year)

F1d<-ggbarplot(Figure1dd,x="Year",y="Count",fill="Body.image.perception",
               palette = c("white", "lightgray","dimgray"),
               horiz=TRUE,names.arg=c('2010','2011','2012','2013','2014','2015','2016'),width=0.4, position = position_dodge(0.4))
F1d

F1e<-ggline(Figure1dd,x="Year",y="Count",color="Body.image.perception",
            palette = c("lightgray","dimgray","black"),size = 1.5,point.size = 3,
            ylab = "count",)

F1e<-F1e+theme(legend.title=element_blank(),
               plot.title=element_text(size=20, color="blue"))+ggtitle("Yearly Body image perception")+theme_classic()+
  theme(legend.title=element_blank(),plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black"),
        axis.title.x=element_blank(),)+
  theme(text=element_text(size=20,  family="A"))+ylab("")

F1e




F1d<-F1d+ggtitle("") + 
  theme_classic()+
  theme(legend.title=element_blank(),plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black"),
        axis.title.x=element_blank(),)+
  theme(text=element_text(size=20,  family="A"))+ylab("")


F1d<-F1d+scale_x_discrete(limits=c("2010", "2011", "2012","2013","2014","2015","2016")) +
  ggtitle("Total groups") + 
  theme_classic()+
  #geom_bar(stat="identity",width=0.5) +
  coord_fixed(ratio = 0.025)+
  theme(legend.position = c(1.15, 0.8),legend.title=element_blank(),plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black"),
        axis.title.x=element_blank(),) +
  ylab("")+
  theme(text=element_text(size=14,  family="A"))
F1d

ggarrange(F1e,F1d,ncol = 1, nrow = 2)  
ggarrange(F1e,F1f,ncol = 1, nrow = 2)  

library(d)
library(ggpubr)

ee1 <- ddply(Figure1dd, "Year", transform, Count=Count/sum(Count)*100)

#F1f<-ggbarplot(ee1,x="Year",y="Count",fill="Body.image.perception",
#palette = c("white", "lightgray","dimgray"),
#horiz=TRUE,names.arg=c('2010','2011','2012','2013','2014','2015','2016'),width=0.4, position = position_dodge(0.4))

F1f<-ggbarplot(ee1,x="Year",y="Count",fill="Body.image.perception",palette = c("white", "lightgray","dimgray"),width=0.6) 
F1f

F1f<-F1f+
  ggtitle("Yearly Body image Perception") + 
  theme_classic()+
  #geom_bar(stat="identity",width=0.5) +
  #coord_fixed(ratio = 0.025)+
  theme(#legend.position = c(1.15, 0.8)#,legend.title=element_blank()
    plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 28, color = "black"),
    axis.title.x=element_blank(),legend.title=element_blank()) +
  coord_fixed(ratio = 0.08)+
  ylab("Proportion (%) of self body image ")+
  theme(axis.title.y = element_text(size=16),text=element_text(size=28,  family="A"))
F1f

#F1c<-F1c+scale_x_discrete(limits=c("Under 18", "Between 19 - 45", "Between 46 - 59","Above 60")) +
ggtitle("Total") + 
  theme_classic()+
  coord_fixed(ratio = 0.05)+
  theme(plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 20, color = "black")) +
  theme(text=element_text(size=20,  family="A"))
#F1c


F1g<-F1f+
  ggtitle("Yearly Body image Perception") + 
  theme_classic()+
  #geom_bar(stat="identity",width=0.5) +
  coord_fixed(ratio = 0.025)+
  theme(#legend.position = c(1.15, 0.8)#,legend.title=element_blank()
    plot.title = element_text(family = "A", face = "bold", hjust = 0.5, size = 28, color = "black"),
    axis.title.x=element_blank(), legend.title=element_blank()) +
  ylab("Proportion(%) of self body image ")+
  theme(axis.title.y = element_text(size=16),text=element_text(size=20,  family="A"))
F1g



