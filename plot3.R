data<-NULL

toFrame<-function(direc=getwd()){
  library(data.table)
  library(dplyr)
  library(magrittr)
  retrieve<-function(direc){
    fn<-"household_power_consumption.txt"
    first100<-fread(fn,sep=";",na.strings="?",nrows=100)
    classes<-sapply(first100,class)
    fulltable<-fread(fn,sep=";",na.strings="?",colClasses = classes)
    tbl<-tbl_df(fulltable)
    return(tbl)
  }
  if(is.null(data)){
    data<<-retrieve(direc)
  }
  data<-data%>%mutate(DateTime=paste(Date,Time))%>%select(-c(Date,Time))
  n<-ncol(data)
  data<-data[,c(n,1:(n-1))]
  data$DateTime<-strptime(data$DateTime,format="%d/%m/%Y %H:%M:%S")
  
  df<-as.data.frame(data)
  df$DateTime<-as.POSIXct(df$DateTime)
  df<-tbl_df(df)
  
  
  df<-df%>%filter(year(df$DateTime)==2007)
  df<-df%>%filter(month(df$DateTime)==2)
  df<-df%>%filter(mday(df$DateTime)==1|mday(df$DateTime)==2)
  return(df)
}

q3<-function(df=toFrame(getwd())){
  png(filename="plot3.png",width=480,height=480)
  par("mfrow"=c(1,1))
  with(df,plot(DateTime,Sub_metering_1,ylab="Energy sub metering",type="l",xlab=""))
  with(df,points(DateTime,Sub_metering_2,type="l",col="red"))
  with(df,points(DateTime,Sub_metering_3,type="l",col="blue"))
  legend("topright",lty=1,col=c("black","red","blue"),
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
}



