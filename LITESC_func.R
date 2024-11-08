rm(list=ls())

lib=c("rvest","lubridate","ggplot2","reshape2")
for(i in lib){
  if(!require(i,character.only = TRUE)){install.packages(i);require(i,character.only = TRUE)}
}

############################

#Dusttrak
Dusttrak_csv=function(path,tz="America/Chicago"){
  htable=path%>%read_html()%>%html_elements(".e5")%>%html_table()
  dust_dat=data.frame(htable[[1]])
  len=dim(dust_dat)[1]
  var_name=c("TimeUNIX","TimeUTC","Aerosol")
  dust_res=matrix(ncol=3,nrow=(len-1)/2)%>%data.frame()
  colnames(dust_res)=var_name
  for(i in 1:((len-1)/2)){
    dust_res[i,3]=dust_dat[i*2,4]
    date=dust_dat[i*2,2]
    month=sub("^(.*?)/.*","\\1",date)%>%as.numeric()
    day=sub(".*/(.*?)/.*","\\1",date)%>%as.numeric()
    year=sub(".*/(.*?)$","\\1",date)%>%as.numeric()
    time=dust_dat[i*2,3]
    hour=sub("^(.*?):.*","\\1",time)%>%as.numeric()
    min=sub(".*:(.*?):.*","\\1",time)%>%as.numeric()
    sec=sub(".*:(.*?)$","\\1",time)%>%as.numeric()
    tim=base::ISOdate(year,month,day,hour,min,sec,tz)%>%
      lubridate::with_tz(test,tzone="UTC")
    dust_res[i,1]=tim
    dust_res[i,2]=tim%>%as.character()
  }
  filename=sub("^(.*?).html$","\\1",basename(path))
  pathname=dirname(path)
  pathname=file.path(pathname,"fixed_dust")
  if(!file.exists(pathname)){dir.create(pathname)}
  write.csv(dust_res,file.path(pathname,paste0(filename,".csv")),row.names = FALSE)
  return(dust_res)
}


############################

#HOBO
HOBO_csv=function(path,tz="America/Detroit"){
  nam=c("TimeUnix","TimeUTC","SolarRadiation","Temperature","DewPoint",
        "RelativeHumidity","WindDirection","WindSpeed","GustSpeed",
        "Pressure")
  var=c("Solar Radiation","Temp","DewPt","RH","Wind Direction",
        "Wind Speed","Gust Speed","Pressure")
  hobo=read.csv(path,skip=1,header = F)
  res=matrix(ncol=10,nrow=dim(hobo)[1]-1)%>%data.frame()
  colnames(res)=nam
  var_order=NULL
  for(i in 3:dim(hobo)[2]){
    n=sub("^(.*?),.*","\\1",hobo[1,i])
    var_order=c(var_order,which(var==n)+2)
  }
  for(i in 2:(dim(hobo)[1])){
    tim=hobo[i,2]
    month=sub("(.*?)/.*","\\1",tim)%>%as.numeric()
    day=sub(".*/(.*?)/.*","\\1",tim)%>%as.numeric()
    year=sub(".*/(.*?) .*","\\1",tim)%>%as.numeric()+2000
    n=sub(".* (.*?)$","\\1",tim);hour=sub(".* (.*?):.*","\\1",tim)%>%as.numeric()
    hour=hour+ifelse(n=="PM"&hour!=12,12,0);if(hour==12 & n=="AM"){hour=0}
    min=sub(".*:(.*?):.*","\\1",tim)%>%as.numeric()
    sec=sub(".*:(.*?) .*","\\1",tim)%>%as.numeric()
    timeUTC=base::ISOdate(year,month,day,hour,min,sec,tz)%>%
      lubridate::with_tz(test,tzone="UTC")
    res[i-1,1]=timeUTC;res[i-1,2]=as.character(timeUTC)
    rowv=hobo[i,3:dim(hobo)[2]]
    res[i-1,var_order]=ifelse(as.numeric(rowv) < -800,NA,rowv)
  }
  filename=sub("^(.*?).csv$","\\1",basename(path))
  pathname=dirname(path)
  pathname=file.path(pathname,"fixed_hobo")
  if(!file.exists(pathname)){dir.create(pathname)}
  write.csv(res,file.path(pathname,paste0(filename,".csv")))
  return(res)
}

############################

#Buoy
Buoy_csv=function(path,tz="UTC",Trange=NA){
  buoy=read.csv(path)
  if(!is.na(Trange)){
    
  }else{
    buoy=buoy[3:(dim(buoy)[1]),]
  }
  nam=c("TimeUnix","TimeUTC","WDIR","WSPD","GST","WVHT","DPD","APD","MWD",
        "PRES","ATMP","WTMP","DEWP","VIS","PTDY","TIDE")
  len=dim(buoy)[1]
  buoy_res=matrix(nrow=len,ncol=16)%>%data.frame()
  colnames(buoy_res)=nam
  for(i in seq(len,1,by=-1)){
    timeUTC=base::ISOdate(buoy[i,1],buoy[i,2],
                          buoy[i,3],buoy[i,4],buoy[i,5],0,tz=tz)
    buoy_res[(len-i+1),1]=timeUTC
    buoy_res[(len-i+1),2]=as.character(timeUTC)
    buoy_res[(len-i+1),3:16]=buoy[i,6:19]
  }
  filename=sub("^(.*?).csv$","\\1",basename(path))
  pathname=dirname(path)
  pathname=file.path(pathname,"fixed_buoy")
  if(!file.exists(pathname)){dir.create(pathname)}
  write.csv(buoy_res,file.path(pathname,paste0(filename,".csv")))
  return(buoy_res)
}

############################

#temp difference between land and lake
LL_Temp_diff=function(land,lake){
  temp_diff=NULL
  for(i in 1:dim(land)[1]){
    tim=land$TimeUnix[i]
    temp_lake=lake[lake$TimeUnix<=tim+20*60&lake$TimeUnix>=tim-20*60,]$ATMP%>%mean()
    temp_land=land$Temperature[i]
    temp_diff=c(temp_diff,temp_land-temp_lake)
  }
  return(data.frame(time=land$TimeUnix,diff=temp_diff))
}


plot_LL_temp_diff=function(land,lake,loc,png=T){
  tz="America/Detroit"
  time_up=land$TimeUnix[1]-60
  time_low=land$TimeUnix[dim(land)[1]]+60
  lake=lake[lake$TimeUnix>=time_up&lake$TimeUnix<=time_low,]
  tmax=max(max(land$Temperature,na.rm=T),max(lake$ATMP,na.rm=T))
  tmin=min(min(land$Temperature,na.rm=T),min(lake$ATMP,na.rm=T))
  p1=ggplot(land,aes(x=TimeUnix%>%as.POSIXct(tz)))+
    geom_line(data=land,aes(y=Temperature,color="land_temp"),size=0.6)+
    geom_line(data=lake,aes(y=ATMP,color="lake_temp"),size=0.6)
  p1=p1+xlab("Time")+ylab("Temperature (degree C)")+ggtitle(paste0(loc,": Temp of Land and Lake"))+
    scale_y_continuous(breaks=seq(-50,50,by=1),limits=c(tmin-0.5,tmax+0.5))+
    scale_color_manual(values = c(land_temp= "red", lake_temp= "blue"))
  if(time_low-time_up>24*60*60){breaktime=6*60*60}else{breaktime=60*60}
  p1=p1+scale_x_datetime(breaks=seq(as.POSIXct("2024-10-10 11:00"),as.POSIXct("2024-10-30 12:00"),by=breaktime),
                             date_labels = "%b-%d %H:%M")
  
  if(png){png(filename = paste0("Temp_Land&Lake_",loc,".png"),width = 2400,height = 1800,res = 200)}
  plot(p1)
  if(png){dev.off()}
}
