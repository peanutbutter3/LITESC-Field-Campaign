Dust_files=list.files(path=file.path("LITESC","raw_data"),pattern=".*.html",full.names=TRUE,recursive=FALSE)
df=sub(".*#(\\d).*","\\1",Dust_files)
lapply(Dust_files[df=="4"],Dusttrak_csv,tz="America/Detroit")
lapply(Dust_files[df!="4"],Dusttrak_csv,tz="America/Chicago")

HOBO_files=list.files(path=file.path("LITESC","HOBO"),pattern=".*.csv",full.names=TRUE,recursive=FALSE)
lapply(HOBO_files,HOBO_csv)

buoy_files=list.files(path=file.path("LITESC","buoy"),pattern=".*.csv",full.names=TRUE,recursive=FALSE)
lapply(buoy_files,Buoy_csv)

########

buoy_mclain=read.csv(file.path("LITESC","buoy","fixed_buoy","buoydata_45023.csv"))
hobo_mclain=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241012&13_McLain.csv"))
plot_LL_temp_diff(hobo_mclain,buoy_mclain,"McLain",F)

buoy_lanse=read.csv(file.path("LITESC","buoy","fixed_buoy","buoydata_45025.csv"))
hobo_lanse=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241014&15_Lanse.csv"))
plot_LL_temp_diff(hobo_lanse,buoy_lanse,"Lanse",F)


########
hobo_swedetown_12=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241012_Swedetown.csv"))
hobo_swedetown_13=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241013_Swedetown.csv"))
hobo_swedetown=rbind(hobo_swedetown_12,hobo_swedetown_13)
plot_LL_temp_diff(hobo_swedetown_12,buoy_mclain,"Swedetown",F)

hobo_lily=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241012_Lily.csv"))
plot_LL_temp_diff(hobo_lily,buoy_mclain,"Lily",F)

########
