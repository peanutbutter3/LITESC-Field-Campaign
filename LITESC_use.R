Dust_files=list.files(path=file.path("LITESC","raw_data"),pattern=".*.html",full.names=TRUE,recursive=FALSE)
df=sub(".*#(\\d).*","\\1",Dust_files)
lapply(Dust_files[df=="4"],Dusttrak_csv,tz="America/Detroit")
lapply(Dust_files[df!="4"],Dusttrak_csv,tz="America/Chicago")

HOBO_files=list.files(path=file.path("LITESC","HOBO"),pattern=".*.csv",full.names=TRUE,recursive=FALSE)
lapply(HOBO_files,HOBO_csv)

buoy_files=list.files(path=file.path("LITESC","buoy"),pattern=".*.csv",full.names=TRUE,recursive=FALSE)
lapply(buoy_files,Buoy_csv)

########
Sys.setlocale("LC_TIME", "English")

buoy_mclain=read.csv(file.path("LITESC","buoy","fixed_buoy","buoydata_45023.csv"))
hobo_mclain=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241012&13_McLain.csv"))
plot_LL_temp_diff(hobo_mclain,buoy_mclain,"McLain",T)

buoy_lanse=read.csv(file.path("LITESC","buoy","fixed_buoy","buoydata_45025.csv"))
hobo_lanse=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241014&15_Lanse.csv"))
plot_LL_temp_diff(hobo_lanse,buoy_lanse,"Lanse",T)


########
hobo_swedetown_12=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241012_Swedetown.csv"))
hobo_swedetown_13=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241013_Swedetown.csv"))
plot_LL_temp_diff(hobo_swedetown_12,buoy_mclain,"Swedetown_12",T)
plot_LL_temp_diff(hobo_swedetown_13,buoy_mclain,"Swedetown_13",T)

hobo_lily=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241012_Lily.csv"))
plot_LL_temp_diff(hobo_lily,buoy_mclain,"Lily_12",T)

hobo_cliff_14=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241014_Cliff.csv"))
hobo_cliff_15=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241015_Cliff.csv"))
plot_LL_temp_diff(hobo_cliff_14,buoy_lanse,"Cliff_14",T)
plot_LL_temp_diff(hobo_cliff_15,buoy_lanse,"Cliff_15",T)

hobo_beach_14=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241014_Beach.csv"))
hobo_beach_15=read.csv(file.path("LITESC","HOBO","fixed_hobo","20241015_Beach.csv"))
hobo_beach_14=hobo_beach_14[!is.na(hobo_beach_14$Temperature),]
plot_LL_temp_diff(hobo_beach_14,buoy_lanse,"Beach_14",T)
plot_LL_temp_diff(hobo_beach_15,buoy_lanse,"Beach_15",T)

########

l_12=list(hobo_mclain,hobo_swedetown_12,hobo_lily)
plot_hobo_wind(l_12,c("McLain","Swedetown","Lily"),buoy_mclain,png=T)

l_13=list(hobo_mclain,hobo_swedetown_13)
plot_hobo_wind(l_13,c("McLain","Swedetown"),buoy_mclain,png=T)

l_14=list(hobo_lanse,hobo_cliff_14,hobo_beach_14)
plot_hobo_wind(l_14,c("Lanse","Cliff","Beach"),buoy_lanse,png=T)

l_15=list(hobo_lanse,hobo_cliff_15,hobo_beach_15)
plot_hobo_wind(l_15,c("Lanse","Cliff","Beach"),buoy_lanse,png=T)

########
