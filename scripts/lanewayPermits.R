# lanewayPermits.R
# Get info on properties with laneway homes from permits file
# Tom Davidoff
# March 7, 2025

library(data.table)

df <- fread("data/raw/issued-building-permits.csv",select=c("TypeOfWork","SpecificUseCategory","IssueDate","Address","geo_point_2d"))
# make two variables, streetNumber and streetRest by splitting Address
df <- df[grepl("New Build",TypeOfWork)==TRUE]
df[,c("civicAddress",paste("a",1:2)):=tstrsplit(Address,",")]
df[,c("civicAddress","chum"):=tstrsplit(civicAddress," #")]
df[,c("dropper","postal"):=tstrsplit(Address,", BC ")]
df[,c("lat","lon"):=tstrsplit(geo_point_2d,",",fixed=TRUE)]
df[,laneway:=grepl("aneway",SpecificUseCategory)==TRUE]
df[,laneway:=max(laneway),by="civicAddress"]
df[,single:=grepl("Single Detached House",SpecificUseCategory)==TRUE]
df[,single:=max(single),by="civicAddress"]
df[,duplex:=grepl("Duplex",SpecificUseCategory)==TRUE]
df[,duplex:=max(duplex),by="civicAddress"]
print(summary(df[,duplex]))
fwrite(unique(df[,.(IssueDate,civicAddress,postal,lat,lon,laneway,single,duplex)]),"data/derived/lanewayPermitAddress.csv")