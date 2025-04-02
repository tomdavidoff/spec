#disappeared.R
# Single family lots that aren't there any more but are duplexes
# Generally get 2024 status of 2016 lots; confine to 30' or 50' by 120 and have laneway
# Tom Davidoff
# March 17, 2025

WIDTH_TOLERANCE <- 2 # 2 foot tolerance around tolerated lot sizes
DEPTH_TOLERANCE <- 4 # 2 foot tolerance around tolerated lot sizes

library(data.table)
library(RSQLite)


# 2016 lots, see which have disappeared and become duplexes

# read from sqlite3 file "~/docs/data/bca/REVD16_and_inventory_extracts.sqlite3" "residentialInventory", get fields: RollNumber, zoning, land_width, land_depth, MB_year_built; and select on  jurisdiction=="City of Vancouver"

# open connection first
con <- dbConnect(RSQLite::SQLite(), "~/docs/data/bca/REVD16_and_inventory_extracts.sqlite3")
dI <- dbGetQuery(con, "SELECT  roll_number, zoning, land_width, land_depth, MB_year_built FROM residentialInventory WHERE jurisdiction=='200' AND (zoning=='RS1' OR zoning=='RS2' OR zoning=='RS3' OR zoning=='RS3A' OR zoning=='RS4' OR zoning=='RS5' OR zoning=='RS6' OR zoning=='RS7' )")
dI <- data.table(dI)
print(summary(dI[,as.numeric(land_depth)]))
dI[,thirty:=land_width<=33+WIDTH_TOLERANCE & land_width>=33-WIDTH_TOLERANCE & land_depth<=122+2*DEPTH_TOLERANCE & land_depth>=122-2*DEPTH_TOLERANCE]
dI[,fifty:=land_width<=50+WIDTH_TOLERANCE & land_width>=50-WIDTH_TOLERANCE & land_depth<=122+2*DEPTH_TOLERANCE & land_depth>=122-2*DEPTH_TOLERANCE]
print(table(dI[,.(thirty,fifty)]))
dI <- dI[thirty==TRUE | fifty==TRUE]


da <- dbGetQuery(con, "SELECT folioID,streetNumber,streetDirectionPrefix,streetName,streetType,streetDirectionSuffix,postalCode,city FROM address ")

df <- dbGetQuery(con,"SELECT folioID,rollNumber from folio")
da <- merge(da,df,by="folioID")

dfd <- dbGetQuery(con,"SELECT folioID, actualUseDescription from folioDescription")
da <- merge(da,dfd,by="folioID")
dv <- dbGetQuery(con, "SELECT folioID, improvementValue FROM valuation")
da <- merge(da,dv,by="folioID")


dI <- merge(dI,da,by.x="roll_number",by.y="rollNumber")
print("IS IT A DATATABLE?")
print(is.data.table(dI))
#print(table(dI[,streetType]))
#AVE   CRES DIVERS     DR    HWY     PL     RD     ST 
# print(table(dI[,streetDirectionPrefix])) empty
#print(table(dI[,streetDirectionSuffix]))        E    N   SE   SW    W 

dI[,streetTypeForLaneway:=ifelse(streetType=="AVE","AV",ifelse(streetType=="CRES","CRESCENT",ifelse(streetType=="DR","DRIVE",ifelse(streetType=="PL","PLACE",ifelse(streetType=="RD","ROAD",streetType)))))]
dI[,addressForLaneway:=trimws(paste(streetNumber,streetDirectionSuffix,streetName,streetTypeForLaneway))]
dI[,addressForLaneway:=gsub("  "," ",addressForLaneway)]

dLaneOk <- fread("data/derived/lanewaySpatial.csv")
dLaneOk[,addressForLaneway:=trimws(paste(streetNumber,streetName))]
dLaneOk[,streetNumber:=NULL]
dLaneOk[,streetName:=NULL]
dI <- merge(dI,dLaneOk,by="addressForLaneway",all.x=TRUE,all.y=TRUE)
# deal with NA laneok approximation
print("IS IT A DATATABLE?")
print(is.data.table(dI))
dI[,madeupLaneok:=is.na(laneok)]
dI[,roundNumber:=floor(as.numeric(streetNumber)/100)]
dI[,meanLaneok:=mean(laneok,na.rm=TRUE),by=roundNumber]
dI[is.na(laneok) & meanLaneok==0,laneok:=0]
print(table(dI[,.(is.na(laneok),streetType)]))
dI[is.na(laneok) & meanLaneok==1,laneok:=1]
print(table(dI[,.(is.na(laneok),streetType)]))
print(table(dI[,actualUseDescription]))
dI <- dI[actualUseDescription== "Single Family Dwelling" | actualUseDescription=="Residential Dwelling with Suite"]
print(names(dI))

fwrite(dI[,.(folioID,roll_number,MB_year_built,improvementValue,streetNumber,streetDirectionPrefix,streetName,streetType,streetDirectionSuffix,postalCode,city,thirty,fifty,laneok)],"data/derived/baseline16.csv")
