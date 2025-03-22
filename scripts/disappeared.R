#disappeared.R
# Single family lots that aren't there any more but are duplexes
# Tom Davidoff
# March 17, 2025

library(data.table)
library(RSQLite)


# 2016 lots, see which have disappeared and become duplexes

# read from sqlite3 file "~/docs/data/bca/REVD16_and_inventory_extracts.sqlite3" "residentialInventory", get fields: RollNumber, zoning, land_width, land_depth, MB_year_built; and select on  jurisdiction=="City of Vancouver"
# open connection first
con <- dbConnect(RSQLite::SQLite(), "~/docs/data/bca/REVD16_and_inventory_extracts.sqlite3")
dI <- dbGetQuery(con, "SELECT  roll_number, zoning, land_width, land_depth, MB_year_built FROM residentialInventory WHERE jurisdiction=='200' AND (zoning=='RS1' OR zoning=='RS2' OR zoning=='RS3' OR zoning=='RS3A' OR zoning=='RS4' OR zoning=='RS5' OR zoning=='RS6' OR zoning=='RS7' )")

da <- dbGetQuery(con, "SELECT folioID,streetNumber,streetDirectionPrefix,streetName,streetType,streetDirectionSuffix,postalCode,city FROM address ")

df <- dbGetQuery(con,"SELECT folioID,rollNumber from folio")
da <- merge(da,df,by="folioID")

dfd <- dbGetQuery(con,"SELECT folioID, actualUseDescription from folioDescription")
da <- merge(da,dfd,by="folioID")


dI <- merge(dI,da,by.x="roll_number",by.y="rollNumber")

# close connection
dbDisconnect(con)
df <- data.table(dI) 
df[,streetNumber16:=as.numeric(streetNumber)]
df[,fullStreet:=paste(streetNumber,streetDirectionPrefix,streetName,streetType,streetDirectionSuffix)]
df[,streetNoNumber:=paste(streetDirectionPrefix,streetName,streetType,streetDirectionSuffix)]
print(table(df[,actualUseDescription]))
df <- df[actualUseDescription=="Single Family Dwelling" | actualUseDescription=="Residential Dwelling with Suite",]
print(summary(df))

df[,nchar:=nchar(as.character(roll_number))]
df[,splitspot:=nchar-4]
df[,rollStart:=substr(roll_number,1,splitspot)]
df[,rollEnd:=substr(roll_number,splitspot+1,nchar)]
print(df[1:10,.(roll_number,rollStart,rollEnd)])


# 2024/5 disappeared lots -- what is the current use?
d24 <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_descriptions_20240331_REVD24.csv",select=c("FOLIO_ID","ROLL_NUMBER","ACTUAL_USE_DESCRIPTION","JURISDICTION_CODE"))
d24 <- d24[JURISDICTION_CODE==200]
#dI <- dI[Zoning=="R1-1" & Jurisdiction==200]
d24[,nchar:=nchar(as.character(ROLL_NUMBER))]
d24[,splitspot:=nchar-4]
d24[,rollStart:=substr(ROLL_NUMBER,1,splitspot)]
d24[,rollEnd24:=substr(ROLL_NUMBER,splitspot+1,nchar)]
d24[,ROLL_NUMBER:=NULL]

da <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_addresses_20240331_REVD24.csv",select=c("FOLIO_ID","STREET_NUMBER","STREET_DIRECTION_PREFIX","STREET_NAME","STREET_TYPE","STREET_DIRECTION_SUFFIX","POSTAL_CODE"),colClasses=c(FOLIO_ID="character"))
da[,fullStreet24:=paste(STREET_NUMBER,STREET_DIRECTION_PREFIX,STREET_NAME,STREET_TYPE,STREET_DIRECTION_SUFFIX)]
da[,streetNoNumber:=paste(STREET_DIRECTION_PREFIX,STREET_NAME,STREET_TYPE,STREET_DIRECTION_SUFFIX)]
da[,streetNumber24:=as.numeric(STREET_NUMBER)]

# GET YEAR BUILT!!

d24 <- merge(d24,da,by="FOLIO_ID")

# merge with 2016
df <- merge(df,d24,by="rollStart",all.x=TRUE)

# find street numbers that are missing in 2024
dfk <- df[is.na(streetNumber24),]
print(table(dfk[,fullStreet %in% d24[i,fullStreet24]]))
