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

dI <- merge(dI,da,by.x="roll_number",by.y="rollNumber")
# close connection
dbDisconnect(con)
df <- data.table(dI) 
print(summary(df))

df[,nchar:=nchar(as.character(roll_number))]
df[,splitspot:=nchar-4]
df[,rollStart:=substr(roll_number,1,splitspot)]
df[,rollEnd:=substr(roll_number,splitspot+1,nchar)]
print(df[1:10,.(roll_number,rollStart,rollEnd)])


# 2024/5 disappeared lots
dI <- fread("~/docs/data/bca/Residential_inventory_202401/20240101_AA09_Residential_Inventory_Extract.txt",select=c("Roll_Number","MB_Year_Built","Zoning","Jurisdiction"),colClasses=c(Roll_Number="character"))
#dI <- dI[Zoning=="R1-1" & Jurisdiction==200]
dI[,nchar:=nchar(as.character(Roll_Number))]
dI[,splitspot:=nchar-4]
dI[,rollStart:=substr(Roll_Number,1,splitspot)]
dI[,rollEnd25:=substr(Roll_Number,splitspot+1,nchar)]
dI[,Roll_Number:=NULL]

# merge with 2016
df <- merge(df,dI,by.x="rollStart",by.y="rollStart",all.x=TRUE)
# note 1811 don't work. Try address trick
df[,fullStreet:=paste(streetNumber,streetDirectionPrefix,streetName,streetType,streetDirectionSuffix)]