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
df <- unique(data.table(dI))
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
d24 <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_descriptions_20240331_REVD24.csv",select=c("FOLIO_ID","ROLL_NUMBER","ACTUAL_USE_DESCRIPTION","JURISDICTION_CODE"),colClasses=c(ROLL_NUMBER="character"))
d24 <- d24[JURISDICTION_CODE==200]
#dI <- dI[Zoning=="R1-1" & Jurisdiction==200]
d24[,nchar:=nchar(as.character(ROLL_NUMBER))]
d24[,splitspot:=nchar-4]
d24[,rollStart:=substr(ROLL_NUMBER,1,splitspot)]
d24[,rollEnd24:=substr(ROLL_NUMBER,splitspot+1,nchar)]

dI24 <- fread("~/docs/data/bca/Residential_inventory_202401/20240101_AA09_Residential_Inventory_Extract.txt",select=c("Jurisdiction","Roll_Number","MB_Year_Built","Zoning"),colClasses=c(Roll_Number="character"))
dI24 <- dI24[Jurisdiction==200]
d24 <- merge(d24,dI24,by.x="ROLL_NUMBER",by.y="Roll_Number")
d24[,ROLL_NUMBER:=NULL]

da <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_addresses_20240331_REVD24.csv",select=c("FOLIO_ID","STREET_NUMBER","STREET_DIRECTION_PREFIX","STREET_NAME","STREET_TYPE","STREET_DIRECTION_SUFFIX","POSTAL_CODE"),colClasses=c(FOLIO_ID="character"))
da[,fullStreet24:=paste(STREET_NUMBER,STREET_DIRECTION_PREFIX,STREET_NAME,STREET_TYPE,STREET_DIRECTION_SUFFIX)]
da[,streetNoNumber24:=paste(STREET_DIRECTION_PREFIX,STREET_NAME,STREET_TYPE,STREET_DIRECTION_SUFFIX)]
da[,streetNumber24:=as.numeric(STREET_NUMBER)]

# GET YEAR BUILT!!

d24 <- merge(d24,da,by="FOLIO_ID")

# merge with 2016
df <- merge(df,d24,by="rollStart",all.x=TRUE) # note: merge on interstitial address almost never happens
dfx <- df[is.na(rollEnd24),]
df <- df[!is.na(rollEnd24),]
dfx <- merge(dfx,d24,by.x="fullStreet",by.y="fullStreet24")

df <- df[,.(FOLIO_ID,ROLL_NUMBER,MB_year_built,MB_Year_Built,actualUseDescription,ACTUAL_USE_DESCRIPTION)]
dfx <- dfx[,.(FOLIO_ID,ROLL_NUMBER,MB_year_built,MB_Year_Built,actualUseDescription,ACTUAL_USE_DESCRIPTION)]
df <- rbind(df,dfx)


if (2>3) {
df[,streetNumber16:=as.numeric(streetNumber16)]
df[,streetNumber24:=as.numeric(streetNumber24)]
df[,evenStreet:=streetNumber16%%2==0]
df[,evenStreet24:=streetNumber24%%2==0]

# find street numbers that are missing in 2024
dfk <- df[is.na(streetNumber24 | is.na(streetNumber)),]
print(table(dfk[,fullStreet %in% d24[fullStreet24]])) # always false

dfk[,lowestHigherNeighbour:=0]
outcomes <- data.table(type=character())
for (i in 1:nrow(dfk)) {
    # get upper and lower limits of addresses that aren't this particular one
    if (is.na(dfk[i,streetNumber24])) {
        print(i)
        thisStreet<- dfk[i,streetNoNumber][1]
        thisNumber<- dfk[i,streetNumber16][1]
        thisEven<- dfk[i,evenStreet][1]
        thisAddress <- dfk[i,fullStreet][1]
        print(dfk[i,fullStreet])
        consideration <- df[streetNoNumber==thisStreet & evenStreet==thisEven]
        print(consideration[,.N])
        consideration[,lowestHigherNeighbour:=min(999999*(streetNumber16<=thisNumber)+(streetNumber16),na.rm=TRUE)]
        consideration[,highestLowerNeighbour:=max((streetNumber16<=thisNumber)*streetNumber16,na.rm=TRUE)]
        print(c(thisStreet,thisNumber,consideration[1,lowestHigherNeighbour],consideration[1,highestLowerNeighbour]))
        consideration[,replacement:= streetNumber24<lowestHigherNeighbour & streetNumber24>highestLowerNeighbour]
        print(consideration[replacement==TRUE,.(streetNumber24)])
        print(df[fullStreet24==thisAddress,.(fullStreet24,streetNoNumber24)])
        outcome <- "neither"
        if (sum(consideration[,replacement]>0,na.rm=TRUE)>0) {
            if (sum(df[,fullStreet24==thisAddress],na.rm=TRUE)>0) {
                outcome  <- "both"
            } 
            else {
                    outcome <- "replacement"
                }
        }
        else {
            if (sum(df[,fullStreet24==thisAddress],na.rm=TRUE)>0) {
                outcome <- "address"
            }
        }
    outcomes <- rbind(outcomes,data.table(type=outcome))
    }
}

print(table(outcomes[,type]))
}