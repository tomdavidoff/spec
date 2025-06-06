#disappeared.R
# Single family lots that aren't there any more but are duplexes
# Generally get 2024 status of 2016 lots; confine to 30' or 50' by 120 and have laneway
# Tom Davidoff
# March 17, 2025

# 2024/5 disappeared lots -- what is the current use?
library(data.table)
d24 <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_descriptions_20240331_REVD24.csv",select=c("FOLIO_ID","ROLL_NUMBER","ACTUAL_USE_DESCRIPTION","JURISDICTION_CODE","NEIGHBOURHOOD"),colClasses=c(ROLL_NUMBER="character"))
d24 <- d24[JURISDICTION_CODE==200]
d24[,nchar:=nchar(as.character(ROLL_NUMBER))]
d24[,splitspot:=nchar-4]
d24[,rollStart:=substr(ROLL_NUMBER,1,splitspot)]
d24[,rollEnd24:=substr(ROLL_NUMBER,splitspot+1,nchar)]

dI24 <- fread("~/docs/data/bca/Residential_inventory_202401/20240101_AA09_Residential_Inventory_Extract.txt",select=c("Jurisdiction","Roll_Number","MB_Year_Built","Zoning"),colClasses=c(Roll_Number="character"))
dI24 <- dI24[Jurisdiction==200]
d24 <- merge(d24,dI24,by.x="ROLL_NUMBER",by.y="Roll_Number")
# rename to ROLL_NUMBER24
d24[,ROLL_NUMBER24:=ROLL_NUMBER]
d24[,ROLL_NUMBER:=NULL]

da <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_addresses_20240331_REVD24.csv",select=c("FOLIO_ID","UNIT_NUMBER","STREET_NUMBER","STREET_DIRECTION_PREFIX","STREET_NAME","STREET_TYPE","STREET_DIRECTION_SUFFIX","POSTAL_CODE"),colClasses=c(FOLIO_ID="character"))
da[,fullStreet24:=paste(STREET_NUMBER,STREET_DIRECTION_PREFIX,STREET_NAME,STREET_TYPE,STREET_DIRECTION_SUFFIX,UNIT_NUMBER)]
da[,streetNoNumber24:=paste(STREET_DIRECTION_PREFIX,STREET_NAME,STREET_TYPE,STREET_DIRECTION_SUFFIX)]
da[,streetNumber24:=as.numeric(STREET_NUMBER)]

d24 <- merge(d24,da,by="FOLIO_ID")
d24 <- d24[Zoning %in% c("RS1","RS2","RS3","RS3A","RS5","RS6","RS7")]
fwrite(d24,"data/derived/use24.csv")
