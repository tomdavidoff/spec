# mergeRoll20052016.R
# merge 2005 and 2016 rolls for Greater Vancouver
# Identify properties that were built in 2005 or later
# Note Tsur's conversion code from stata "Landcor Roll+Transactions 2005.do"
# Tom Davidoff
# 07/11/25

library(data.table)

d2005 <- fread("data/derived/Landcor Roll 2005.csv",select=c("juris_id","psh_rollnum","act_use","year_built","single","suite","value_stru05","value_land05"))
d2005[,numericRoll:=as.numeric(gsub("[^0-9]","",psh_rollnum))]
d2005[,rollStart:=substring(numericRoll,1,nchar(numericRoll)-1)]
setkey(d2005,rollStart,juris_id)
jurList <- unique(d2005[,juris_id])
print(jurList)
#d2005 <- d2005[single==1]

diri <- "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/"
dirf <- list.files(diri)

di25 <- data.table(Jurisdiction=numeric(),Roll_Number=character(),MB_Year_Built=numeric())
for (f in paste0(diri,dirf)) {
	d <- fread(f,select=c("Jurisdiction","Roll_Number","MB_Year_Built"),colClasses=c(Roll_Number="character",Jurisdiction="numeric") )
	di25 <- rbind(di25,d)
}
di25 <- di25[!is.na(MB_Year_Built)]
di25[,numericRoll:=as.numeric(gsub("[^0-9]","",Roll_Number))]
di25[,rollStart:=substring(numericRoll,1,nchar(numericRoll)-1)]
di25 <- di25[Jurisdiction %in% jurList]
# confine to single family and duplexes
# confine to single famil

dd25 <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("FOLIO_ID","ROLL_NUMBER","ACTUAL_USE_DESCRIPTION","JURISDICTION_CODE"))
useList <- c("Single Family Dwelling","Duplex, Strata Side by Side","Duplex, Strata Front / Back","Duplex, Strata Up / Down","Duplex, Non-Strata Side by Side or Front / Back","Duplex, Non-Strata Up / Down","Residential Dwelling with Suite")
dd25 <- dd25[ACTUAL_USE_DESCRIPTION %in% useList]
print(head(dd25))
dd25[,numericRoll:=as.numeric(gsub("[^0-9]","",ROLL_NUMBER))]
setnames(dd25,"JURISDICTION_CODE","Jurisdiction")
di25 <- merge(di25,dd25[,.(numericRoll,Jurisdiction,FOLIO_ID,ACTUAL_USE_DESCRIPTION)],by=c("numericRoll","Jurisdiction"))

# grab folio addresses file to see where missings are
d25add <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_addresses_20250331_REVD25.csv",select=c("FOLIO_ID","STREET_NUMBER","STREET_DIRECTION_PREFIX","STREET_NAME","STREET_TYPE","STREET_DIRECTION_SUFFIX","CITY"))
d25add[,address:=paste(STREET_NUMBER,STREET_DIRECTION_PREFIX,STREET_NAME,STREET_TYPE,STREET_DIRECTION_SUFFIX,sep=" ")]
di25 <- merge(di25,d25add[,.(FOLIO_ID,address)],by="FOLIO_ID",all.x=TRUE)


print(nrow(di25))
print(head(di25))
print(head(d2005))
setnames(d2005,"juris_id","Jurisdiction")

di25 <- merge(di25,d2005,by=c("rollStart","Jurisdiction"),all.x=TRUE,all.y=TRUE)
print(nrow(di25))
print(head(di25))
print(nrow(di25))
print(table(di25[,.(Jurisdiction,is.na(year_built))]))
print(table(di25[,.(Jurisdiction,is.na(MB_Year_Built))]))
print(table(di25[Jurisdiction==200,is.na(year_built)]))
print(table(di25[Jurisdiction==200,is.na(MB_Year_Built)]))
print(table(di25[Jurisdiction==200,single,is.na(MB_Year_Built)]))
# drop non-singles from 2005 roll
print(summary(di25))
di25 <- di25[single==1 & !is.na(single) & !is.na(MB_Year_Built)]
print(summary(di25[,year_built==MB_Year_Built]))
di25[,structureLand:=value_stru05/value_land05]
summary(di25[MB_Year_Built==year_built,structureLand])
summary(di25[MB_Year_Built!=year_built,structureLand])
# are there mess ups of *older*
print(summary(di25[MB_Year_Built!=year_built,MB_Year_Built]))
print(summary(di25[MB_Year_Built!=year_built,year_built]))
print(di25[MB_Year_Built!=year_built & MB_Year_Built<year_built,.(psh_rollnum,Roll_Number)])
print(summary(di25[MB_Year_Built!=year_built & (as.numeric(psh_rollnum)==as.numeric(Roll_Number)),.(MB_Year_Built,year_built)]))
print(table(di25[MB_Year_Built<2005 & MB_Year_Built!=year_built,ACTUAL_USE_DESCRIPTION]))
print(table(di25[MB_Year_Built<2005 & MB_Year_Built!=year_built & (as.numeric(psh_rollnum)==as.numeric(Roll_Number)),MB_Year_Built-year_built]))
di25[,duplex:=grepl("Duplex",ACTUAL_USE_DESCRIPTION)]
di25 <- di25[as.numeric(psh_rollnum)==as.numeric(Roll_Number) | duplex]
print("Post-purge")
print(summary(di25[MB_Year_Built!=year_built & (as.numeric(psh_rollnum)==as.numeric(Roll_Number)),.(MB_Year_Built,year_built)]))
print(summary(di25))
print(summary(di25[duplex==1 ,.(MB_Year_Built)]))
print(summary(di25[MB_Year_Built==year_built,.(structureLand,year_built)]))
print(summary(di25[MB_Year_Built!=year_built,.(structureLand,year_built)]))
fwrite(di25[MB_Year_Built>2005,.(Jurisdiction,Roll_Number,MB_Year_Built,value_stru05,FOLIO_ID)],"data/derived/newBuilds.csv")
