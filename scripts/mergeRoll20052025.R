# mergeRoll20052025
# merge 2005 and 2025 rolls for Greater Vancouver
# Identify properties that were built in 2005 or later
# Note Tsur's conversion code from stata "Landcor Roll+Transactions 2005.do"
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
	d <- d[Jurisdiction %in% jurList & !is.na(MB_Year_Built) ]
	if (nrow(d)>0) {
		di25 <- rbind(di25,d)
	}
}
di25 <- di25[!is.na(MB_Year_Built)]
di25[,numericRoll:=as.numeric(gsub("[^0-9]","",Roll_Number))]
di25[,rollStart:=substring(numericRoll,1,nchar(numericRoll)-1)]
# confine to single family and duplexes

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
# drop non-singles from 2005 roll
di25 <- di25[single==1 & !is.na(single) & !is.na(MB_Year_Built)]
# get ratio from 2005 as moment to match with spec indicator
di25[,structureLand:=value_stru05/value_land05]
# are there mess ups of *older*
print(di25[MB_Year_Built!=year_built & MB_Year_Built<year_built,.(psh_rollnum,Roll_Number)])
print(summary(di25[MB_Year_Built!=year_built & (as.numeric(psh_rollnum)==as.numeric(Roll_Number)),.(MB_Year_Built,year_built)]))
di25[,duplex:=grepl("Duplex",ACTUAL_USE_DESCRIPTION)]
di25 <- di25[as.numeric(psh_rollnum)==as.numeric(Roll_Number) | duplex]
# only keep first post-2005 by 2005 roll number
di25[,minNew:=min(MB_Year_Built+3000*(MB_Year_Built<=2005),na.rm=TRUE),by=.(Jurisdiction,psh_rollnum)]
di25 <- di25[MB_Year_Built==minNew | MB_Year_Built<=2005]
di25 <- unique(di25[,.(Jurisdiction,Roll_Number,MB_Year_Built,year_built,value_stru05,value_land05,FOLIO_ID)])
fwrite(di25,"data/derived/newBuilds.csv")
