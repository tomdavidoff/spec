// Merge Laneway data to roll


cd /href/research3/laneway/data

// past data from BCA2021 Dropbox
use Laneways2009_2018, clear
destring act_use, replace
ren (year_built_lane eff_year_lane floorarea_lane found_area_lane man_class_lane) (lw_year_built lw_eff_year lw_floorarea lw_slabarea lw_man_class_desc)
drop act_use psh_rollnum haslaneway struc_count
save laneway0918, replace

use "Laneway list 2015-23.dta", clear
destring jur, gen(juris_id)
destring actual_use_code, gen(act_use)
ren (year_built effective_year total_finished_area slab_area manual_class_desc) (lw_year_built lw_eff_year lw_floorarea lw_slabarea lw_man_class_desc)
drop jur actual_use_code area rollyear
save laneway1523, replace

append using laneway0918
sort roll_num str_no
by roll_num: replace str_no=str_no[_n+1] if missing(str_no)
by roll_num: replace str_name=str_name[_n+1] if missing(str_name)
by roll_num: replace civic_address=civic_address[_n+1] if missing(civic_address)
by roll_num: drop if _n>1 // dropping duplicates, keeping more recent information

save laneway0923, replace

use /href/research3/bca/data/BCA2016, clear
cap drop man_class man_class_desc
gen roll_year = 2016
keep if juris_id==200
replace neigh_name=proper(neigh_name)
replace neigh_name = "Arbutus/Mackenzie Heights" if neigh_name == "Arbutus Ridge - Mackenzie Heights"
replace neigh_name = "Main/Fraser" if neigh_name == "Main & Fraser"
replace neigh_name = "Coal Harbour" if neigh_name == "Harbour"
save vanroll1624, replace
forvalues y=2017(1)2024{
	use /href/research3/bca/data/BCA`y', clear
	cap rename neighbourhood neigh_name
	cap drop man_class man_class_desc plumbing_nil_flag
	gen roll_year = `y'
	*count if inlist(act_use, 0, 32) & juris_id==200 & (conveyance_type>6 & conveyance_type<=8) & year>=2012
	keep if juris_id==200
	replace neigh_name=proper(neigh_name)
	replace neigh_name = "Arbutus/Mackenzie Heights" if neigh_name == "Arbutus Ridge - Mackenzie Heights"
	replace neigh_name = "Main/Fraser" if neigh_name == "Main & Fraser"
	replace neigh_name = "Coal Harbour" if neigh_name == "Harbour"
	append using vanroll1624
	compress
	save vanroll1624, replace
}

use vanroll1624, clear
tostring pid, replace
gsort juris_id psh_rollnum year_built -roll_year // duplicates drop keeps only the first, and I want the most recent roll
duplicates drop juris_id psh_rollnum year_built, force
merge m:1 juris_id roll_num using /href/research3/Filtering/data/laneway0923
drop if _merge==2
drop _merge
cap drop foliostatus juris_desc alrdescription tenure_desc _merge
order juris_id roll_num
save vanroll_laneway, replace

use vanroll_laneway, clear
merge m:m juris_id psh_rollnum using /href/research3/bca/data/sales7524, update replace keep(1 3 4 5) nogen // shockingly not a mistake. Since the roll is about the house, there are sometimes multiple year_built homes per roll_num. But sales are just by the lot. So we use m:m and then drop any where the sale occurred before the house was built. I use replace because Tsur's data 7518 has more accurate year_built and eff_year info
drop if year<year_built
duplicates drop roll_num year month price, force
gen haslaneway = lw_eff_year<=year & !missing(lw_year_built) if !missing(year) // mark as having laneway if the laneway was built before the sale. Mark missing if no sale

// make this work with andrey's data
tostring act_use, replace format("%03.0f")
gen west = (neigh_name=="Point Grey" | neigh_name=="Kitsilano" | neigh_name=="Dunbar" | neigh_name=="Arbutus/Mackenzie Heights" | neigh_name=="Kerrisdale")
replace zoning = "One Family Dwelling" if regexm(zoning, "RS") | zoning=="R1-1"
keep if zoning == "One Family Dwelling"
keep if single==1
keep if year>2011 // some kind of problem with the 23/24 rolls that needs to be fixed to keep 2022 and 2023 sales, just using Andrey's sales for now. Was hoping that would duplicate out but there's a mismatch
gen age = year - eff_year if !missing(year) // lots of negative ages should probably be dropped
replace age = 0 if age<0
*gen eff_age = year - eff_year if !missing(year)
gen new = year - year_built <= 5
gen andrey=0
gen lprice = ln(price)

// identify spec (sold year of build or year after)
gen spec = (lw_eff_year==year | lw_eff_year==(year-1)) & !missing(lw_year_built)

// need to normalize floor area to exclude basement
gen floor_area = floorarea - base_finish_area
replace floor_area = floorarea - base_area if missing(base_finish_area) & !missing(base_area)
replace floor_area = floorarea if missing(base_area) & missing(base_finish_area) & !missing(floorarea)
replace floor_area = floorarea_1st + floorarea_2nd + floorarea_3rd if missing(floor_area) & missing(base_area) & missing(base_finish_area)
replace floor_area = floor_area/1000
replace lot_size = lot_size/1000
replace price = price/1000

// 2012-present, winsorize
drop if year<2012 | missing(year)
winsor2 price floorarea lot_size, replace cuts(0.5 99.5)

save vanroll_lw_sales, replace
