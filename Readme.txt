#### Main analyses #############################################

#### div_sta_area_neon/fish.csv: diversity & stability scaling exponents and general information of 31 NEON sites/47 RivfishTIME basins ####

#region: site ID for NEON dataset; basin ID for RivfishTIME dataset
#duration:temporal duration of study period 
#start_year/end_year: starting/end year of study period 
#sstar_slope: log-log slope of the average species stability-area relationship
#asar_slope: log-log slope of  the species asynchrony-area relationship
#estar_slope: log-log slope of the ecosystem stability-area relationship 
#sar_ave_slope: log-log slope of the species-area relationship
#mp_dist: the mean pairwise distance between plots/locations within focal site/basin 
#mnn_dist: the mean nearest neighbor spatial distance  between plots/locations within focal site/basin
#N: number of  plots/locations within focal site/basin 
#MAT_C: mean annual temperature, C
#MAP_mm: mean annual precipitation, mm
#MAT_sd: temperature variation; temporal standard deviation of annual temperature (
#MAP_sd: precipitation variation;temporal standard deviation of annual precipitation   

#### div_sta_area_full_log2_neon.csv: diversity & stability at each area of 31 NEON sites/47 RivfishTIME basins ######
#region: site ID for NEON dataset; basin ID for RivfishTIME dataset
#area: number of plots for NEON dataset; number of sampling locations for RivfishTIME dataset
#spe_sta:the average species stability
#spe_asyn：species asynchrony
#com_sta: ecosystem stability
#sr: species richness

#### site_info_main_neon/fish.csv ######
##shared variables
#Latitude/Longitude: coordinates of sites/locations, WGS 84 based
#time:  years in study period
#start/end_year : starting/end year of study period 

## additional variables in  site_info_main_fish.csv 
#Basin: hydrobasin ID
#location: sampling location ID
#continent/country: the continent/country of focal site

## additional variables in  site_info_main_neon.csv 
#Site: NEON site ID
#plot:  NEON plot ID

#### Supplementary analyses  #############################################

div_sta_area_neon/fish_5year: contaning the same variables as in  div_sta_area_neon/fish.csv, based on a standardized dataset with 14 plots continuously observed for >= 5 years

div_sta_area_neon_herb: contaning the same variables as in  div_sta_area_neon.csv, based on a dataset including only herbaceous species

div_sta_area_neon_sst: contaning the same variables as in  div_sta_area_neon.csv, with an additional column "sstar_ave_slope" denoting the  log-log slope of the average species stability-area relationship where the average species stability was measured by  simple weighted mean of stability across species

div_sta_area_neon_tmp/pre: contaning the same variables as in  div_sta_area_neon.csv, based on average species stability and ecosystem stability defined as relative invariability, i.e. the ratio of species/ecosystem invariability to the temperature(tmp)/precipitation(pre) invariability within the local site.

div_sta_area_fish_EU: contaning the same variables as in  div_sta_area_fish.csv, except that the construction of divesity&stability-area relationships were based on Euclidean distance

div_sta_area_fish_bio_full: contaning the same variables as in  div_sta_area_fish.csv, except that the divesity&stability metrics were based on biomass data

div_sta_area_fish_bio: contaning the same variables as in  div_sta_area_fish_bio_full.csv, based on a dataset where rare species were removd

div_sta_area_full_log2_neon_tmp/pre.csv: contaning the same variables as in div_sta_area_full_log2_neon.csv, with average species stability and ecosystem stability defined as relative invariability, i.e. the ratio of species/ecosystem invariability to the temperature(tmp)/precipitation(pre) invariability within the local site.