pacman::p_load(readxl, writexl, dplyr)

### Solar facilities from Wiki-Solar:
wiki_str = '/vol/milkunB/jbosmans/WikiSolar/200929_RUN365_B99--.xlsx'	
wind_str = '/vol/milkunB/jbosmans/SolarWindPotentials/turbines_LatLongAdded_20200106_1.xlsx'	#update?
wiki_df  <- read_excel(wiki_str)
wind_df  <- read_excel(wind_str)

### for PV select those (nearly) in operation and with location
wiki_df <- wiki_df[wiki_df$Status != 'C',]
wiki_df <- wiki_df[complete.cases(wiki_df$"Lat,Long"),] 

### Count facilities per country
PV_country   <- table(wiki_df$Country)
wind_country <- table(wind_df$Country_x)

### Define regions - see IMAGE_regions.txt (also includes list of countries in Wiki-Solar)
Canada    <- c('Canada')
USA       <- c('St. Pierre and Miquelon', 'United States', 'USA')
Mexico 	  <- c('Mexico')
Central_America <- c('Anguilla', 'Aruba', 'The Bahamas', 'Barbados', 'Belize', 'Bermuda', 'Cayman Islands', 'Costa Rica' , 'Cuba', 'Dominica', 'Dominican Republic', 'El Salvador', 'Grenada', 'Guadeloupe', 'Guatemala', 'Haiti', 'Honduras', 'Jamaica', 'Martinique', 'Montserrat', 'Netherlands Antilles', 'Nicaragua', 'Panama', 'Puerto Rico', 'St. Kitts and Nevis', 'Saint Kitts and Nevis', 'St. Lucia', 'Saint Lucia', 'St. Vincent and the Grenadines', 'Trinidad and Tobago', 'Turks and Caicos Isl.', 'Virgin Isl.', 'Virgin Islands (U.S.)', 'US Virgin Islands', 'Bonaire, Sint Eustatius and Saba', 'Curacao')
Brazil 	         <-  c('Brazil')
Ro_South_America <-  c('Argentina', 'Bolivia', 'Chile', 'Colombia', 'Ecuador', 'Falklands Isl.', 'French Guyana', 'Guyana', 'Paraguay', 'Peru', 'Suriname', 'Uruguay', 'Venezuela', 'RB', 'French Guiana')
Northern_Africa  <-  c('Algeria', 'Egypt', 'Arab Rep.', 'Libya', 'Morocco', 'Tunisia', 'Western Sahara')
Western_Africa 	 <-  c('Benin', 'Burkina Faso', 'Cameroon', 'Cape Verde', 'Central African Republic', 'Chad', 'Congo, Dem. Rep.', 'Congo, Rep.', 'Cote d\'Ivoire', 'Ivory Coast', 'Equatorial Guinea', 'Gabon', 'Gambia', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Liberia', 'Mali', 'Mauritania', 'Niger', 'Nigeria', 'Sao Tome and Principe', 'Senegal', 'Sierra Leone', 'St. Helena', 'Togo')
Eastern_Africa 	 <- c('Burundi', 'Comoros', 'Djibouti', 'Eritrea', 'Ethiopia', 'Kenya', 'Madagascar', 'Mauritius', 'Reunion', 'Rwanda', 'Seychelles', 'Somalia', 'Sudan', 'Uganda','Mayotte','RÃ©union')
South_Africa 	 <- c('South Africa')
Western_Europe 	 <- c('Andorra', 'Austria', 'Belgium', 'Denmark', 'Faeroe Islands', 'Faroe Islands', 'Finland', 'France', 'Germany', 'Gibraltar', 'Greece', 'Iceland', 'Ireland', 'Italy', 'Liechtenstein', 'Luxembourg', 'Malta', 'Monaco', 'Netherlands', 'Norway', 'Portugal',' San Marino', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom', 'United-Kingdom', 'Vatican City State')
Central_Europe 	 <- c('Albania', 'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Estonia', 'Hungary', 'Latvia', 'Lithuania', 'Macedonia', 'North Macedonia', 'FYR', 'Poland', 'Romania', 'Serbia and Montenegro', 'Serbia', 'Montenegro', 'Slovak Republic', 'Slovakia', 'Slovenia', 'Kosovo')
Turkey 	          <- c('Turkey')
Ukraine_region 	  <- c('Belarus', 'Moldova', 'Ukraine')
Central_Asia  	  <- c('Kazakhstan', 'Kyrgyz Republic', 'Kyrgyzstan', 'Tajikistan', 'Turkmenistan', 'Uzbekistan')
Russia_region 	  <- c('Armenia', 'Azerbaijan', 'Georgia', 'Russian Federation', 'Russia')
Middle_East       <- c('Bahrain', 'Iran', 'Iraq', 'Israel', 'Jordan', 'Kuwait', 'Lebanon', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Syria', 'United Arab Emirates', 'Yemen, Rep.', 'Palestine')
India 	          <- c('India')
Korea_region      <- c('Korea, Dem. Rep.', 'Korea, Rep.', 'South Korea')
China_region 	  <- c('China', 'Hong Kong, China', 'Macao, China', 'Mongolia', 'Taiwan')
Southeastern_Asia <- c('Brunei', 'Cambodia', 'Lao PDR', 'Malaysia', 'Myanmar', 'Philippines', 'Singapore', 'Thailand', 'Vietnam')
Indonesia_region  <- c('East Timor', 'Indonesia', 'Papua New Guinea')
Japan 	          <- c('Japan')
Oceania 	      <- c('American Samoa', 'Australia', 'Cook Isl.', 'Fiji', 'French Polynesia', 'Kiribati', 'Marshall Islands', 'Micronesia, Fed. Sts.', 'Micronesia', 'Nauru', 'New Caledonia', 'New Zealand', 'New-Zealand', 'Niue', 'Northern Mariana Islands', 'Palau', 'Pitcairn', 'Samoa', 'Solomon Islands', 'Tokelau', 'Tonga', 'Tuvalu', 'Vanuatu', 'Wallis ans Futuna Island','Guam')
Ro_South_Asia     <- c('Afghanistan', 'Bangladesh', 'Bhutan', 'Maldives', 'Nepal', 'Pakistan', 'Sri Lanka')
Ro_Southern_Africa<- c('Angola', 'Botswana', 'Lesotho', 'Malawi', 'Mozambique', 'Namibia', 'Swaziland', 'Tanzania', 'Zambia', 'Zimbabwe')

### Add a column with region and re-count
# First for PV
wiki_df$region = 'na'
wiki_df$region[wiki_df$Country %in% Canada]             <- 'Canada'
wiki_df$region[wiki_df$Country %in% USA]                <- 'USA'
wiki_df$region[wiki_df$Country %in% Mexico]             <- 'Mexico'
wiki_df$region[wiki_df$Country %in% Central_America]    <- 'Central_America'
wiki_df$region[wiki_df$Country %in% Brazil]             <- 'Brazil'
wiki_df$region[wiki_df$Country %in% Ro_South_America]   <- 'Ro_South_America'
wiki_df$region[wiki_df$Country %in% Northern_Africa]    <- 'Northern_Africa'
wiki_df$region[wiki_df$Country %in% Western_Africa]     <- 'Western_Africa'
wiki_df$region[wiki_df$Country %in% Eastern_Africa]     <- 'Eastern_Africa'
wiki_df$region[wiki_df$Country %in% South_Africa]       <- 'South_Africa'
wiki_df$region[wiki_df$Country %in% Western_Europe]     <- 'Western_Europe'
wiki_df$region[wiki_df$Country %in% Central_Europe]     <- 'Central_Europe'
wiki_df$region[wiki_df$Country %in% Turkey]             <- 'Turkey'
wiki_df$region[wiki_df$Country %in% Ukraine_region]     <- 'Ukraine_region'
wiki_df$region[wiki_df$Country %in% Central_Asia]       <- 'Central_Asia'
wiki_df$region[wiki_df$Country %in% Russia_region]      <- 'Russia_region'
wiki_df$region[wiki_df$Country %in% Middle_East]        <- 'Middle_East'
wiki_df$region[wiki_df$Country %in% India]              <- 'India'
wiki_df$region[wiki_df$Country %in% Korea_region]       <- 'Korea_region'
wiki_df$region[wiki_df$Country %in% China_region]       <- 'China_region'
wiki_df$region[wiki_df$Country %in% Southeastern_Asia]  <- 'Southeastern_Asia'
wiki_df$region[wiki_df$Country %in% Indonesia_region]   <- 'Indonesia_region'
wiki_df$region[wiki_df$Country %in% China_region]       <- 'China_region'
wiki_df$region[wiki_df$Country %in% Japan]              <- 'Japan'
wiki_df$region[wiki_df$Country %in% Oceania]            <- 'Oceania'
wiki_df$region[wiki_df$Country %in% Ro_South_Asia]      <- 'Ro_South_Asia'
wiki_df$region[wiki_df$Country %in% Ro_Southern_Africa] <- 'Ro_Southern_Africa'

PV_region <- table(wiki_df$region)

# Then for wind 
wind_df$region = 'na'
wind_df$region[wind_df$Country_x %in% Canada]             <- 'Canada'
wind_df$region[wind_df$Country_x %in% USA]                <- 'USA'
wind_df$region[wind_df$Country_x %in% Mexico]             <- 'Mexico'
wind_df$region[wind_df$Country_x %in% Central_America]    <- 'Central_America'
wind_df$region[wind_df$Country_x %in% Brazil]             <- 'Brazil'
wind_df$region[wind_df$Country_x %in% Ro_South_America]   <- 'Ro_South_America'
wind_df$region[wind_df$Country_x %in% Northern_Africa]    <- 'Northern_Africa'
wind_df$region[wind_df$Country_x %in% Western_Africa]     <- 'Western_Africa'
wind_df$region[wind_df$Country_x %in% Eastern_Africa]     <- 'Eastern_Africa'
wind_df$region[wind_df$Country_x %in% South_Africa]       <- 'South_Africa'
wind_df$region[wind_df$Country_x %in% Western_Europe]     <- 'Western_Europe'
wind_df$region[wind_df$Country_x %in% Central_Europe]     <- 'Central_Europe'
wind_df$region[wind_df$Country_x %in% Turkey]             <- 'Turkey'
wind_df$region[wind_df$Country_x %in% Ukraine_region]     <- 'Ukraine_region'
wind_df$region[wind_df$Country_x %in% Central_Asia]       <- 'Central_Asia'
wind_df$region[wind_df$Country_x %in% Russia_region]      <- 'Russia_region'
wind_df$region[wind_df$Country_x %in% Middle_East]        <- 'Middle_East'
wind_df$region[wind_df$Country_x %in% India]              <- 'India'
wind_df$region[wind_df$Country_x %in% Korea_region]       <- 'Korea_region'
wind_df$region[wind_df$Country_x %in% China_region]       <- 'China_region'
wind_df$region[wind_df$Country_x %in% Southeastern_Asia]  <- 'Southeastern_Asia'
wind_df$region[wind_df$Country_x %in% Indonesia_region]   <- 'Indonesia_region'
wind_df$region[wind_df$Country_x %in% China_region]       <- 'China_region'
wind_df$region[wind_df$Country_x %in% Japan]              <- 'Japan'
wind_df$region[wind_df$Country_x %in% Oceania]            <- 'Oceania'
wind_df$region[wind_df$Country_x %in% Ro_South_Asia]      <- 'Ro_South_Asia'
wind_df$region[wind_df$Country_x %in% Ro_Southern_Africa] <- 'Ro_Southern_Africa'

# special case for Curacao, weirdly spelled in wind database (Curaï¿½ao)
wind_df$region <- ifelse(grepl('Cura',wind_df$Country_x),'Central_America',wind_df$region)

#check that there's no missing values amonst region:
wind_na <- wind_df[which(wind_df[,'region'] == 'na'),]	#neater: sum(is.na(wind_df$region))
table(wind_na$Country_x)

# wind farm file has been pre-processed by Tine so no missing values for rated or total power (check that I use the right file)
wind_df$TotalPower <- as.numeric(wind_df$'Total power')

# split into on- and offshore wind
wind_on  <- wind_df[which(wind_df[,'Offshore'] == 0),]
wind_off <- wind_df[which(wind_df[,'Offshore'] == 1),]

PV_region       <- as.data.frame(table(wiki_df$region))
onshore_region  <- as.data.frame(table(wind_on$region))
offshore_region <- as.data.frame(table(wind_off$region))

# given that onshore wind contains a lot of single turbines, also sum capacity per region (see also line 111-112)
# wiki-solar PV farms not yet pre-processed so replace missing MWp using MWac
wiki_df$MWp <- ifelse(is.na(wiki_df$MWp), wiki_df$MWac/0.8, wiki_df$MWp)

print(paste('Total GW capacity in wind database: ',sum(wind_df$TotalPower)/1000000))
print(paste('Total GW capacity in solar database:',sum(wiki_df$MWp)/1000))

# capacities in GW per region:
PV_capacity       <- aggregate(wiki_df$MWp/1000.0,by=list(region=wiki_df$region),FUN=sum)
onshore_capacity  <- aggregate(wind_on$TotalPower/1000000.0,by=list(region=wind_on$region),FUN=sum)
offshore_capacity <- aggregate(wind_off$TotalPower/1000000.0,by=list(region=wind_off$region),FUN=sum)

# rename columns and merge dataframes
names(PV_region)[names(PV_region) == "Var1"]              <- "region"
names(PV_region)[names(PV_region) == "Freq"]              <- "PV_facilities"
names(onshore_region)[names(onshore_region) == "Var1"]    <- "region"
names(onshore_region)[names(onshore_region) == "Freq"]    <- "onshore_wind_facilities"
names(offshore_region)[names(offshore_region) == "Var1"]  <- "region"
names(offshore_region)[names(offshore_region) == "Freq"]  <- "offshore_wind_facilities"
names(PV_capacity)[names(PV_capacity) == "x"]             <- "PV_capacity_GW"
names(onshore_capacity)[names(onshore_capacity) == "x"]   <- "onshore_capacity_GW"
names(offshore_capacity)[names(offshore_capacity) == "x"] <- "offshore_capacity_GW"

region_df <- Reduce(function(x,y) merge(x = x, y = y, by = "region",all=TRUE), list(PV_region, onshore_region, offshore_region,PV_capacity,onshore_capacity,offshore_capacity))
write_xlsx(region_df,'Wind_solar_per_IMAGEregion.xlsx')
