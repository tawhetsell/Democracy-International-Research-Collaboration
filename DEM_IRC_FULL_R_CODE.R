# Meta-Data ---------------------------------------------------------------
#Author: Travis A. Whetsell
#Date: May 31, 2023
#Title: Democratic Governance and International Research Collaboration 


# Packages ----------------------------------------------------------------

#for mac
# setwd(
#   "/Users/traviswhetsell/Library/CloudStorage/OneDrive-GeorgiaInstituteofTechnology/Priority 1/global S&T network data/for github/"
# )

#for pc
# setwd('C:\\Users\\twhetsell3\\OneDrive - Georgia Institute of Technology\\Priority 1\\global S&T network data\\for github')


library(tidyverse)
library(statnet)
library(GGally)
library(ggplot2)
library(grid)
library(gridExtra)
library(Hmisc)
library(zoo)
library(cshapes)
library(patchwork)
library(countrycode)
library(btergm)
library(backbone)
library(speedglm)
library(ergMargins)
library(texreg)
library(lme4)
library(plm)

# Importing, Merging & Cleaning Data ----------------------------------------------------------------

## read in the wide format data. I re-formatted these manually in excel.

VDEM_WIDE <- read.csv("VDEM WIDE.csv" , sep = "," , header = TRUE)
## drops ssd which has no data
NODELIST <- VDEM_WIDE[-146, , drop = FALSE]
GDPcap <- read.csv("GDPcap Data.csv" , sep = "," , header = TRUE)
Population_Data <-
  read.csv("Population Data.csv" , sep = "," , header = TRUE)
urbanization <-
  read.csv("urbanization.csv" , sep = "," , header = TRUE)
authors <- read.csv("authors.csv" , sep = "," , header = TRUE)

### merge with nodelist

NODELIST <-
  merge(x = NODELIST,
        y = GDPcap,
        by = "country_code",
        all.x = TRUE)
NODELIST <-
  merge(x = NODELIST,
        y = Population_Data,
        by = "country_code",
        all.x = TRUE)
NODELIST <-
  merge(x = NODELIST,
        y = urbanization,
        by = "country_code",
        all.x = TRUE)
NODELIST <-
  merge(x = NODELIST,
        y = authors,
        by = "country_code",
        all.x = TRUE)

## drop zzb no data, v-dem data has two gaza country codes pse and psg, dropped psg which has no world bank data
## no world bank data on somaliland, somalia will suffice
## XKX was dropped no data in bibliometrics

NODELIST  <-
  select(NODELIST, -ends_with("2007"), -ends_with("2018"))
NODELIST  <-
  NODELIST [!grepl("PSG|ZZB|SML|XKX", NODELIST$country_code),]

## identify countrues with missing values
missing_data_countries <- NODELIST  %>%
  filter(
    is.na(gdp2008) |
      is.na(gdp2009) |
      is.na(gdp2010) | 
      is.na(gdp2011) | 
      is.na(gdp2012) |
      is.na(gdp2013) |
      is.na(gdp2014) |
      is.na(gdp2015) | 
      is.na(gdp2016) | 
      is.na(gdp2017)
  ) %>%
  select(country_code)
print(missing_data_countries)

# country_code
# 1          ERI
# 2          PRK
# 3          SOM
# 4          SYR
# 5          TWN
# 6          VEN

# count_missing <- function(row) {
#   return(sum(is.na(row)))
# }
# 
# # Identify countries with partially missing data
# NODELIST$missing_count <- apply(NODELIST[, c('gdp2008', 'gdp2009', 'gdp2010', 'gdp2011', 'gdp2012', 'gdp2013', 'gdp2014', 'gdp2015', 'gdp2016', 'gdp2017')], 1, count_missing)
# partially_missing_data_countries <- NODELIST%>%
#   filter(missing_count > 0 & missing_count < 10) %>%
#   select(country_code)
# print(partially_missing_data_countries)
# 
# # country_code
# 1          ERI
# 2          SOM
# 3          VEN

# twn - gdpcap from v-dem 
# 
# 34762.37 33974.45 37385.49 38565 39207 39971 41472 41668 42165 43503.79
# 
# ven - gdpcap from v-dem 
# 
# 18019.09 17242.94 17105.58 17746 18549 21429 20317 18802 15219 12879.14
# 
# prk - gdpcap from v-dem 
# 
# 1794.66 1768.89 1751.44 1756.61 1770.92 1781.3 1791.34 1762.52 1742.02 1673.28
# 


# no v-dem data for , eri eritrea,som somalia, but eri som and ven have partial data in world bank
# twn, prk, syr, will be ammended with v-dem gdp data

## impute v-dem gdp data for twn prk

twn_gdp_values <-
  c(
    gdp2008 = 34762.37,
    gdp2009 = 33974.45,
    gdp2010 = 37385.49,
    gdp2011 = 38565,
    gdp2012 = 39207,
    gdp2013 = 39971,
    gdp2014 = 41472,
    gdp2015 = 41668,
    gdp2016 = 42165,
    gdp2017 = 43503.79
  )

NODELIST <- NODELIST %>%
  mutate(across(
    starts_with("gdp"),
    ~ case_when(country_code == "TWN" ~ twn_gdp_values[cur_column()],
                TRUE ~ .),
    .names = "{col}"
  ))

prk_gdp_values <-
  c(
    gdp2008 = 1794.66,
    gdp2009 = 1768.89,
    gdp2010 = 1751.44,
    gdp2011 = 1756.61,
    gdp2012 = 1770.92,
    gdp2013 = 1781.3,
    gdp2014 = 1791.34,
    gdp2015 = 1762.52,
    gdp2016 = 1742.02,
    gdp2017 = 1673.28
  )

NODELIST <- NODELIST %>%
  mutate(across(
    starts_with("gdp"),
    ~ case_when(country_code == "PRK" ~ prk_gdp_values[cur_column()],
                TRUE ~ .),
    .names = "{col}"
  ))

# for some reason my download file from the world bank does not contain gdp data on syr, however the website shows the data. The data must have updated. Will impute from website

syr_gdp_values <-
  c(
    gdp2008 = 10156,
    gdp2009 = 10228.1,
    gdp2010 = 11304.6,
    gdp2011 = 2971.3,
    gdp2012 = 1910.6,
    gdp2013 = 993.7,
    gdp2014 = 1071.2,
    gdp2015 = 857.5,
    gdp2016 = 862.3,
    gdp2017 = 3241.4
  )

NODELIST <- NODELIST %>%
  mutate(across(
    starts_with("gdp"),
    ~ case_when(country_code == "SYR" ~ syr_gdp_values[cur_column()],
                TRUE ~ .),
    .names = "{col}"
  ))

# eri som ven are partially missing will be mean imputed based on adjacent data

NODELIST <- NODELIST %>%
  rowwise() %>%
  mutate(across(starts_with("gdp"), ~ ifelse(is.na(.), mean(
    c_across(starts_with("gdp")), na.rm = TRUE
  ), .))) %>%
  ungroup()

# missing_data_countries <- NODELIST  %>%
#   filter(is.na(pop2008) | is.na(pop2009) | is.na(pop2010) | is.na(pop2011) | is.na(pop2012) |
#            is.na(pop2013) | is.na(pop2014) | is.na(pop2015) | is.na(pop2016) | is.na(pop2017)) %>%
#   select(country_code)
# print(missing_data_countries)
# 
# # 1 ERI         
# # 2 TWN     
# 
# # TWN has no world bank pop data. I impute statistia data - https://www.statista.com/statistics/319793/taiwan-population/
# 23040000
# 23120000
# 23160000
# 23220000
# 23320000
# 23370000
# 23430000
# 23490000
# 23540000
# 23570000

## imputing TWN pop data from statistia, absent in world bank

twn_pop_values <-
  c(
    pop2008 = 23040000,
    pop2009 = 23120000,
    pop2010 = 23160000,
    pop2011 = 23220000,
    pop2012 = 23320000,
    pop2013 = 23370000,
    pop2014 = 23430000,
    pop2015 = 23490000,
    pop2016 = 23540000,
    pop2017 = 23570000
  )

NODELIST <- NODELIST %>%
  mutate(across(
    starts_with("pop"),
    ~ case_when(country_code == "TWN" ~ twn_pop_values[cur_column()],
                TRUE ~ .),
    .names = "{col}"
  ))

# eri has partial pop data will be row mean inputed

NODELIST <- NODELIST %>%
  rowwise() %>%
  mutate(across(starts_with("pop"), ~ ifelse(is.na(.), mean(
    c_across(starts_with("pop")), na.rm = TRUE
  ), .))) %>%
  ungroup()

# missing_data_countries <- NODELIST  %>%
#   filter(is.na(urb2008) | is.na(urb2009) | is.na(urb2010) | is.na(urb2011) | is.na(urb2012) |
#            is.na(urb2013) | is.na(urb2014) | is.na(urb2015) | is.na(urb2016) | is.na(urb2017)) %>%
#   select(country_code)
# print(missing_data_countries)

# 1 ERI         
# 2 TWN  

# taiwan has partial data from statistia -- https://www.statista.com/statistics/1319652/urbanization-rate-in-taiwan/

# 72.30 2005
# 74.70 2010
# 76.90 2015

twn_urb_values <-
  c(
    urb2008 = 72.30,
    urb2009 = 72.30,
    urb2010 = 74.70,
    urb2011 = 74.70,
    urb2012 = 74.70,
    urb2013 = 74.70,
    urb2014 = 74.70,
    urb2015 = 76.90,
    urb2016 = 76.90,
    urb2017 = 76.90
  )

NODELIST <- NODELIST %>%
  mutate(across(
    starts_with("urb"),
    ~ case_when(country_code == "TWN" ~ twn_urb_values[cur_column()],
                TRUE ~ .),
    .names = "{col}"
  ))

# eri has partial data urb will be mean inputed

NODELIST <- NODELIST %>%
  rowwise() %>%
  mutate(across(starts_with("urb"), ~ ifelse(is.na(.), mean(
    c_across(starts_with("urb")), na.rm = TRUE
  ), .))) %>%
  ungroup()

#From the initial 200 country matrix, I have identified row/columns that do not have complete data. In the following code I will remove these nodes from the matrixes. I also formatted these matrixes to contain uniform rows and columns manually before import (a requirement for TERGM), which I based on the last year 2017 list.  

years <- 2008:2017
matrix_list <- list()

for (year in years) {
  file_name <- paste0(year, ".csv")
  matrix <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_list[[as.character(year)]] <- matrix
}

# identify country codes and row names that do not match
# country_codes <- unique(NODELIST$country_code)
# country_codes_not_in_matrix <- setdiff(country_codes, rownames(matrix2008del))
# rows_not_in_country_codes <- setdiff(rownames(matrix2008del), country_codes)
# cat("Country codes in NODELIST but not in matrix2008del: ", paste(country_codes_not_in_matrix, collapse = ", "), "\n")
# cat("Rows in matrix2008del but not in NODELIST: ", paste(rows_not_in_country_codes, collapse = ", "), "\n")
# 
# # Country codes in NODELIST but not in matrix2008del:  COG, GNB, GNQ, HKG 
# Rows in matrix2008del but not in NODELIST:  AND, ANT, ATG, BHS, BLZ, BMU, BRN, FSM, GIB, GLP, GRD, GRL, GUF, KNA, KOS, LCA, LIE, MCO, NCL, PLW, PYF, REU, SMR, SSD, TOA, VAT, VCT, WAS, WSM, ZRN 

## these nodes are not contained in the matrixes, must remove from nodelist

NODELIST  <-
  NODELIST [!grepl("COG|GNB|GNQ|HKG", NODELIST$country_code), ]

### these are not contained in nodelist, must remove from matrixes

names_to_remove <-
  c(
    "AND",
    "ANT",
    "ATG",
    "BHS",
    "BLZ",
    "BMU",
    "BRN",
    "FSM",
    "GIB",
    "GLP",
    "GRD",
    "GRL",
    "GUF",
    "KNA",
    "KOS",
    "LCA",
    "LIE",
    "MCO",
    "NCL",
    "PLW",
    "PYF",
    "REU",
    "SMR",
    "SSD",
    "TOA",
    "VAT",
    "VCT",
    "WAS",
    "WSM",
    "ZRN"
  )

for (year in names(matrix_list)) {
  matrix <- matrix_list[[year]]
  matrix <-
    matrix[!(rownames(matrix) %in% names_to_remove),!(colnames(matrix) %in% names_to_remove)]
  matrix_list[[year]] <- matrix
}

# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2008del <- matrix2008del[!(rownames(matrix2008del) %in% names_to_remove), !(colnames(matrix2008del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2009del <- matrix2009del[!(rownames(matrix2009del) %in% names_to_remove), !(colnames(matrix2009del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2010del <- matrix2010del[!(rownames(matrix2010del) %in% names_to_remove), !(colnames(matrix2010del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2011del <- matrix2011del[!(rownames(matrix2011del) %in% names_to_remove), !(colnames(matrix2011del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2012del <- matrix2012del[!(rownames(matrix2012del) %in% names_to_remove), !(colnames(matrix2012del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2013del <- matrix2013del[!(rownames(matrix2013del) %in% names_to_remove), !(colnames(matrix2013del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2014del <- matrix2014del[!(rownames(matrix2014del) %in% names_to_remove), !(colnames(matrix2014del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2015del <- matrix2015del[!(rownames(matrix2015del) %in% names_to_remove), !(colnames(matrix2015del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2016del <- matrix2016del[!(rownames(matrix2016del) %in% names_to_remove), !(colnames(matrix2016del) %in% names_to_remove)]
# 
# names_to_remove <- c("AND", "ANT", "ATG", "BHS", "BLZ", "BMU", "BRN", "FSM", "GIB", "GLP", "GRD", "GRL", "GUF", "KNA", "KOS", "LCA", "LIE", "MCO", "NCL", "PLW", "PYF", "REU", "SMR", "SSD", "TOA", "VAT", "VCT", "WAS", "WSM", "ZRN")
# matrix2017del <- matrix2017del[!(rownames(matrix2017del) %in% names_to_remove), !(colnames(matrix2017del) %in% names_to_remove)]

# exporting matrix files

for (year in names(matrix_list)) {
  matrix <- matrix_list[[year]]
  output_filename <- paste0("matrix", year, "rem.csv")
  write.csv(matrix, output_filename)
}

## now I am going to make my log transformations in the nodelist

for (year in 2008:2017) {
  column_name <- paste0("authors", year)
  new_column_name <- paste0("ln", column_name)
  NODELIST[[new_column_name]] <- log(NODELIST[[column_name]] + 1)
}

for (year in 2008:2017) {
  column_name <- paste0("pop", year)
  new_column_name <- paste0("ln", column_name)
  NODELIST[[new_column_name]] <- log(NODELIST[[column_name]] + 1)
}

for (year in 2008:2017) {
  column_name <- paste0("gdp", year)
  new_column_name <- paste0("ln", column_name)
  NODELIST[[new_column_name]] <- log(NODELIST[[column_name]] + 1)
}

write.csv(NODELIST, "NODELIST.csv")

### now I need to make the distance matrix conform to my matrix. Here is a list of matrix nodes


countries <-
  c(
    "AFG",
    "AGO",
    "ALB",
    "ARE",
    "ARG",
    "ARM",
    "AUS",
    "AUT",
    "AZE",
    "BDI",
    "BEL",
    "BEN",
    "BFA",
    "BGD",
    "BGR",
    "BHR",
    "BIH",
    "BLR",
    "BOL",
    "BRA",
    "BRB",
    "BTN",
    "BWA",
    "CAF",
    "CAN",
    "CHE",
    "CHL",
    "CHN",
    "CIV",
    "CMR",
    "COD",
    "COL",
    "COM",
    "CPV",
    "CRI",
    "CUB",
    "CYP",
    "CZE",
    "DEU",
    "DJI",
    "DNK",
    "DOM",
    "DZA",
    "ECU",
    "EGY",
    "ERI",
    "ESP",
    "EST",
    "ETH",
    "FIN",
    "FJI",
    "FRA",
    "GAB",
    "GBR",
    "GEO",
    "GHA",
    "GIN",
    "GMB",
    "GRC",
    "GTM",
    "GUY",
    "HND",
    "HRV",
    "HTI",
    "HUN",
    "IDN",
    "IND",
    "IRL",
    "IRN",
    "IRQ",
    "ISL",
    "ISR",
    "ITA",
    "JAM",
    "JOR",
    "JPN",
    "KAZ",
    "KEN",
    "KGZ",
    "KHM",
    "KOR",
    "KWT",
    "LAO",
    "LBN",
    "LBR",
    "LBY",
    "LKA",
    "LSO",
    "LTU",
    "LUX",
    "LVA",
    "MAR",
    "MDA",
    "MDG",
    "MDV",
    "MEX",
    "MKD",
    "MLI",
    "MLT",
    "MMR",
    "MNE",
    "MNG",
    "MOZ",
    "MRT",
    "MUS",
    "MWI",
    "MYS",
    "NAM",
    "NER",
    "NGA",
    "NIC",
    "NLD",
    "NOR",
    "NPL",
    "NZL",
    "OMN",
    "PAK",
    "PAN",
    "PER",
    "PHL",
    "PNG",
    "POL",
    "PRK",
    "PRT",
    "PRY",
    "PSE",
    "QAT",
    "ROU",
    "RUS",
    "RWA",
    "SAU",
    "SDN",
    "SEN",
    "SGP",
    "SLB",
    "SLE",
    "SLV",
    "SOM",
    "SRB",
    "STP",
    "SUR",
    "SVK",
    "SVN",
    "SWE",
    "SWZ",
    "SYC",
    "SYR",
    "TCD",
    "TGO",
    "THA",
    "TJK",
    "TKM",
    "TLS",
    "TTO",
    "TUN",
    "TUR",
    "TWN",
    "TZA",
    "UGA",
    "UKR",
    "URY",
    "USA",
    "UZB",
    "VEN",
    "VNM",
    "VUT",
    "YEM",
    "ZAF",
    "ZMB",
    "ZWE"
  )

iso_codes <-
  c(
    "AFG",
    "AGO",
    "ALB",
    "ARE",
    "ARG",
    "ARM",
    "AUS",
    "AUT",
    "AZE",
    "BDI",
    "BEL",
    "BEN",
    "BFA",
    "BGD",
    "BGR",
    "BHR",
    "BIH",
    "BLR",
    "BOL",
    "BRA",
    "BRB",
    "BTN",
    "BWA",
    "CAF",
    "CAN",
    "CHE",
    "CHL",
    "CHN",
    "CIV",
    "CMR",
    "COD",
    "COL",
    "COM",
    "CPV",
    "CRI",
    "CUB",
    "CYP",
    "CZE",
    "DEU",
    "DJI",
    "DNK",
    "DOM",
    "DZA",
    "ECU",
    "EGY",
    "ERI",
    "ESP",
    "EST",
    "ETH",
    "FIN",
    "FJI",
    "FRA",
    "GAB",
    "GBR",
    "GEO",
    "GHA",
    "GIN",
    "GMB",
    "GRC",
    "GTM",
    "GUY",
    "HND",
    "HRV",
    "HTI",
    "HUN",
    "IDN",
    "IND",
    "IRL",
    "IRN",
    "IRQ",
    "ISL",
    "ISR",
    "ITA",
    "JAM",
    "JOR",
    "JPN",
    "KAZ",
    "KEN",
    "KGZ",
    "KHM",
    "KOR",
    "KWT",
    "LAO",
    "LBN",
    "LBR",
    "LBY",
    "LKA",
    "LSO",
    "LTU",
    "LUX",
    "LVA",
    "MAR",
    "MDA",
    "MDG",
    "MDV",
    "MEX",
    "MKD",
    "MLI",
    "MLT",
    "MMR",
    "MNE",
    "MNG",
    "MOZ",
    "MRT",
    "MUS",
    "MWI",
    "MYS",
    "NAM",
    "NER",
    "NGA",
    "NIC",
    "NLD",
    "NOR",
    "NPL",
    "NZL",
    "OMN",
    "PAK",
    "PAN",
    "PER",
    "PHL",
    "PNG",
    "POL",
    "PRK",
    "PRT",
    "PRY",
    "PSE",
    "QAT",
    "ROU",
    "RUS",
    "RWA",
    "SAU",
    "SDN",
    "SEN",
    "SGP",
    "SLB",
    "SLE",
    "SLV",
    "SOM",
    "SRB",
    "STP",
    "SUR",
    "SVK",
    "SVN",
    "SWE",
    "SWZ",
    "SYC",
    "SYR",
    "TCD",
    "TGO",
    "THA",
    "TJK",
    "TKM",
    "TLS",
    "TTO",
    "TUN",
    "TUR",
    "TWN",
    "TZA",
    "UGA",
    "UKR",
    "URY",
    "USA",
    "UZB",
    "VEN",
    "VNM",
    "VUT",
    "YEM",
    "ZAF",
    "ZMB",
    "ZWE"
  )

cow_codes <- countrycode(iso_codes, "iso3c", "cown")
iso_cow_codes <- cbind(iso_codes, cow_codes)
print(iso_cow_codes)
isocow <- as.data.frame(iso_cow_codes)

# Warning message:
#   In countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest,  :
#                            Some values were not matched unambiguously: PSE, SRB

#isocow[isocow$iso_codes == "PSE", "cow_codes"] <- "666"

## this told me that two nodes aren't matching. PSE is not recongnized in the COW codes. So I will impute distances from ISR. I also found that SRB is actually 345 so I am manually telling it that.

isocow[isocow$iso_codes == "SRB", "cow_codes"] <- "345"

# print(isocow$cow_codes)

# "700" "540" "339" "696" "160" "371" "900" "305" "373" "516" "211" "434" "439" "771" "355" "692" "346" "370" "145" "140" "53"  "760" "571" "482" "20"  "225" "155" "710" "437" "471" "490" "100" "581" "402" "94"  "40" 
# "352" "316" "255" "522" "390" "42"  "615" "130" "651" "531" "230" "366" "530" "375" "950" "220" "481" "200" "372" "452" "438" "420" "350" "90"  "110" "91"  "344" "41"  "310" "850" "750" "205" "630" "645" "395" "666"
# "325" "51"  "663" "740" "705" "501" "703" "811" "732" "690" "812" "660" "450" "620" "780" "570" "368" "212" "367" "600" "359" "580" "781" "70"  "343" "432" "338" "775" "341" "712" "541" "435" "590" "553" "820" "565"
# "436" "475" "93"  "210" "385" "790" "920" "698" "770" "95"  "135" "840" "910" "290" "731" "235" "150" "666" "694" "360" "365" "517" "670" "625" "433" "830" "940" "451" "92"  "520" "345" "403" "115" "317" "349" "380"
# "572" "591" "652" "483" "461" "800" "702" "701" "860" "52"  "616" "640" "713" "510" "500" "369" "165" "2"   "704" "101" "816" "935" "679" "560" "551" "552"

# Create a vector of COW codes to keep that correpsond to ISO codes

cow_codes <-
  c(
    "700",
    "540",
    "339",
    "696",
    "160",
    "371",
    "900",
    "305",
    "373",
    "516",
    "211",
    "434",
    "439",
    "771",
    "355",
    "692",
    "346",
    "370",
    "145",
    "140",
    "53",
    "760",
    "571",
    "482",
    "20",
    "225",
    "155",
    "710",
    "437",
    "471",
    "490",
    "100",
    "581",
    "402",
    "94",
    "40",
    "352",
    "316",
    "255",
    "522",
    "390",
    "42",
    "615",
    "130",
    "651",
    "531",
    "230",
    "366",
    "530",
    "375",
    "950",
    "220",
    "481",
    "200",
    "372",
    "452",
    "438",
    "420",
    "350",
    "90",
    "110",
    "91",
    "344",
    "41",
    "310",
    "850",
    "750",
    "205",
    "630",
    "645",
    "395",
    "666",
    "325",
    "51",
    "663",
    "740",
    "705",
    "501",
    "703",
    "811",
    "732",
    "690",
    "812",
    "660",
    "450",
    "620",
    "780",
    "570",
    "368",
    "212",
    "367",
    "600",
    "359",
    "580",
    "781",
    "70",
    "343",
    "432",
    "338",
    "775",
    "341",
    "712",
    "541",
    "435",
    "590",
    "553",
    "820",
    "565",
    "436",
    "475",
    "93",
    "210",
    "385",
    "790",
    "920",
    "698",
    "770",
    "95",
    "135",
    "840",
    "910",
    "290",
    "731",
    "235",
    "150",
    "666",
    "694",
    "360",
    "365",
    "517",
    "670",
    "625",
    "433",
    "830",
    "940",
    "451",
    "92",
    "520",
    "345",
    "403",
    "115",
    "317",
    "349",
    "380",
    "572",
    "591",
    "652",
    "483",
    "461",
    "800",
    "702",
    "701",
    "860",
    "52",
    "616",
    "640",
    "713",
    "510",
    "500",
    "369",
    "165",
    "2",
    "704",
    "101",
    "816",
    "935",
    "679",
    "560",
    "551",
    "552"
  )

##generate distance matrix

distmatrix <- distmatrix(
  date = as.Date(c("2017-1-1")),
  type = "mindist",
  keep = 0.1,
  useGW = FALSE,
  dependencies = TRUE
)
print(distmatrix)

##drop unmatching columns

keep_rows_cols <-
  rownames(distmatrix) %in% cow_codes &
  colnames(distmatrix) %in% cow_codes
distmatrix1 <- distmatrix[keep_rows_cols, keep_rows_cols]

cow_codes <- isocow$cow_code
iso_codes <- isocow$iso_code

## renaming cow codes ISO codes

rownames(distmatrix1) <- sapply(rownames(distmatrix1), function(x) {
  if (x %in% cow_codes) {
    return(iso_codes[which(cow_codes == x)])
  }
  return(x)
})

colnames(distmatrix1) <- sapply(colnames(distmatrix1), function(x) {
  if (x %in% cow_codes) {
    return(iso_codes[which(cow_codes == x)])
  }
  return(x)
})

## adding PSE and imputing ISR

new_values <- rep(0, nrow(distmatrix1))
distmatrix1 <- rbind(distmatrix1, PSE = new_values)
distmatrix1 <- cbind(distmatrix1, PSE = new_values)

#Getting this message. But the dataframe looks correct. Warning message:In cbind(distmatrix1, PSE = new_values) :number of rows of result is not a multiple of vector length (arg 2)

row_isr <- distmatrix1["ISR", ]
col_isr <- distmatrix1[, "ISR"]
distmatrix1["PSE", ] <- row_isr
distmatrix1[, "PSE"] <- col_isr

sorted_row_indices <- order(rownames(distmatrix1))
sorted_col_indices <- order(colnames(distmatrix1))
distmatrix1 <- distmatrix1[sorted_row_indices, sorted_col_indices]
distmatrix1 <- as.data.frame(distmatrix1)
write.csv(distmatrix1, "distmatrixfinal.csv")

### now all the files are ready for analysis

# Descriptive Statistics ----------------------------------------------------------------

#first I will generate the descriptive statistics for the networks

read_and_process_matrix <- function(year) {
  file_name <- paste0("matrix", year, "rem.csv")
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  as.network(
    matrix_adj,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )
}

years <- 2008:2017
networks <- lapply(years, read_and_process_matrix)


for (net in networks) {
  cat("Network size:", network.size(net), "\n")
  cat("Number of dyads:", network.dyadcount(net), "\n")
  cat("Number of edges:", sum(network.edgecount(net)), "\n")
  cat("Network density:", network.density(net), "\n")
  cat("Number of components:", components(net), "\n")
  cat("Number of isolates:", sum(degree(net) == 0), "\n")
  cat("Sum of edge weights:", sum(get.edge.attribute(net, "weight")), "\n")
  cat("In-degree centralization:",
      centralization(net, degree, cmode = "indegree"),
      "\n")
  cat("Maximum edge weight:", max(get.edge.attribute(net, "weight")), "\n")
  cat("\n")
}

### documentation for triad census says valued nets are funky, so binarize first 
read_and_process_matrix <- function(year) {
  file_name <- paste0("matrix", year, "rem.csv")
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  matrix_net <-
    global(matrix_adj, upper = 0, class = "Matrix") %>% as.matrix()
  as.network(
    matrix_net,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )
}

years <- 2008:2017
networks <- lapply(years, read_and_process_matrix)

for (net in networks) {
  cat("Triad census:", triad.census(net), "\n")
  cat("Transitivity:", gtrans(net, mode = "graph"), "\n")
  cat("\n")
}

#triad types represented in triad census. Disconnected nodes are not con

#003 a<-/->b<-/->c, a<-/->c, 

#102 a<-->b<-/->c,  a<-/->c

#201 a<-->b<-/->c,  a<-->c

#300 a<-->b<-->c,  a<-->c

# the global clustering coefficient is closed/closed+open triplets, T = (type300) / (type102 + type201 + type300)

# Triad census: 259485 0 278955 0 0 0 0 0 0 0 157155 0 0 0 0 108845 
# Transitivity: 0.6750915 
# 0.1997321 = 108845 /(278955+ 157155 +108845 )
# 804440 = 259485 + 278955+ 157155 +108845 

# Triad census: 54397 0 175684 0 0 0 0 0 0 0 201961 0 0 0 0 372398 
# Transitivity: 0.8469012 
# 0.4965022 = 372398/(175684 + 201961 + 372398)
# total triads - 54397 + 175684 + 201961 + 372398
# 

# Network Visualizations ----------------------------------------------------------------
#Start with Trimmed Network

read_matrix <- function(file_name) {
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  matrix_net <-
    disparity(matrix_adj,
              alpha = 0.05,
              narrative = TRUE,
              class = "Matrix")
  matrix_net <- as.matrix(matrix_net)
  network_data <-
    as.network(
      matrix_net,
      matrix.type = 'adjacency',
      ignore.eval = FALSE,
      names.eval = 'weight',
      directed = FALSE
    )
  return(network_data)
}

demvis_transform <- function(data, column_name) {
  data[[column_name]][data[[column_name]] >= .9 &
                        data[[column_name]] < 1] <- 9
  data[[column_name]][data[[column_name]] >= .8 &
                        data[[column_name]] < .9] <- 8
  data[[column_name]][data[[column_name]] >= .7 &
                        data[[column_name]] < .8] <- 7
  data[[column_name]][data[[column_name]] >= .6 &
                        data[[column_name]] < .7] <- 6
  data[[column_name]][data[[column_name]] >= .5 &
                        data[[column_name]] < .6] <- 5
  data[[column_name]][data[[column_name]] >= .4 &
                        data[[column_name]] < .5] <- 4
  data[[column_name]][data[[column_name]] >= .3 &
                        data[[column_name]] < .4] <- 3
  data[[column_name]][data[[column_name]] >= .2 &
                        data[[column_name]] < .3] <- 2
  data[[column_name]][data[[column_name]] >= .1 &
                        data[[column_name]] < .2] <- 1
  data[[column_name]][data[[column_name]] >= .0 &
                        data[[column_name]] < .1] <- 0
  return(data)
}

years <- 2008:2017
networks <- list()

for (year in years) {
  file_name <- paste0("matrix", year, "rem.csv")
  networks[[as.character(year)]] <- read_matrix(file_name)
}


NODELIST01132022 <-
  read.csv("NODELIST.csv", sep = ",", header = TRUE)

for (year in years) {
  column_name <- paste0("demvis", year)
  NODELIST01132022[[column_name]] <-
    NODELIST01132022[[paste0("v2x_libdem", year)]]
  NODELIST01132022 <-
    demvis_transform(NODELIST01132022, column_name)
}

years <- 2008:2017
networks <- paste0("network", years)
Anetvis <- paste0("Anetvis", years)

#

years <- 2008:2017
networks <- list()

for (year in years) {
  file_name <- paste0("matrix", year, "rem.csv")
  network_data <- read_matrix(file_name)
  networks[[as.character(year)]] <- network_data
}

set.seed(101)

counter <- 1

for (year in years) {
  network_data <- networks[[as.character(year)]]
  network_data <-
    set.vertex.attribute(network_data, "Liberal Democracy", NODELIST01132022[[paste0("demvis", year)]])
  
  Anetvis_temp <-
    ggnet2(
      network_data,
      mode = "fruchtermanreingold",
      layout.par = list(niter = 10000),
      size.min = 1,
      edge.alpha = 0.3,
      edge.color = c("color", "grey50"),
      edge.size = 0.25,
      size.cut = 100,
      size = "degree",
      max_size = 2.5,
      node.color = "Liberal Democracy",
      palette = "RdYlBu",
      alpha = .75
    ) +
    guides(size = "none", color = "none")
  
  assign(Anetvis[counter], Anetvis_temp)
  counter <- counter + 1
}

# FULL Untrimmed NETWORK VIS

read_matrix <- function(file_name) {
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  matrix_net <- as.matrix(matrix_adj)
  network_data <-
    as.network(
      matrix_net,
      matrix.type = 'adjacency',
      ignore.eval = FALSE,
      names.eval = 'weight',
      directed = FALSE
    )
  return(network_data)
}

demvis_transform <- function(data, column_name) {
  data[[column_name]][data[[column_name]] >= .9 &
                        data[[column_name]] < 1] <- 9
  data[[column_name]][data[[column_name]] >= .8 &
                        data[[column_name]] < .9] <- 8
  data[[column_name]][data[[column_name]] >= .7 &
                        data[[column_name]] < .8] <- 7
  data[[column_name]][data[[column_name]] >= .6 &
                        data[[column_name]] < .7] <- 6
  data[[column_name]][data[[column_name]] >= .5 &
                        data[[column_name]] < .6] <- 5
  data[[column_name]][data[[column_name]] >= .4 &
                        data[[column_name]] < .5] <- 4
  data[[column_name]][data[[column_name]] >= .3 &
                        data[[column_name]] < .4] <- 3
  data[[column_name]][data[[column_name]] >= .2 &
                        data[[column_name]] < .3] <- 2
  data[[column_name]][data[[column_name]] >= .1 &
                        data[[column_name]] < .2] <- 1
  data[[column_name]][data[[column_name]] >= .0 &
                        data[[column_name]] < .1] <- 0
  return(data)
}

years <- 2008:2017
networks <- list()

for (year in years) {
  file_name <- paste0("matrix", year, "rem.csv")
  networks[[as.character(year)]] <- read_matrix(file_name)
}

NODELIST01132022 <-
  read.csv("NODELIST.csv", sep = ",", header = TRUE)

for (year in years) {
  column_name <- paste0("demvis", year)
  NODELIST01132022[[column_name]] <-
    NODELIST01132022[[paste0("v2x_libdem", year)]]
  NODELIST01132022 <-
    demvis_transform(NODELIST01132022, column_name)
}

years <- 2008:2017
networks <- paste0("network", years)
Bnetvis <- paste0("Bnetvis", years)

#

years <- 2008:2017
networks <- list()

for (year in years) {
  file_name <- paste0("matrix", year, "rem.csv")
  network_data <- read_matrix(file_name)
  networks[[as.character(year)]] <- network_data
}

set.seed(101)

counter <- 1

for (year in years) {
  network_data <- networks[[as.character(year)]]
  network_data <-
    set.vertex.attribute(network_data, "Liberal Democracy", NODELIST01132022[[paste0("demvis", year)]])
  
  Bnetvis_temp <-
    ggnet2(
      network_data,
      mode = "fruchtermanreingold",
      layout.par = list(niter = 10000),
      size.min = 0.5,
      edge.alpha = 0.3,
      edge.color = c("color", "grey50"),
      edge.size = 0.25,
      size.cut = 100,
      size = "degree",
      max_size = 2.5,
      node.color = "Liberal Democracy",
      palette = "RdYlBu",
      alpha = .75
    ) +
    guides(size = "none") + guides(color = "none")
  
  assign(Bnetvis[counter], Bnetvis_temp)
  counter <- counter + 1
}

# Now Vizualize

Anetvis_plots <- list()
Bnetvis_plots <- list()

for (year in years) {
  Anetvis_temp <-
    get(paste0("Anetvis", year)) + ggtitle(year) + theme(plot.title = element_text(size = 10, family = "Times"))
  Anetvis_plots[[as.character(year)]] <- Anetvis_temp
  
  Bnetvis_temp <-
    get(paste0("Bnetvis", year)) + ggtitle(year) + theme(plot.title = element_text(size = 10, family = "Times"))
  Bnetvis_plots[[as.character(year)]] <- Bnetvis_temp
}

# plot_list <- c(Anetvis_plots, Bnetvis_plots)
# do.call("grid.arrange", c(plot_list, list(ncol = 5, nrow = 4)))
# fig2 <-
#   do.call("grid.arrange", c(plot_list, list(ncol = 5, nrow = 4)))

# pdf("Figure 2.pdf", width = 7.5, height = 6, points=8, bg= "white")
# plot(fig2, main = '')
# dev.off()

plot_list1 <- c(Anetvis_plots)
do.call("grid.arrange", c(plot_list1, list(ncol = 5, nrow = 2)))
fig2a <-
  do.call("grid.arrange", c(plot_list1, list(ncol = 5, nrow = 2)))

# pdf("Figure 2a.pdf", width = 12.5, height = 5, points=8, bg= "white")
# plot(fig2a, main = '')
# dev.off()

plot_list2 <- c(Bnetvis_plots)
do.call("grid.arrange", c(plot_list2, list(ncol = 5, nrow = 2)))
fig2b <-
  do.call("grid.arrange", c(plot_list2, list(ncol = 5, nrow = 2)))

# pdf("Figure 2b.pdf", width = 12.5, height = 5, points=8, bg= "white")
# plot(fig2b, main = '')
# dev.off()

# BTERGMs ----------------------------------------------------------------

# BTERGM.BIN ---------------------------------------------------------------
#there are some conflicts in packages, load only tidyverse, btergm, backbone
detach(package:statnet)
detach

read_and_process_matrix <- function(year) {
  file_name <- paste0("matrix", year, "rem.csv")
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  matrix_net <-
    global(matrix_adj, upper = 0, class = "Matrix") %>% as.matrix()
  as.network(
    matrix_net,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )
}

add_vertex_attributes <- function(network, year, nodelist) {
  network %v% "libdem" <- nodelist[[paste0("v2x_libdem", year)]]
  network %v% "lngdp" <- nodelist[[paste0("lngdp", year)]]
  network %v% "lnpop" <- nodelist[[paste0("lnpop", year)]]
  network %v% "lnauthors" <- nodelist[[paste0("lnauthors", year)]]
  network %v% "urban" <- nodelist[[paste0("urb", year)]]
  network %v% "region" <- nodelist$e_regionpol
  network %v% "country" <- nodelist$country_code
  network
}

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv",
    sep = ",",
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  ) %>%
  as.matrix() %>%
  as.network(
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

years <- 2008:2017
networks <- lapply(years, read_and_process_matrix)

NODELIST01132022 <-
  read.csv("NODELIST.csv", sep = ",", header = TRUE)

networks <-
  Map(add_vertex_attributes,
      networks,
      years,
      MoreArgs = list(nodelist = NODELIST01132022))

# procedure

ptm <- proc.time()

set.seed(101)

btergm.bin <- btergm(
  networks
  ~ edges
  + gwdegree(0.5, fixed = TRUE)
  + gwesp(0.25, fixed = TRUE)
  + memory(type = "autoregression", lag = 1)
  + timecov(
    transform = function(t)
      t)
  + nodecov("libdem")
  + absdiff("libdem")
  + nodecov("lnauthors")
  + absdiff("lnauthors")
  + nodecov("urban")
  + absdiff("urban")
  + nodecov("lngdp")
  + absdiff("lngdp")
  + nodecov("lnpop")
  + absdiff("lnpop")
  + nodefactor("region")
  + nodematch("region")
  + edgecov(distmatrixfinal),
  R = 1500,
  parallel = c("snow"),
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm

summary_btergm_bin <- summary(btergm.bin)

#results_df

# save(btergm.bin, file="btergm.bin.rda")

# load("btergm.bin.rda")
# if you have statnet installed in addition to btergm this wont return the stats, even specifying btergm:: does not work. load only tidyverse, btergm, and backbone

#gofplot

par(mfrow = c(1, 3))
gof.btergm.bin <-
  gof(btergm.bin,
      nsim = 100,
      statistics = c(esp, deg, geodesic))

save(gof.btergm.bin, file = "gof.btergm.bin.rda")

rm(networks)

# BTERGM.BACK.5 ---------------------------------------------------------------

read_and_process_matrix <- function(year) {
  file_name <- paste0("matrix", year, "rem.csv")
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  matrix_net <-
    disparity(matrix_adj,
              alpha = 0.5,
              narrative = TRUE,
              class = "Matrix") %>% as.matrix()
  as.network(
    matrix_net,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )
}

add_vertex_attributes <- function(network, year, nodelist) {
  network %v% "libdem" <- nodelist[[paste0("v2x_libdem", year)]]
  network %v% "lngdp" <- nodelist[[paste0("lngdp", year)]]
  network %v% "lnpop" <- nodelist[[paste0("lnpop", year)]]
  network %v% "lnauthors" <- nodelist[[paste0("lnauthors", year)]]
  network %v% "urban" <- nodelist[[paste0("urb", year)]]
  network %v% "region" <- nodelist$e_regionpol
  network %v% "country" <- nodelist$country_code
  network
}

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv",
    sep = ",",
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  ) %>%
  as.matrix() %>%
  as.network(
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

years <- 2008:2017
networks <- lapply(years, read_and_process_matrix)

NODELIST01132022 <-
  read.csv("NODELIST.csv", sep = ",", header = TRUE)

networks <-
  Map(add_vertex_attributes,
      networks,
      years,
      MoreArgs = list(nodelist = NODELIST01132022))

# procedure

ptm <- proc.time()

set.seed(101)

btergm.back.5 <- btergm(
  networks
  ~ edges
  + gwdegree(0.5, fixed = TRUE)
  + gwesp(0.25, fixed = TRUE)
  + memory(type = "autoregression", lag = 1)
  + timecov(
    transform = function(t)
      t)
  + nodecov("libdem")
  + absdiff("libdem")
  + nodecov("lnauthors")
  + absdiff("lnauthors")
  + nodecov("urban")
  + absdiff("urban")
  + nodecov("lngdp")
  + absdiff("lngdp")
  + nodecov("lnpop")
  + absdiff("lnpop")
  + nodefactor("region")
  + nodematch("region")
  + edgecov(distmatrixfinal),
  R = 1500,
  parallel = c("snow"),
  ncpus = 5,
  verbose = TRUE
)

proc.time() - ptm

summary_btergm_back5 <- summary(btergm.back.5)

save(btergm.back.5, file = "btergm.back.5.rda")

#gofplot

par(mfrow = c(1, 3))
gof.btergm.back.5 <-
  gof(btergm.back.5,
      nsim = 100,
      statistics = c(esp, deg, geodesic))
plot(gof.btergm.back.5)

save(gof.btergm.back.5, file = "gof.btergm.back.5.rda")

rm(networks)

# BTERGM.BACK.25 ---------------------------------------------------------------

read_and_process_matrix <- function(year) {
  file_name <- paste0("matrix", year, "rem.csv")
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  matrix_net <-
    disparity(matrix_adj,
              alpha = 0.25,
              narrative = TRUE,
              class = "Matrix") %>% as.matrix()
  as.network(
    matrix_net,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )
}

add_vertex_attributes <- function(network, year, nodelist) {
  network %v% "libdem" <- nodelist[[paste0("v2x_libdem", year)]]
  network %v% "lngdp" <- nodelist[[paste0("lngdp", year)]]
  network %v% "lnpop" <- nodelist[[paste0("lnpop", year)]]
  network %v% "lnauthors" <- nodelist[[paste0("lnauthors", year)]]
  network %v% "urban" <- nodelist[[paste0("urb", year)]]
  network %v% "region" <- nodelist$e_regionpol
  network %v% "country" <- nodelist$country_code
  network
}

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv",
    sep = ",",
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  ) %>%
  as.matrix() %>%
  as.network(
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

years <- 2008:2017
networks <- lapply(years, read_and_process_matrix)

NODELIST01132022 <- read.csv("NODELIST.csv", sep = ",", header = TRUE)

networks <-
  Map(add_vertex_attributes,
      networks,
      years,
      MoreArgs = list(nodelist = NODELIST01132022))

#procedure

ptm <- proc.time()

set.seed(101)

btergm.back.25 <- btergm(
  networks
  ~ edges
  + gwdegree(0.5, fixed = TRUE)
  + gwesp(0.25, fixed = TRUE)
  + memory(type = "autoregression", lag = 1)
  + timecov(
    transform = function(t)
      t
  )
  + nodecov("libdem")
  + absdiff("libdem")
  + nodecov("lnauthors")
  + absdiff("lnauthors")
  + nodecov("urban")
  + absdiff("urban")
  + nodecov("lngdp")
  + absdiff("lngdp")
  + nodecov("lnpop")
  + absdiff("lnpop")
  + nodefactor("region")
  + nodematch("region")
  + edgecov(distmatrixfinal),
  R = 1500,
  parallel = c("snow"),
  ncpus = 5,
  verbose = TRUE
)

summary_btergm_back25<-summary(btergm.back.25)

proc.time() - ptm

save(btergm.back.25, file="btergm.back.25.rda")

#gofplot

par(mfrow = c(1, 3))
gof.btergm.back.25 <-
  gof(btergm.back.25,
      nsim = 100,
      statistics = c( esp, deg, geodesic))
plot(gof.btergm.back.25)

save(gof.btergm.back.25, file="gof.btergm.back.25.rda")

rm(networks)

# BTERGM.BACK.05 ---------------------------------------------------------------

read_and_process_matrix <- function(year) {
  file_name <- paste0("matrix", year, "rem.csv")
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  matrix_net <-
    disparity(matrix_adj,
              alpha = 0.05,
              narrative = TRUE,
              class = "Matrix") %>% as.matrix()
  as.network(
    matrix_net,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )
}

add_vertex_attributes <- function(network, year, nodelist) {
  network %v% "libdem" <- nodelist[[paste0("v2x_libdem", year)]]
  network %v% "lngdp" <- nodelist[[paste0("lngdp", year)]]
  network %v% "lnpop" <- nodelist[[paste0("lnpop", year)]]
  network %v% "lnauthors" <- nodelist[[paste0("lnauthors", year)]]
  network %v% "urban" <- nodelist[[paste0("urb", year)]]
  network %v% "region" <- nodelist$e_regionpol
  network %v% "country" <- nodelist$country_code
  network
}

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv",
    sep = ",",
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  ) %>%
  as.matrix() %>%
  as.network(
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

years <- 2008:2017
networks <- lapply(years, read_and_process_matrix)

NODELIST01132022 <- read.csv("NODELIST.csv", sep = ",", header = TRUE)

networks <-
  Map(add_vertex_attributes,
      networks,
      years,
      MoreArgs = list(nodelist = NODELIST01132022))

#procedure

ptm <- proc.time()

set.seed(101)

btergm.back.05 <- btergm(
  networks
  ~ edges
  + gwdegree(0.5, fixed = TRUE)
  + gwesp(0.25, fixed = TRUE)
  + memory(type = "autoregression", lag = 1)
  + timecov(
    transform = function(t)
      t
  )
  + nodecov("libdem")
  + absdiff("libdem")
  + nodecov("lnauthors")
  + absdiff("lnauthors")
  + nodecov("urban")
  + absdiff("urban")
  + nodecov("lngdp")
  + absdiff("lngdp")
  + nodecov("lnpop")
  + absdiff("lnpop")
  + nodefactor("region")
  + nodematch("region")
  + edgecov(distmatrixfinal),
  R = 1500,
  parallel = c("snow"),
  ncpus = 5,
  verbose = TRUE
)

summary_btergm_back05<-summary(btergm.back.05)

proc.time() - ptm

save(btergm.back.05, file="btergm.back.05.rda")

#gofplot

par(mfrow = c(1, 3))
gof.btergm.back.05 <-
  gof(btergm.back.05,
      nsim = 100,
      statistics = c(esp, deg, geodesic))
plot(gof.btergm.back.05)

save(gof.btergm.back.05, file = "gof.btergm.back.05.rda")


# export gof plots

library(gridExtra)
pdf("gof.btergm.pdf", width = 7.5, height = 2.5)
p1 <- plot(gof.btergm.bin)
p2 <- plot(gof.btergm.back.5)
p3 <- plot(gof.btergm.back.25)
p4 <- plot(gof.btergm.back.05)
dev.off()

# put all the results in a dataframe

process_results <- function(summary_btergm) {
  boot_mean_rounded <- round(summary_btergm[, "Boot mean"], 3)
  lower_bound_rounded <- round(summary_btergm[, "2.5%"], 3)
  upper_bound_rounded <- round(summary_btergm[, "97.5%"], 3)
  
  asterisks <- mapply(function(lower, upper) {
    if (lower > 0 || upper < 0)
      "*"
    else
      ""
  }, lower_bound_rounded, upper_bound_rounded)
  
  paste0(
    boot_mean_rounded,
    " (",
    lower_bound_rounded,
    ", ",
    upper_bound_rounded,
    ")",
    asterisks
  )
}

variable_names <- rownames(summary_btergm_bin)
results_df1 <-
  data.frame(Variable = variable_names, stringsAsFactors = FALSE)
results_df1$Boot_mean_and_CI <- process_results(summary_btergm_bin)
results_df1$Boot_mean_and_CI_b5 <-
  process_results(summary_btergm_back5)
results_df1$Boot_mean_and_CI_b25 <-
  process_results(summary_btergm_back25)
results_df1$Boot_mean_and_CI_b05 <-
  process_results(summary_btergm_back05)

results_df1

# save export btergm results

write.csv(results_df, file = "BTERGMresults04292023.csv", row.names = FALSE)

rm(networks)

# load("btergm.bin.rda") 
# load("btergm.back.5.rda") 
# load("btergm.back.25.rda") 
# load("btergm.back.05.rda") 

# btergm1 <- summary(btergm.bin)
# btergm2 <- summary(btergm.back.5)
# btergm3 <- summary(btergm.back.25)
# btergm4 <- summary(btergm.back.05)

# screenreg appears to be outputing the incorrect confidence intervals, actually looks like it is substituting the boot mean for the lower ci. I can't find a way to automate the output. So I will do it manually copy/paste to excel. 

# VIF for BTERGMs ----------------------------------------------------------------

# function copied from https://github.com/sduxbury/vif-ergm/blob/master/Collinearity%20test%20for%20ERGM.R

vif.ergm<-function(my.ergm){
  
  
  if(class(my.ergm)%in%"btergm"){
    data_mat<-my.ergm@effects
    corr5<-stats::cor(data_mat[!rownames(data_mat)%in%"edges",
                               !colnames(data_mat)%in%"edges"]) ##omit edges term
    beta<-btergm::coef(my.ergm)
  }else{
    
    #get correlation matrix
    if(class(my.ergm)%in%"mlergm"){
      cor.mat<-stats::cov2cor(solve(my.ergm$information_matrix))
      beta<-my.ergm$theta
      
    }else{
      cor.mat<-stats::cov2cor(my.ergm$covar) #calculate correlation matrix
      beta<-stats::coef(my.ergm)
      
    }
    
    #omit edges, assign names to matrix
    rownames(cor.mat)<-colnames(cor.mat)<-names(beta)
    corr5<-cor.mat[!rownames(cor.mat)%in%"edges",
                   !colnames(cor.mat)%in%"edges"]
  }
  
  corr5<-corr5[!is.na(corr5[1:nrow(corr5)]),]
  corr5<-corr5[,which(!is.na(corr5[1,1:ncol(corr5)]))]
  
  VIFS<-matrix(0,nrow=1,ncol=ncol(corr5))
  
  for(i in 1:ncol(corr5)){
    
    gvec<-as.vector(corr5[-c(i),i]) ##create vector of correlations between covariate of interest and other covariates in the model
    tgvec<-t(gvec)
    xcor<-solve(corr5[-c(i),-c(i)]) ##create square matrix of correlations between covariates in the model other than the one of interest
    Rsq<-tgvec%*%xcor%*%gvec
    VIFS[1,i]<-1/(1-Rsq)
  }
  
  colnames(VIFS)<-names(beta[!names(beta)%in%"edges"])
  
  if(class(my.ergm)%in%"btergm"){
    
    warning("VIFS for bootstrap TERGM based on model matrix, not the covariance matrix of the estimator. Benchmarks used for MCMC ML estimation may not apply.")
    
  }else{
    message("Higher values indicate greater correlation.\nVIF > 20 is concerning, VIF > 100 indicates severe collinearity.")
    
  }
  VIFS
}

vif.ergm(btergm.bin)
vif.ergm(btergm.back.5)
vif.ergm(btergm.back.25)
vif.ergm(btergm.back.05)

# no significant multi except endogenous terms on model bin

# VERGMs ------------------------------------------------------------------

# At first these models were run on GATECH HPC Servers, they take a very long time to run when transitive term is included, the earlier years are shorter but the last few years take upward of 75-200 hours. The transitivity term increases run time exponentially. It also appears to induce extreme multicollinearity. Further, it isn't even significant in several models. So I removed it. 

# 2008VERGM ---------------------------------------------------------------

detach(package:btergm)
detach(package:backbone)

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2008rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2008back <- as.matrix(binned_matrix_matrix)

network2008 <- as.network(matrix2008back,  matrix.type='adjacency',ignore.eval=FALSE,names.eval='weight', directed = FALSE)

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2008 %v% "libdem" <- NODELIST10252022$v2x_libdem2008
network2008 %v% "lngdp" <- NODELIST10252022$lngdp2008
network2008 %v% "lnpop" <- NODELIST10252022$lnpop2008
network2008 %v% "lnauthors" <- NODELIST10252022$lnauthors2008
network2008 %v% "urban" <- NODELIST10252022$urb2008
network2008 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2008a <- ergm(
  network2008 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  #+ transitiveweights("min","max","min")
  + +nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)

summary(VERGM2008a)

proc.time() - ptm

# 2009VERGM ---------------------------------------------------------------

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2009rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2009back <- as.matrix(binned_matrix_matrix)

network2009 <-
  as.network(
    matrix2009back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2009 %v% "libdem" <- NODELIST10252022$v2x_libdem2009
network2009 %v% "lngdp" <- NODELIST10252022$lngdp2009
network2009 %v% "lnpop" <- NODELIST10252022$lnpop2009
network2009 %v% "lnauthors" <- NODELIST10252022$lnauthors2009
network2009 %v% "urban" <- NODELIST10252022$urb2009
network2009 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2009a <- ergm(
  network2009 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  #+ transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2009a)
proc.time() - ptm

# 2010VERGM ---------------------------------------------------------------

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2010rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2010back <- as.matrix(binned_matrix_matrix)

network2010 <-
  as.network(
    matrix2010back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2010 %v% "libdem" <- NODELIST10252022$v2x_libdem2010
network2010 %v% "lngdp" <- NODELIST10252022$lngdp2010
network2010 %v% "lnpop" <- NODELIST10252022$lnpop2010
network2010 %v% "lnauthors" <- NODELIST10252022$lnauthors2010
network2010 %v% "urban" <- NODELIST10252022$urb2010
network2010 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2010a <- ergm(
  network2010 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  # + transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2010a)
proc.time() - ptm

# 2011VERGM ---------------------------------------------------------------

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2011rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2011back <- as.matrix(binned_matrix_matrix)

network2011 <-
  as.network(
    matrix2011back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2011 %v% "libdem" <- NODELIST10252022$v2x_libdem2011
network2011 %v% "lngdp" <- NODELIST10252022$lngdp2011
network2011 %v% "lnpop" <- NODELIST10252022$lnpop2011
network2011 %v% "lnauthors" <- NODELIST10252022$lnauthors2011
network2011 %v% "urban" <- NODELIST10252022$urb2011
network2011 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2011a <- ergm(
  network2011 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  #+ transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2011a)
proc.time() - ptm

# 2012VERGM ---------------------------------------------------------------

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2012rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2012back <- as.matrix(binned_matrix_matrix)

network2012 <-
  as.network(
    matrix2012back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2012 %v% "libdem" <- NODELIST10252022$v2x_libdem2012
network2012 %v% "lngdp" <- NODELIST10252022$lngdp2012
network2012 %v% "lnpop" <- NODELIST10252022$lnpop2012
network2012 %v% "lnauthors" <- NODELIST10252022$lnauthors2012
network2012 %v% "urban" <- NODELIST10252022$urb2012
network2012 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2012a <- ergm(
  network2012 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  # + transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2012a)
proc.time() - ptm

# 2013VERGM ---------------------------------------------------------------

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2013rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2013back <- as.matrix(binned_matrix_matrix)

network2013 <-
  as.network(
    matrix2013back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2013 %v% "libdem" <- NODELIST10252022$v2x_libdem2013
network2013 %v% "lngdp" <- NODELIST10252022$lngdp2013
network2013 %v% "lnpop" <- NODELIST10252022$lnpop2013
network2013 %v% "lnauthors" <- NODELIST10252022$lnauthors2013
network2013 %v% "urban" <- NODELIST10252022$urb2013
network2013 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2013a <- ergm(
  network2013 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  # + transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2013a)
proc.time() - ptm

# 2014VERGM ---------------------------------------------------------------

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2014rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2014back <- as.matrix(binned_matrix_matrix)

network2014 <-
  as.network(
    matrix2014back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2014 %v% "libdem" <- NODELIST10252022$v2x_libdem2014
network2014 %v% "lngdp" <- NODELIST10252022$lngdp2014
network2014 %v% "lnpop" <- NODELIST10252022$lnpop2014
network2014 %v% "lnauthors" <- NODELIST10252022$lnauthors2014
network2014 %v% "urban" <- NODELIST10252022$urb2014
network2014 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2014a <- ergm(
  network2014 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  #+ transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2014a)
proc.time() - ptm

# 2015VERGM ---------------------------------------------------------------


distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2015rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2015back <- as.matrix(binned_matrix_matrix)

network2015 <-
  as.network(
    matrix2015back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2015 %v% "libdem" <- NODELIST10252022$v2x_libdem2015
network2015 %v% "lngdp" <- NODELIST10252022$lngdp2015
network2015 %v% "lnpop" <- NODELIST10252022$lnpop2015
network2015 %v% "lnauthors" <- NODELIST10252022$lnauthors2015
network2015 %v% "urban" <- NODELIST10252022$urb2015
network2015 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2015a <- ergm(
  network2015 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  #+ transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2015a)
proc.time() - ptm


# 2016VERGM ---------------------------------------------------------------


distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2016rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2016back <- as.matrix(binned_matrix_matrix)

network2016 <-
  as.network(
    matrix2016back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2016 %v% "libdem" <- NODELIST10252022$v2x_libdem2016
network2016 %v% "lngdp" <- NODELIST10252022$lngdp2016
network2016 %v% "lnpop" <- NODELIST10252022$lnpop2016
network2016 %v% "lnauthors" <- NODELIST10252022$lnauthors2016
network2016 %v% "urban" <- NODELIST10252022$urb2016
network2016 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2016a <- ergm(
  network2016 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  #+ transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2016a)
proc.time() - ptm


# 2017VERGM ---------------------------------------------------------------

distmatrixfinal <-
  read.csv(
    "distmatrixfinal.csv" ,
    sep = "," ,
    header = TRUE,
    row.names = 1,
    check.names = FALSE,
    na.strings = ""
  )
distmatrixfinal <- as.matrix(distmatrixfinal)
distmatrixfinal = as.network(
  distmatrixfinal ,
  matrix.type = 'adjacency',
  ignore.eval = FALSE,
  names.eval = 'weight',
  directed = FALSE
)

matrix <-
  as.matrix(
    read.csv(
      "matrix2017rem.csv",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = "",
      sep = ","
    )
  )
matrix[matrix == 0] <- NA
matrix <- log(matrix + 1)
bin_cut_points <-
  unique(quantile(matrix, probs = seq(0, 1, by = 1 / 6), na.rm = TRUE))
binned_matrix <-
  as.numeric(cut(matrix, breaks = bin_cut_points, labels = FALSE))
binned_matrix_matrix <-
  matrix(binned_matrix,
         nrow = nrow(matrix),
         ncol = ncol(matrix))
binned_matrix_matrix[is.na(binned_matrix_matrix)] <- 0
rownames(binned_matrix_matrix) <- row.names(matrix)
colnames(binned_matrix_matrix) <- colnames(matrix)
matrix2017back <- as.matrix(binned_matrix_matrix)

network2017 <-
  as.network(
    matrix2017back,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )

NODELIST10252022 <-
  read.csv("NODELIST.csv" , sep = "," , header = TRUE)

network2017 %v% "libdem" <- NODELIST10252022$v2x_libdem2017
network2017 %v% "lngdp" <- NODELIST10252022$lngdp2017
network2017 %v% "lnpop" <- NODELIST10252022$lnpop2017
network2017 %v% "lnauthors" <- NODELIST10252022$lnauthors2017
network2017 %v% "urban" <- NODELIST10252022$urb2017
network2017 %v% "region" <- NODELIST10252022$e_regionpol

ptm <- proc.time()

VERGM2017a <- ergm(
  network2017 ~  sum
  + nonzero
  + nodecovar(center = TRUE, transform = "sqrt")
  #+ transitiveweights("min","max","min")+nodecov("libdem", form = "sum")
  + absdiff("libdem", form = "sum")
  + nodecov("lnauthors", form = "sum")
  + absdiff("lnauthors", form = "sum")
  + nodecov("urban", form = "sum")
  + absdiff("urban", form = "sum")
  + nodecov("lngdp", form = "sum")
  + absdiff("lngdp", form = "sum")
  + nodecov("lnpop", form = "sum")
  + absdiff("lnpop", form = "sum")
  + nodefactor("region", form = "sum")
  + nodematch("region", form = "sum")
  + edgecov(distmatrixfinal, form = "sum")
  ,
  response = "weight",
  reference = ~ Binomial(5),
  eval.loglik = FALSE,
  control = control.ergm(
    seed = 101,
    MCMC.samplesize = 15000,
    parallel = 7,
    parallel.type = "PSOCK"
  )
)
summary(VERGM2017a)
proc.time() - ptm

#

save(VERGM2008a, file = "VERGM2008a.rda")
save(VERGM2009a, file = "VERGM2009a.rda")
save(VERGM2010a, file = "VERGM2010a.rda")
save(VERGM2011a, file = "VERGM2011a.rda")
save(VERGM2012a, file = "VERGM2012a.rda")
save(VERGM2013a, file = "VERGM2013a.rda")
save(VERGM2014a, file = "VERGM2014a.rda")
save(VERGM2015a, file = "VERGM2015a.rda")
save(VERGM2016a, file = "VERGM2016a.rda")
save(VERGM2017a, file = "VERGM2017a.rda")

htmlreg(
  list(
    VERGM2008a,
    VERGM2009a,
    VERGM2010a,
    VERGM2011a,
    VERGM2012a,
    VERGM2013a,
    VERGM2014a,
    VERGM2015a,
    VERGM2016a,
    VERGM2017a
  ),
  digits = 3,
  file = "VERGMRESULTSFINAL04292023.doc"
)


# VIF VERGM ---------------------------------------------------------------
# these vifs all appear to be pretty good
model_list <-
  list(
    VERGM2008a,
    VERGM2009a,
    VERGM2010a,
    VERGM2011a,
    VERGM2012a,
    VERGM2013a,
    VERGM2014a,
    VERGM2015a,
    VERGM2016a,
    VERGM2017a
  )

for (i in seq_along(model_list)) {
  year <- 2008 + i - 1
  vif <- vif.ergm(model_list[[i]])
  print(paste0("VIF for VERGM", year, "a:"))
  print(vif)
}

#the following vifs were run on models which contained the transitivity term. including transitivity induced a significant amount of variance inflation into the model compared to the vif numbers above in models that do not include the transitivity term. 

# model_listb <-
#   list(
#     VERGM2008b,
#     VERGM2009b,
#     VERGM2010b,
#     VERGM2011b,
#     VERGM2012b,
#     VERGM2013b,
#     VERGM2014b,
#     VERGM2015b,
#     VERGM2016b,
#     VERGM2017b
#   )

# for (i in seq_along(model_listb)) {
#   year <- 2008 + i - 1
#   vif <- vif.ergm(model_list[[i]])
#   print(paste0("VIF for VERGM", year, "b:"))
#   print(vif)
# }

# REVIEWER CONCERNS & GRAPHICS ---------------------------------------------------------------

# CREATING PANEL DATA ---------------------------------------------------------------

read_and_process_matrix <- function(year) {
  file_name <- paste0("matrix", year, "rem.csv")
  matrix_data <-
    read.csv(
      file_name,
      sep = ",",
      header = TRUE,
      row.names = 1,
      check.names = FALSE,
      na.strings = ""
    )
  matrix_adj <- as.matrix(matrix_data)
  as.network(
    matrix_adj,
    matrix.type = 'adjacency',
    ignore.eval = FALSE,
    names.eval = 'weight',
    directed = FALSE
  )
}

years <- 2008:2017
networks <- lapply(years, read_and_process_matrix)

network2008 <- (networks[[1]])
network2009 <- (networks[[2]])
network2010 <- (networks[[3]])
network2011 <- (networks[[4]])
network2012 <- (networks[[5]])
network2013 <- (networks[[6]])
network2014 <- (networks[[7]])
network2015 <- (networks[[8]])
network2016 <- (networks[[9]])
network2017 <- (networks[[10]])

adj_matrix_2008 <- as.matrix(network2008, attr = "weight")
adj_matrix_2009 <- as.matrix(network2009, attr = "weight")
adj_matrix_2010 <- as.matrix(network2010, attr = "weight")
adj_matrix_2011 <- as.matrix(network2011, attr = "weight")
adj_matrix_2012 <- as.matrix(network2012, attr = "weight")
adj_matrix_2013 <- as.matrix(network2013, attr = "weight")
adj_matrix_2014 <- as.matrix(network2014, attr = "weight")
adj_matrix_2015 <- as.matrix(network2015, attr = "weight")
adj_matrix_2016 <- as.matrix(network2016, attr = "weight")
adj_matrix_2017 <- as.matrix(network2017, attr = "weight")

node_weights_2008 <- rowSums(adj_matrix_2008)
node_weights_2009 <- rowSums(adj_matrix_2009)
node_weights_2010 <- rowSums(adj_matrix_2010)
node_weights_2011 <- rowSums(adj_matrix_2011)
node_weights_2012 <- rowSums(adj_matrix_2012)
node_weights_2013 <- rowSums(adj_matrix_2013)
node_weights_2014 <- rowSums(adj_matrix_2014)
node_weights_2015 <- rowSums(adj_matrix_2015)
node_weights_2016 <- rowSums(adj_matrix_2016)
node_weights_2017 <- rowSums(adj_matrix_2017)

combined_vector <- c(
  node_weights_2008,
  node_weights_2009,
  node_weights_2010,
  node_weights_2011,
  node_weights_2012,
  node_weights_2013,
  node_weights_2014,
  node_weights_2015,
  node_weights_2016,
  node_weights_2017
)
df <- as.data.frame(combined_vector)

row_names <- c(
  names(node_weights_2008),
  names(node_weights_2009),
  names(node_weights_2010),
  names(node_weights_2011),
  names(node_weights_2012),
  names(node_weights_2013),
  names(node_weights_2014),
  names(node_weights_2015),
  names(node_weights_2016),
  names(node_weights_2017)
)

df <- data.frame(row_names, combined_vector)


names(df)[2] <- "sum_weights"

names(df)[1] <- "country_code"

year_vec <- rep(2008:2017, each = 170)
df$year <- year_vec

####

NODELIST01132022 <-
  read.csv("NODELIST.csv", sep = ",", header = TRUE)

sub_df <-
  NODELIST01132022[, c(
    "country_code",
    "v2x_libdem2008",
    "v2x_libdem2009",
    "v2x_libdem2010",
    "v2x_libdem2011",
    "v2x_libdem2012",
    "v2x_libdem2013",
    "v2x_libdem2014",
    "v2x_libdem2015",
    "v2x_libdem2016",
    "v2x_libdem2017",
    "lngdp2008",
    "lngdp2009",
    "lngdp2010",
    "lngdp2011",
    "lngdp2012",
    "lngdp2013",
    "lngdp2014",
    "lngdp2015",
    "lngdp2016",
    "lngdp2017" ,
    "lnpop2008",
    "lnpop2009",
    "lnpop2010",
    "lnpop2011",
    "lnpop2012",
    "lnpop2013",
    "lnpop2014",
    "lnpop2015",
    "lnpop2016",
    "lnpop2017",
    "urb2008",
    "urb2009",
    "urb2010",
    "urb2011",
    "urb2012",
    "urb2013",
    "urb2014",
    "urb2015",
    "urb2016",
    "urb2017",
    "lnauthors2008",
    "lnauthors2009",
    "lnauthors2010",
    "lnauthors2011",
    "lnauthors2012",
    "lnauthors2013",
    "lnauthors2014",
    "lnauthors2015",
    "lnauthors2016",
    "lnauthors2017",
    "e_regionpol"
  )]

new_df1 <- reshape(
  sub_df,
  varying = list(
    c(
      "v2x_libdem2008",
      "v2x_libdem2009",
      "v2x_libdem2010",
      "v2x_libdem2011",
      "v2x_libdem2012",
      "v2x_libdem2013",
      "v2x_libdem2014",
      "v2x_libdem2015",
      "v2x_libdem2016",
      "v2x_libdem2017"
    ),
    c(
      "lngdp2008",
      "lngdp2009",
      "lngdp2010",
      "lngdp2011",
      "lngdp2012",
      "lngdp2013",
      "lngdp2014",
      "lngdp2015",
      "lngdp2016",
      "lngdp2017"
    ),
    c(
      "lnpop2008",
      "lnpop2009",
      "lnpop2010",
      "lnpop2011",
      "lnpop2012",
      "lnpop2013",
      "lnpop2014",
      "lnpop2015",
      "lnpop2016",
      "lnpop2017"
    ),
    c(
      "urb2008",
      "urb2009",
      "urb2010",
      "urb2011",
      "urb2012",
      "urb2013",
      "urb2014",
      "urb2015",
      "urb2016",
      "urb2017"
    ),
    c(
      "lnauthors2008",
      "lnauthors2009",
      "lnauthors2010",
      "lnauthors2011",
      "lnauthors2012",
      "lnauthors2013",
      "lnauthors2014",
      "lnauthors2015",
      "lnauthors2016",
      "lnauthors2017"
    )
  ),
  v.names = c("libdem", "lngdp", "lnpop", "urb", "lnauthors"),
  timevar = "year",
  times = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017),
  direction = "long"
)

names(new_df1)[2] <- "region"


new_df1$year <- as.integer(new_df1$year)
new_df1 <- new_df1 %>%
  left_join(df,
            by = c("country_code", "year"),
            select(df, country_code, year, sum_weight))


b_2008 <- ifelse(adj_matrix_2008  != 0, 1, 0)
b_2009 <- ifelse(adj_matrix_2009  != 0, 1, 0)
b_2010 <- ifelse(adj_matrix_2010  != 0, 1, 0)
b_2011 <- ifelse(adj_matrix_2011  != 0, 1, 0)
b_2012 <- ifelse(adj_matrix_2012  != 0, 1, 0)
b_2013 <- ifelse(adj_matrix_2013  != 0, 1, 0)
b_2014 <- ifelse(adj_matrix_2014  != 0, 1, 0)
b_2015 <- ifelse(adj_matrix_2015  != 0, 1, 0)
b_2016 <- ifelse(adj_matrix_2016  != 0, 1, 0)
b_2017 <- ifelse(adj_matrix_2017  != 0, 1, 0)

node_2008 <- rowSums(b_2008)
node_2009 <- rowSums(b_2009)
node_2010 <- rowSums(b_2010)
node_2011 <- rowSums(b_2011)
node_2012 <- rowSums(b_2012)
node_2013 <- rowSums(b_2013)
node_2014 <- rowSums(b_2014)
node_2015 <- rowSums(b_2015)
node_2016 <- rowSums(b_2016)
node_2017 <- rowSums(b_2017)

combined_vector1 <- c(
  node_2008 ,
  node_2009 ,
  node_2010 ,
  node_2011 ,
  node_2012 ,
  node_2013 ,
  node_2014 ,
  node_2015 ,
  node_2016 ,
  node_2017
)
df1 <- as.data.frame(combined_vector1)

row_names1 <- c(
  names(node_2008),
  names(node_2009),
  names(node_2010),
  names(node_2011),
  names(node_2012),
  names(node_2013),
  names(node_2014),
  names(node_2015),
  names(node_2016),
  names(node_2017)
)
df1 <- data.frame(row_names1, combined_vector1)

year_vec <- rep(2008:2017, each = 170)
df1$year <- year_vec

names(df1)[1] <- "country_code"
names(df1)[2] <- "degree"

# names(new_df1)[9] <- "weight"

new_df1 <- new_df1 %>%
  left_join(df1,
            by = c("country_code", "year"),
            select(df1, country_code, year, degree))
names(new_df1)[10] <- "weight"

new_df1 <- arrange(new_df1, id, year)
panel_data_p <-
  pdata.frame(new_df1, index = c("country_code", "year"))
panel_data_ps$region <- as.factor(panel_data_ps$region)

# GLMER TESTS ---------------------------------------------------------------

## edge weights are a count variable, but they have overdispersion, so negative binomial
## get dispersion theta per this https://statisticalhorizons.com/fe-nbreg/ and this https://rdrr.io/cran/lme4/man/glmer.nb.html
# model implodes, needs rescaled variables

scaled_vars <- c("libdem", "lngdp", "lnauthors", "lnpop", "urb")
panel_data_ps <- panel_data_p
panel_data_ps[scaled_vars] <- scale(panel_data_ps[scaled_vars])
panel_data_ps$year_n <- as.numeric(panel_data_ps$year)

# basically I am trying to replicate my ERGM model, but get country*libdem slopes for a graph, including year in the model overidentifies it so I added it as a random effect, leaving time out seems to wreck the model, probably because as we know collaboration is increasing.

model <-
  glmer.nb(
    weight ~ libdem + lngdp + lnauthors + lnpop + urb + year + (1 + libdem |
                                                                  country_code / region),
    data = panel_data_ps,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
getME(model, "glmer.nb.theta")

#the model didnt converge but I have the dispersion number I want, I think it is the glmer.nb function, because it converges fine in the next model w/glmer

#[1] 9.350832

## extract country specific effects of libden on sum tie weights over time, eliminates low variation countries, and provides estinates only with reference country. multilevel allows estimation of slopes for all countries.

model <-
  glmer(
    weight ~ libdem + lngdp + lnauthors + lnpop + urb + year + (1 + libdem |
                                                                  country_code / region) ,
    data = panel_data_ps,
    family = negative.binomial(theta = 9.350832) ,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
summary(model)

ranef_model <- ranef(model)$country_code

results <- data.frame(country_code = rownames(ranef_model),
                      slope = ranef_model[, 2])

### democracy difference

country_ids <-
  factor(panel_data_p$country_code,
         levels = unique(panel_data_p$country_code))

country_ids <-
  factor(country_ids, levels = unique(panel_data_p$country_code))
dem_differences <- numeric(length = length(levels(country_ids)))

for (i in seq_along(levels(country_ids))) {
  country_data <-
    panel_data_p$libdem[panel_data_p$country_code == levels(country_ids)[i]]
  dem_difference <-
    tail(country_data, n = 1) - head(country_data, n = 1)
  dem_differences[i] <- dem_difference
}

#### weight difference
country_ids <-
  factor(country_ids, levels = unique(panel_data_p$country_code))
w_differences <- numeric(length = length(levels(country_ids)))

for (i in seq_along(levels(country_ids))) {
  country_data <-
    panel_data_p$weight[panel_data_p$country_code == levels(country_ids)[i]]
  w_difference <-
    tail(country_data, n = 1) - head(country_data, n = 1)
  w_differences[i] <- w_difference
}

## democracy average
country_ids <-
  factor(country_ids, levels = unique(panel_data_p$country_code))
dem_averages <- numeric(length = length(levels(country_ids)))

for (i in seq_along(levels(country_ids))) {
  country_data <-
    panel_data_p$libdem[panel_data_p$country_code == levels(country_ids)[i]]
  dem_average <- mean(country_data, na.rm = TRUE)
  dem_averages[i] <- dem_average
}

# weight sum
country_ids <-
  factor(country_ids, levels = unique(panel_data_p$country_code))
w_total <- numeric(length = length(levels(country_ids)))

for (i in seq_along(levels(country_ids))) {
  country_data <-
    panel_data_p$weight[panel_data_p$country_code == levels(country_ids)[i]]
  w_total[i] <- sum(country_data, na.rm = TRUE)
}

# combine into new frame

results_dem <-
  data.frame(
    country_code = levels(country_ids),
    w_differences = w_differences,
    dem_differences = dem_differences,
    dem_averages = dem_averages,
    w_total = w_total
  )

results_dem$dem_averages_breaks <- cut(
  results_dem$dem_averages,
  breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  include.lowest = TRUE
)


##

panel_data_2017 <- panel_data_p %>% filter(year == 2017)
libdem_2017a <- tibble(libdem_2017 = panel_data_2017$libdem,
                       country_code = panel_data_2017$country_code)
libdem_2017 <- as.data.frame(libdem_2017a)
results_dem <- merge(results_dem, libdem_2017, by = "country_code")

results_dem$dem_2017_breaks <- cut(
  results_dem$libdem_2017,
  breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  include.lowest = TRUE
)

results_dem <- merge(results_dem, results, by = "country_code")

# make interaction term for plots, keep signs if negatives are used

# this was more circuitous that I anticipated, creating an interaction term between variables that contain negative numbers, then plotting the interaction by unit has the wrong interpretation, for example if country has negative change on dem and negative change on weight the result it positive and the country will appear next to other countries with positive change on both. This isn't so much of a problem because weight only has one country with negative change SYR. However, when rescaling the weight variable to make it more interpretable in the plot, lot of negatives introduced. 

sign <- function(x) {
  ifelse(x > 0, 1, ifelse(x < 0, -1, 0))
}

results_dem$dem_diff <- results_dem$dem_difference
results_dem$d_sign <- sign(results_dem$dem_diff)
results_dem$w_diff <- (results_dem$w_difference)
cond <- sign(results_dem$dem_diff) == -1 & sign(results_dem$w_diff) == -1
results_dem$Dem_Weight[cond] <- -abs(results_dem$Dem_Weight[cond])

# now try log , ill prob go with this one, since the other way is sensitive to weight outliers

results_dem$w_diff_ln <- log(results_dem$w_difference+115)
results_dem$Dem_Weight_ln<- results_dem$w_diff_ln*results_dem$dem_diff

results_dem$w_total_ln <- log(results_dem$w_total)

results_dem$Dem_Weight_TOT<- results_dem$dem_averages*results_dem$w_total_ln


#### average democracy score 
stats <-
  c(
    min = 0.0063,
    q1 = 0.1683,
    median = 0.4037,
    q3 = 0.6579,
    max = 0.8903
  )

# PLOTS ---------------------------------------------------------------

#plots dem averages
dev.off()


# create the plots (replace with your actual ggplot objects)
#dem average
plot1 <-
  ggplot(data = na.omit(results_dem), aes(x = reorder(country_code, -dem_averages), y = dem_averages)) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(
    expand = expansion(mult = c(0.02, 0.02)),
    limits = c(max(stats), min(stats)),
    breaks = stats,
    labels = stats
  ) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)),
                   limits = rev(levels(
                     reorder(results_dem$country_code, -results_dem$dem_averages)
                   ))) +
  geom_hline(yintercept = stats,
             color = "grey",
             linetype = "dotted") +
  labs(x = "Country", y = "Average Democracy Score 2008-2017") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none"
  ) +
  geom_point(size = 1.5,
             pch = 22,
             aes(fill = dem_averages_breaks))


#dem difference, for this plot the year 2017 libdem score is used

plot2 <-
  ggplot(data = na.omit(results_dem),
         aes(
           x = reorder(country_code, -dem_differences),
           y = dem_differences,
           fill = dem_2017_breaks
         )) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(expand = expansion(c(0.02, 0.02))) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)), limits = rev(levels(
    reorder(results_dem$country_code, -results_dem$dem_differences)
  ))) +
  labs(x = "", y = "Democracy Difference 2017-2008") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = c(0.9,0.9), legend.title = element_text(size = 8, face = "bold")
  )  +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  labs(fill = "Lib.Democracy") +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  )+
  geom_point(size = 1.5, pch = 22) 

grid.arrange(plot1, plot2,
             layout_matrix = rbind(c(1, 2)))

### total edge weight
###

plot3 <-
  ggplot(data = na.omit(results_dem), aes(x = reorder(country_code, -w_total), y = w_total)) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)),
                   limits = rev(levels(
                     reorder(results_dem$country_code, -results_dem$w_total)
                   ))) +
  geom_hline(yintercept = stats,
             color = "grey",
             linetype = "dashed") +
  labs(x = "", y = "Total Edge Weight") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey",
                                      linetype = "dotted"),
    legend.position = "none"
  ) +
  geom_point(size = 1.5,
             pch = 22,
             aes(fill = dem_averages_breaks))

# weight change difference colored by average democracy score

plot4 <-
  ggplot(data = na.omit(results_dem),
         aes(
           x = reorder(country_code, -w_differences),
           y = w_differences,
           fill = dem_averages_breaks
         )) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(expand = expansion(c(0.02, 0.02))) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)), limits = rev(levels(
    reorder(results_dem$country_code, -results_dem$w_differences)
  ))) +
  labs(x = "", y = "Edge Weight Diff. 2008-2017") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    legend.position = "none"
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_point(size = 1.5, pch = 22)

# dem effect on edge weight by country
plot5 <- ggplot(data = na.omit(results_dem),
                aes(
                  x = reorder(country_code,-slope),
                  y = slope,
                  fill = dem_averages_breaks
                )) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(expand = expansion(c(0.02, 0.02))) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)),
                   limits = rev(levels(
                     reorder(results_dem$country_code,-results_dem$slope)
                   ))) +
  labs(x = "", y = "Democracy on Edge Weights") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    legend.position = c(0.9, 0.9),
    legend.title = element_text(size = 8, face = "bold")
  ) +
  labs(fill = "Lib.Democracy") +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  )+
geom_point(size = 1.5, pch = 22) 


# INTERACTION TERM dem effect on edge weight by country
plot6 <- ggplot(data = na.omit(results_dem),
                aes(
                  x = reorder(country_code,-Dem_Weight),
                  y = Dem_Weight,
                  fill = dem_2017_breaks
                )) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(expand = expansion(c(0.02, 0.02))) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)),
                   limits = rev(levels(
                     reorder(results_dem$country_code,-results_dem$Dem_Weight)
                   ))) +
  labs(x = "", y = "Democracy Diff X Weight Diff") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    legend.position = c(0.9, 0.9),
    legend.title = element_text(size = 8, face = "bold")
  ) +
  labs(fill = "Lib.Democracy") +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  )+
  geom_point(size = 1.5, pch = 22) 






# LOG.  INTERACTION TERM dem effect on edge weight by country
plot7<- ggplot(data = na.omit(results_dem),
                aes(
                  x = reorder(country_code,-Dem_Weight_ln),
                  y = Dem_Weight_ln,
                  fill = dem_2017_breaks
                )) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(expand = expansion(c(0.02, 0.02))) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)),
                   limits = rev(levels(
                     reorder(results_dem$country_code,-results_dem$Dem_Weight_ln)
                   ))) +
  labs(x = "", y = "Democracy Difference X Edge Weight Difference") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    legend.position = c(0.9, 0.9),
    legend.title = element_text(size = 8, face = "bold")
  ) +
  labs(fill = "Lib.Democracy") +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  )+
  geom_point(size = 1.5, pch = 22) 


# LOG.  INTERACTION TERM dem effect on edge weight by country
plot8<- ggplot(data = na.omit(results_dem),
               aes(
                 x = reorder(country_code,-Dem_Weight_TOT),
                 y = Dem_Weight_TOT,
                 fill = dem_averages_breaks
               )) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", type = "div") +
  scale_y_reverse(expand = expansion(c(0.02, 0.02))) +
  scale_x_discrete(expand = expansion(c(0.007, 0.007)),
                   limits = rev(levels(
                     reorder(results_dem$country_code,-results_dem$Dem_Weight_TOT)
                   ))) +
  labs(x = "", y = "Democracy Average X Sum Edge Weight") +
  theme(
    axis.title.y = element_text(size = 0),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    legend.position = "none" 
  ) +
  
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  )+
  geom_point(size = 1.5, pch = 22) 


# arrange the plots in a grid
#margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
grid.arrange( plot8, plot7, 
             layout_matrix = rbind(c(1, 2)))