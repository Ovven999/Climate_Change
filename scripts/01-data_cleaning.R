#### Preamble ####
# Purpose: Clean the survey data downloaded from ICOS, Government of Canada, and Statistic Canada (Links can be found in comments above each data cleaning code)
# Author: Owen Huang
# Data: 27 April 2022
# Contact: o.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the appropriate data and saved it to inputs/data


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
library("readxl")
library(readr)
# Read in the raw data. 
# national carbon emissions https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2021
National_Territorial_Emissions <- read_excel("../../inputs/data/National_Carbon_Emissions_2021v1.0.xlsx", 
                                             sheet = "Territorial Emissions")
National_Territorial_Emissions <- National_Territorial_Emissions[11:73,]
colnames(National_Territorial_Emissions) <- National_Territorial_Emissions[1,]
colnames(National_Territorial_Emissions)[1] <- "Year"
National_Territorial_Emissions <- National_Territorial_Emissions[-1, ] 
Country_Emissions <- National_Territorial_Emissions %>% select(Year, Australia, Brazil, Canada, China, Cuba, France, Germany, India, Japan, Mexico, Sweden, USA)


Country_Emissions <- mutate_all(Country_Emissions, function(x) as.numeric(as.character(x)))
Country_Emissions <- Country_Emissions %>% 
  pivot_longer(colnames(Country_Emissions)[2:13], names_to = "country", values_to = "GHG Emission")

rm(National_Territorial_Emissions)

# canada ghg emission https://www.canada.ca/en/environment-climate-change/services/environmental-indicators/greenhouse-gas-emissions.html#agriculture
canada_ghg_emission <- read_csv("../../inputs/data/canada_ghg_emission.csv")
canada_ghg_emission <- canada_ghg_emission[2:33,]
colnames(canada_ghg_emission) <- canada_ghg_emission[1,]
canada_ghg_emission <- canada_ghg_emission[-1, ] 
canada_ghg_emission <- mutate_all(canada_ghg_emission, function(x) as.numeric(as.character(x)))

# canada gdp per capita https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=CA
CAD_GDP_per_cap <- read_excel("../../inputs/data/GDP_per_cap.xls")
CAD_GDP_per_cap <- CAD_GDP_per_cap[3:269,]
colnames(CAD_GDP_per_cap) <- CAD_GDP_per_cap[1,]
CAD_GDP_per_cap <- CAD_GDP_per_cap[-1, ] 
CAD_GDP_per_cap <- CAD_GDP_per_cap %>% filter(`Country Name` == "Canada")
temp <- CAD_GDP_per_cap %>% 
  pivot_longer(colnames(CAD_GDP_per_cap), names_to = "Year", values_to = "GDP per Capita")
temp <- temp[5:65,]
CAD_GDP_per_cap <- temp
rm(temp)

# ghg_emission by province https://www.canada.ca/en/environment-climate-change/services/environmental-indicators/greenhouse-gas-emissions.html#agriculture
prov_ghg_emissions <- read_csv("../../inputs/data/ghg-emissions-regional-en.csv", 
                               skip = 1)
prov_ghg_emissions <- prov_ghg_emissions[1:14,]
colnames(prov_ghg_emissions) <- prov_ghg_emissions[1,]
prov_ghg_emissions <- prov_ghg_emissions[-1, ] 
prov_ghg_emissions  <- prov_ghg_emissions %>% 
  pivot_longer(colnames(prov_ghg_emissions)[2:4], names_to = "Year", values_to = "GHG Emission") %>%
  mutate(`Province or territory` = case_when(`Province or territory` == "Newfoundland and Labrador (NL)"~"NL",
                                             `Province or territory` == "Prince Edward Island (PE)"~"PE",
                                             `Province or territory` == "Nova Scotia (NS)"~"NS",
                                             `Province or territory` == "New Brunswick (NB)"~"NB",
                                             `Province or territory` == "Quebec (QC)"~"QC",
                                             `Province or territory` == "Manitoba (MB)"~"MB",
                                             `Province or territory` == "Saskatchewan (SK)"~"SK",
                                             `Province or territory` == "Alberta (AB)"~"AB",
                                             `Province or territory` == "British Columbia (BC)"~"BC",
                                             `Province or territory` == "Yukon (YT)"~"YT",
                                             `Province or territory` == "Northwest Territories (NT)"~"NT",
                                             `Province or territory` == "Nunavut (NU)[A]"~"NU",
                                             `Province or territory` == "Ontario (ON)"~"ON")) %>%
  mutate(Year = case_when(Year == "1990 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)"~"1990 GHG emission",
                          Year == "2005 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)"~"2005 GHG emission",
                          Year == "2020 greenhouse gas emissions (megatonnes of carbon dioxide equivalent)"~"2020 GHG emission"))
prov_ghg_emissions$`GHG Emission` <- as.numeric(prov_ghg_emissions$`GHG Emission`)

# ghg emission by sectors https://www.canada.ca/en/environment-climate-change/services/environmental-indicators/greenhouse-gas-emissions.html#agriculture
sector_ghg_emissions <- read_csv("../../inputs/data/ghg-emissions-sector-en.csv", 
                                 skip = 1)
sector_ghg_emissions <- sector_ghg_emissions[1:32,]
colnames(sector_ghg_emissions) <- sector_ghg_emissions[1,]
sector_ghg_emissions <- sector_ghg_emissions[-1, ] 
colnames(sector_ghg_emissions) <- c("Year", "Oil and gas", "Transport", "Buildings", "Electricity", "Heavy industry", "Agriculture", "Waste and others")
sector_ghg_emissions <- mutate_all(sector_ghg_emissions, function(x) as.numeric(as.character(x)))
sector_ghg_emissions <- sector_ghg_emissions %>% mutate(Total = `Oil and gas` + Transport + Buildings + Electricity + `Heavy industry` + Agriculture + `Waste and others`)
temp <- sector_ghg_emissions %>% 
  pivot_longer(colnames(sector_ghg_emissions)[2:8], names_to = "sector", values_to = "Megatonnes of carbon dioxide equivalent")
sector_ghg_emissions <- temp
rm(temp)

# canada population https://data.worldbank.org/indicator/SP.POP.TOTL?locations=CA
CAD_pop <- read_excel("../../inputs/data/pop.xls")
CAD_pop <- CAD_pop[3:269,]
colnames(CAD_pop) <- CAD_pop[1,]
CAD_pop <- CAD_pop[-1, ] 
CAD_pop <- CAD_pop %>% filter(`Country Name` == "Canada")
temp <- CAD_pop %>% 
  pivot_longer(colnames(CAD_pop), names_to = "Year", values_to = "pop")
temp <- temp[5:65,]
CAD_pop <- temp
rm(temp)

# join gdp and pop
canada_data <- left_join(CAD_GDP_per_cap, CAD_pop)
canada_data <- mutate_all(canada_data, function(x) as.numeric(as.character(x)))
canada_data <- canada_data %>% mutate(GDP = `GDP per Capita`*pop) %>% filter(Year >= 1990)
rm(CAD_GDP_per_cap, CAD_pop)

canada_data <- right_join(canada_data, canada_ghg_emission)
rm(canada_ghg_emission)

# canada gdp by expenditure https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610022201
gdp_expenditure <- read_csv("../../inputs/data/gdp_ex.csv")
gdp_expenditure <- gdp_expenditure %>% select(REF_DATE, Estimates, VALUE)


gdp_exp <- read_csv("../../inputs/data/gdp_exp.csv", col_names = FALSE)
gdp_exp <- t(gdp_exp)
colnames(gdp_exp) <- gdp_exp[1,]
colnames(gdp_exp)[1] <- "Year"
gdp_exp <- gdp_exp[-1, ] 
gdp_exp <- as.data.frame(gdp_exp)
temp <- gdp_exp %>% select(Year, 
                           "Final consumption expenditure", 
                           "Household final consumption expenditure", 
                           "Goods", "Durable goods", 
                           "Semi-durable goods",
                           "Non-durable goods",
                           "Services",                                                                                                                                        
                           "Non-profit institutions serving households' final consumption expenditure",
                           "General governments final consumption expenditure",
                           "Gross fixed capital formation",
                           "Business gross fixed capital formation",
                           "Residential structures",
                           "Non-residential structures, machinery and equipment",
                           "Non-residential structures",
                           "Machinery and equipment",
                           "Intellectual property products",
                           "Non-profit institutions serving households' gross fixed capital formation",
                           "General governments gross fixed capital formation",
                           "Investment in inventories",
                           "Of which: business investment in inventories",
                           "Non-farm",
                           "Farm",
                           "Exports to other countries",
                           "Imports from other countries",
                           "Gross domestic product at market prices",
                           "Final domestic demand")
temp <- mutate_all(temp, function(x) as.numeric(gsub(",", "", x)))
temp <- left_join(canada_data, temp)
canada_data <- temp
rm(temp)
rm(gdp_exp)