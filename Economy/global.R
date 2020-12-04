# Load the libraries
#update.packages()
library(tidyverse) # learned in class
library(lubridate) # to work with date-time values
library(shinydashboard) # to create the dashboard layout
library(shiny) # to also create the dashboard layout
library(shinyWidgets) # honestly only used it for one argument: options = list(`actions-box` = TRUE)
library(tidyquant) # used to download stock and economc data
library(plotly) # to make my graphs interactive!

#install and load streamgraph
# library(zoo) # need to install this to install streamgraph apparently
#devtools::install_github("hrbrmstr/streamgraph") # to make a cool looking steamgraph
library(streamgraph) # to make a cool looking steamgraph
library(rsconnect)


# Load the data
gdp <- read.csv("data/gdp.csv")

colnames(gdp)
# Clean the data
gdp <- gdp %>%
  select( "date", "class","VALUE" )

colnames(gdp) <- c("Date","Industry","GDP")
gdp$Date <- gsub("-","",gdp$Date) # remove dashes
gdp <- gdp %>% # change date column into date values in R
  mutate(Date2 = paste0(Date,"01")) %>%
  mutate(Datef = strptime(Date2,format = "%Y%m%d"))
gdp <- gdp %>%
  select(Industry,GDP,Datef)
colnames(gdp) <- c("Industry","GDP","Date")
gdp$Date <- ymd(gdp$Date)

unique_industry <- unique(gdp$Industry) # number of unique industries


# data for bar graph and bubble graph

f <- gdp %>%
  filter(Date >= "2020-01-01") %>%
  group_by(Industry) %>%
  mutate(pct_change = (GDP/lag(GDP) - 1) * 100) # for graphing line graph of pct change in gdp

fm <- f %>%
  summarise(max_pct_change = max(pct_change, na.rm = TRUE), min_pct_change = min(pct_change, na.rm = TRUE)) %>% # for graphing bar graph of pct change in gdp
  pivot_longer(c(min_pct_change,max_pct_change),names_to = "pct_change", values_to = "values")


# all gdp data (for can, eu, can, etc)
gdp_all <- tq_get(c("NAEXKP01CAQ189S","GDPC1","CLVMEURSCAB1GQEA19","CLVMNACSCAB1GQUK","JPNRGDPEXP"),get = "economic.data")
gdp_all$symbol[gdp_all$symbol == "NAEXKP01CAQ189S"] <- "Canada"
gdp_all$symbol[gdp_all$symbol =="GDPC1" ] <- "United States"
gdp_all$symbol[gdp_all$symbol =="CLVMEURSCAB1GQEA19" ] <- "Europe (excluding UK)"
gdp_all$symbol[gdp_all$symbol =="CLVMNACSCAB1GQUK" ] <- "United Kingdom (UK)"
gdp_all$symbol[gdp_all$symbol =="JPNRGDPEXP" ] <- "Japan"

gdp_all <- gdp_all %>%
  group_by(symbol) %>%
  mutate(pct_change = (price/lag(price) - 1) * 100)
gdp_all_mm <- gdp_all %>%
  summarise(`Largest GDP Growth` = max(pct_change,na.rm = TRUE), `Largest Decrease in GDP` = min(pct_change, na.rm = TRUE))
gdp_all_mm <- gdp_all_mm %>%
  pivot_longer(cols = c("Largest GDP Growth","Largest Decrease in GDP"), names_to = "Percent Change",values_to = "Values")


# data cleaning for canada unemployment rates
h <-'
call1 <- "https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/14100287/en" # link to unemployment data for canada
unemployment_rest <- GET(call1) # call the api
can_labor <- download.file(content(unemployment_rest)$object, paste(getwd(),"/","download", sep=""), method="auto", quiet = FALSE) # download the api to my working directory

ul <- unzip(paste(getwd(),"/","download", sep=""), list = TRUE) # unzip the file
# print(ul$Name) # see what the indivdual csv files are
labor_c <- data.table::fread(ul$Name[1], data.table = FALSE)

labor_c <- labor_c %>%
        filter(Statistics == "Estimate", `Data type` == "Seasonally adjusted") %>%
        select("REF_DATE","GEO","Labour force characteristics","Sex","Age group","VALUE")
        #filter(Statistics == "Estimate", Data.type == "Seasonally adjusted") %>%
        # select("?..REF_DATE","GEO","Labour.force.characteristics","Sex","Age.group","VALUE")

colnames(labor_c) <- c("date","geo","labor.force.type","sex","age.group","value")
#2020-10-01 is the most recent date
labor_c$date <- ymd(labor_c$date, truncated = 2) # make the date column a datetime object'



labor_c <- data.table::fread("data/final.csv", data.table = FALSE) # to read the data faster


colnames(labor_c) <- c("date","geo","labor.force.type","sex","age.group","value")
#2020-10-01 is the most recent date
labor_c$date <- ymd(labor_c$date, truncated = 2) # make the date column a datetime object

unique_geo <- unique(labor_c$geo)
unique_age <- unique(labor_c$age.group)
unique_sex <- unique(labor_c$sex)
unique_labor <- unique(labor_c$labor.force.type)


js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #E75555;
}"' # to make tabpanel panes red instead of blue (which is the default) #d73925





