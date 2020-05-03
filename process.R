library(dplyr)
library(rio)
library(tidyr)
library(readr)

JHU <- data.frame()

start <- as.Date("2020-01-20") #The first data file is actually 2020-01-22, but the first few import(myurl) sometimes fails.
end   <- as.Date(Sys.Date())

theDate <- start

while (theDate <= end)
{
  #Importing data
  myurl <- paste0("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/", format(theDate, "%m-%d-%Y"), ".csv")

  nextFile <- tryCatch(import(myurl),error=function(e) e, warning=function(w) w)
  
  if(!is.null(dim(nextFile))) {
    #Fixing headings and interpretting date formats
    if (theDate <= as.Date("2020-01-22")) {
      nextFile <- nextFile %>% 
        rename(
          Country_Region = "Country/Region",
          Province_State = "Province/State",
          Last_Update = "Last Update"
        )
      #nextFile$Last_Update <- as.Date(nextFile$Last_Update, format = "%m/%d/%Y %H:%M")
    } else if (theDate <= as.Date("2020-01-30")) {
      nextFile <- nextFile %>% 
        rename(
          Country_Region = "Country/Region",
          Province_State = "Province/State",
          Last_Update = "Last Update"
        )
      #nextFile$Last_Update <- as.Date(nextFile$Last_Update, format = "%m/%d/%y %H:%M")
    } else if (theDate <= as.Date("2020-02-01")) {
      nextFile <- nextFile %>% 
        rename(
          Country_Region = "Country/Region",
          Province_State = "Province/State",
          Last_Update = "Last Update"
        )
      #nextFile$Last_Update <- as.Date(nextFile$Last_Update, format = "%m/%d/%Y %H:%M")
    } else if (theDate <= as.Date("2020-03-21")) {
      nextFile <- nextFile %>% 
        rename(
          Country_Region = "Country/Region",
          Province_State = "Province/State",
          Last_Update = "Last Update"
        )
      #nextFile$Last_Update <- as.Date(nextFile$Last_Update, format = "%Y-%m-%dT%H:%M:%S")
    } else if (theDate <= as.Date("2020-03-22")) {
      #nextFile$Last_Update <- as.Date(nextFile$Last_Update, format = "%m/%d/%y %H:%M")
    } else {
      #nextFile$Last_Update <- as.Date(nextFile$Last_Update, format = "%Y-%m-%d %H:%M:%S")
    }
    
    nextFile$Last_Update <- theDate
    
    JHU <- bind_rows(JHU,nextFile)
    print(theDate)
  } else sprintf("Data for %s could not be read. This may be normal if the date is today.",format(theDate, "%Y-%m-%d"))
  
  theDate <- theDate + 1                    
}

#Cleaning up column names in final data frame
JHU <- JHU %>% 
  rename(
    Long = Long_,
    County_City = Admin2
  )

#Cleanup China
JHU[JHU$Country_Region=="Mainland China",]$Country_Region <- "China"
JHU[JHU$Country_Region=="Macau",]$Province_State <- "Macau"
JHU[JHU$Country_Region=="Macau",]$Country_Region <- "China"
JHU[JHU$Country_Region=="Macao SAR",]$Province_State <- "Macau"
JHU[JHU$Country_Region=="Macao SAR",]$Country_Region <- "China"
JHU[JHU$Country_Region=="Hong Kong SAR",]$Province_State <- "Hong Kong"
JHU[JHU$Country_Region=="Hong Kong SAR",]$Country_Region <- "China"
JHU[JHU$Country_Region=="Hong Kong",]$Province_State <- "Hong Kong"
JHU[JHU$Country_Region=="Hong Kong",]$Country_Region <- "China"

#Cleanup Taiwan
JHU[JHU$Country_Region=="Taipei and environs",]$Province_State <- ""
JHU[JHU$Country_Region=="Taipei and environs",]$Country_Region <- "Taiwan"
JHU[JHU$Country_Region=="Taiwan*",]$Country_Region <- "Taiwan"
JHU[JHU$Country_Region=="Taiwan",]$Province_State <- ""

#Cleanup Vietnam. Seem to be missing significant data
JHU[JHU$Country_Region=="Viet Nam",]$Country_Region <- "Vietnam"

#Cleanup Cabo Verde
JHU[JHU$Country_Region=="Cape Verde",]$Country_Region <- "Cabo Verde"

#Remove US, US and Puerto Rico (Country) rows
JHU <- JHU[!(JHU$Province_State=="US" & JHU$Country_Region=="US"),]
JHU <- JHU[!(JHU$Country_Region=="Puerto Rico"),]

#Clean up US
#US has a separate datapoint for Recovered. This is being dropped due to poor quality.
JHU <- JHU[!(JHU$Province_State=="Recovered" & JHU$Country_Region=="US"),]
JHU[JHU$Province_State=="United States Virgin Islands",]$Province_State <- "Virgin Islands"
JHU[JHU$Province_State=="Chicago",]$County_City <- "Cook"
JHU[JHU$Province_State=="Chicago",]$Province_State <- "Illinois"
JHU[JHU$Province_State=="New York City, NY",]$County_City <- "New York City"
JHU[JHU$Province_State=="New York City, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="New York County, NY",]$County_City <- "New York City"
JHU[JHU$Province_State=="New York County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="King County, WA",]$County_City <- "King"
JHU[JHU$Province_State=="King County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Westchester County, NY",]$County_City <- "Westchester"
JHU[JHU$Province_State=="Westchester County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="Santa Clara County, CA",]$County_City <- "Santa Clara"
JHU[JHU$Province_State=="Santa Clara County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Snohomish County, WA",]$County_City <- "Snohomish"
JHU[JHU$Province_State=="Snohomish County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Los Angeles, CA",]$County_City <- "Los Angeles"
JHU[JHU$Province_State=="Los Angeles, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="San Francisco County, CA",]$County_City <- "San Francisco"
JHU[JHU$Province_State=="San Francisco County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Cook County, IL",]$County_City <- "Cook"
JHU[JHU$Province_State=="Cook County, IL",]$Province_State <- "Illinois"
JHU[JHU$Province_State=="Harris County, TX",]$County_City <- "Harris"
JHU[JHU$Province_State=="Harris County, TX",]$Province_State <- "Texas"
JHU[JHU$Province_State=="Bergen County, NJ",]$County_City <- "Bergen"
JHU[JHU$Province_State=="Bergen County, NJ",]$Province_State <- "New Jersey"
JHU[JHU$Province_State=="Nassau County, NY",]$County_City <- "Nassau"
JHU[JHU$Province_State=="Nassau County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="Douglas County, CO",]$County_City <- "Douglas"
JHU[JHU$Province_State=="Douglas County, CO",]$Province_State <- "Colorado"
JHU[JHU$Province_State=="Fort Bend County, TX",]$County_City <- "Fort Bend"
JHU[JHU$Province_State=="Fort Bend County, TX",]$Province_State <- "Texas"
JHU[JHU$Province_State=="Fulton County, GA",]$County_City <- "Fulton"
JHU[JHU$Province_State=="Fulton County, GA",]$Province_State <- "Georgia"
JHU[JHU$Province_State=="Maricopa County, AZ",]$County_City <- "Maricopa"
JHU[JHU$Province_State=="Maricopa County, AZ",]$Province_State <- "Arizona"
JHU[JHU$Province_State=="Providence County, RI",]$County_City <- "Providence"
JHU[JHU$Province_State=="Providence County, RI",]$Province_State <- "Rhode Island"
JHU[JHU$Province_State=="Washington County, OR",]$County_City <- "Washington"
JHU[JHU$Province_State=="Washington County, OR",]$Province_State <- "Oregon"
JHU[JHU$Province_State=="Alameda County, CA",]$County_City <- "Alameda"
JHU[JHU$Province_State=="Alameda County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Broward County, FL",]$County_City <- "Broward"
JHU[JHU$Province_State=="Broward County, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Fairfield County, CT",]$County_City <- "Fairfield"
JHU[JHU$Province_State=="Fairfield County, CT",]$Province_State <- "Connecticut"
JHU[JHU$Province_State=="Floyd County, GA",]$County_City <- "Floyd"
JHU[JHU$Province_State=="Floyd County, GA",]$Province_State <- "Georgia"
JHU[JHU$Province_State=="Lee County, FL",]$County_City <- "Lee"
JHU[JHU$Province_State=="Lee County, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Pinal County, AZ",]$County_City <- "Pinal"
JHU[JHU$Province_State=="Pinal County, AZ",]$Province_State <- "Arizona"
JHU[JHU$Province_State=="Rockland County, NY",]$County_City <- "Rockland"
JHU[JHU$Province_State=="Rockland County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="Saratoga County, NY",]$County_City <- "Saratoga"
JHU[JHU$Province_State=="Saratoga County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="Charleston County, SC",]$County_City <- "Charleston"
JHU[JHU$Province_State=="Charleston County, SC",]$Province_State <- "South Carolina"
JHU[JHU$Province_State=="Clark County, WA",]$County_City <- "Clark"
JHU[JHU$Province_State=="Clark County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Cobb County, GA",]$County_City <- "Cobb"
JHU[JHU$Province_State=="Cobb County, GA",]$Province_State <- "Georgia"
JHU[JHU$Province_State=="Davis County, UT",]$County_City <- "Davis"
JHU[JHU$Province_State=="Davis County, UT",]$Province_State <- "Utah"
JHU[JHU$Province_State=="El Paso County, CO",]$County_City <- "El Paso"
JHU[JHU$Province_State=="El Paso County, CO",]$Province_State <- "Colorado"
JHU[JHU$Province_State=="Honolulu County, HI",]$County_City <- "Honolulu"
JHU[JHU$Province_State=="Honolulu County, HI",]$Province_State <- "Hawaii"
JHU[JHU$Province_State=="Jackson County, OR ",]$County_City <- "Jackson"
JHU[JHU$Province_State=="Jackson County, OR ",]$Province_State <- "Oregon"
JHU[JHU$Province_State=="Jefferson County, WA",]$County_City <- "Jefferson"
JHU[JHU$Province_State=="Jefferson County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Kershaw County, SC",]$County_City <- "Kershaw"
JHU[JHU$Province_State=="Kershaw County, SC",]$Province_State <- "South Carolina"
JHU[JHU$Province_State=="Klamath County, OR",]$County_City <- "Klamath"
JHU[JHU$Province_State=="Klamath County, OR",]$Province_State <- "Oregon"
JHU[JHU$Province_State=="Madera County, CA",]$County_City <- "Madera"
JHU[JHU$Province_State=="Madera County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Middlesex County, MA",]$County_City <- "Middlesex"
JHU[JHU$Province_State=="Middlesex County, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Pierce County, WA",]$County_City <- "Pierce"
JHU[JHU$Province_State=="Pierce County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Plymouth County, MA",]$County_City <- "Plymouth"
JHU[JHU$Province_State=="Plymouth County, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Santa Cruz County, CA",]$County_City <- "Santa Cruz"
JHU[JHU$Province_State=="Santa Cruz County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Tulsa County, OK",]$County_City <- "Tulsa"
JHU[JHU$Province_State=="Tulsa County, OK",]$Province_State <- "Oklahoma"
JHU[JHU$Province_State=="Montgomery County, TX",]$County_City <- "Montgomery"
JHU[JHU$Province_State=="Montgomery County, TX",]$Province_State <- "Texas"
JHU[JHU$Province_State=="Contra Costa County, CA",]$County_City <- "Contra Costa"
JHU[JHU$Province_State=="Contra Costa County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Suffolk County, MA",]$County_City <- "Suffolk"
JHU[JHU$Province_State=="Suffolk County, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Norfolk County, MA",]$County_City <- "Norfolk"
JHU[JHU$Province_State=="Norfolk County, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Grafton County, NH",]$County_City <- "Grafton"
JHU[JHU$Province_State=="Grafton County, NH",]$Province_State <- "New Hampshire"
JHU[JHU$Province_State=="Montgomery County, MD",]$County_City <- "Montgomery"
JHU[JHU$Province_State=="Montgomery County, MD",]$Province_State <- "Maryland"
JHU[JHU$Province_State=="Montgomery County, PA",]$County_City <- "Montgomery"
JHU[JHU$Province_State=="Montgomery County, PA",]$Province_State <- "Pennsylvania"
JHU[JHU$Province_State=="Clark County, NV",]$County_City <- "Clark"
JHU[JHU$Province_State=="Clark County, NV",]$Province_State <- "Nevada"
JHU[JHU$Province_State=="Fairfax County, VA",]$County_City <- "Fairfax"
JHU[JHU$Province_State=="Fairfax County, VA",]$Province_State <- "Virginia"
JHU[JHU$Province_State=="Rockingham County, NH",]$County_City <- "Rockingham County"
JHU[JHU$Province_State=="Rockingham County, NH",]$Province_State <- "New Hampshire"
JHU[JHU$Province_State=="Washington, D.C.",]$Province_State <- "District of Columbia"
JHU[JHU$Province_State=="District of Columbia",]$County_City <- ""
JHU[JHU$Province_State=="Washoe County, NV",]$County_City <- "Washoe"
JHU[JHU$Province_State=="Washoe County, NV",]$Province_State <- "Nevada"
JHU[JHU$Province_State=="Berkshire County, MA",]$County_City <- "Berkshire"
JHU[JHU$Province_State=="Berkshire County, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Davidson County, TN",]$County_City <- "Davidson"
JHU[JHU$Province_State=="Davidson County, TN",]$Province_State <- "Tennessee"
JHU[JHU$Province_State=="Douglas County, OR",]$County_City <- "Douglas"
JHU[JHU$Province_State=="Douglas County, OR",]$Province_State <- "Oregon"
JHU[JHU$Province_State=="Fresno County, CA",]$County_City <- "Fresno"
JHU[JHU$Province_State=="Fresno County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Harford County, MD",]$County_City <- "Harford"
JHU[JHU$Province_State=="Harford County, MD",]$Province_State <- "Maryland"
JHU[JHU$Province_State=="Hendricks County, IN",]$County_City <- "Hendricks"
JHU[JHU$Province_State=="Hendricks County, IN",]$Province_State <- "Indiana"
JHU[JHU$Province_State=="Hudson County, NJ",]$County_City <- "Hudson"
JHU[JHU$Province_State=="Hudson County, NJ",]$Province_State <- "New Jersey"
JHU[JHU$Province_State=="Johnson County, KS",]$County_City <- "Johnson"
JHU[JHU$Province_State=="Johnson County, KS",]$Province_State <- "Kansas"
JHU[JHU$Province_State=="Kittitas County, WA",]$County_City <- "Kittitas"
JHU[JHU$Province_State=="Kittitas County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Manatee County, FL",]$County_City <- "Manatee"
JHU[JHU$Province_State=="Manatee County, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Marion County, OR",]$County_City <- "Marion"
JHU[JHU$Province_State=="Marion County, OR",]$Province_State <- "Oregon"
JHU[JHU$Province_State=="Okaloosa County, FL",]$County_City <- "Okaloosa"
JHU[JHU$Province_State=="Okaloosa County, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Polk County, GA",]$County_City <- "Polk"
JHU[JHU$Province_State=="Polk County, GA",]$Province_State <- "Georgia"
JHU[JHU$Province_State=="Riverside County, CA",]$County_City <- "Riverside"
JHU[JHU$Province_State=="Riverside County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Santa Rosa County, FL",]$County_City <- "Santa Rosa"
JHU[JHU$Province_State=="Santa Rosa County, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Shelby County, TN",]$County_City <- "Shelby"
JHU[JHU$Province_State=="Shelby County, TN",]$Province_State <- "Tennessee"
JHU[JHU$Province_State=="Spokane County, WA",]$County_City <- "Spokane"
JHU[JHU$Province_State=="Spokane County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="St. Louis County, MO",]$County_City <- "St. Louis"
JHU[JHU$Province_State=="St. Louis County, MO",]$Province_State <- "Missouri"
JHU[JHU$Province_State=="Suffolk County, NY",]$County_City <- "Suffolk"
JHU[JHU$Province_State=="Suffolk County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="Ulster County, NY",]$County_City <- "Ulster"
JHU[JHU$Province_State=="Ulster County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="Unassigned Location, VT",]$Province_State <- "Vermont"
JHU[JHU$Province_State=="Unknown Location, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Volusia County, FL",]$County_City <- "Volusia"
JHU[JHU$Province_State=="Volusia County, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State==" Norfolk County, MA",]$County_City <- "Norfolk"
JHU[JHU$Province_State==" Norfolk County, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Ashland, NE",]$County_City <- "Saunders"
JHU[JHU$Province_State=="Ashland, NE",]$Province_State <- "Nebraska"
JHU[JHU$Province_State=="Bennington County, VT",]$County_City <- "Bennington"
JHU[JHU$Province_State=="Bennington County, VT",]$Province_State <- "Vermont"
JHU[JHU$Province_State=="Berkeley, CA",]$County_City <- "Alameda"
JHU[JHU$Province_State=="Berkeley, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Boston, MA",]$County_City <- "Suffolk"
JHU[JHU$Province_State=="Boston, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Carver County, MN",]$County_City <- "Carver"
JHU[JHU$Province_State=="Carver County, MN",]$Province_State <- "Minnesota"
JHU[JHU$Province_State=="Charlotte County, FL",]$County_City <- "Charlotte"
JHU[JHU$Province_State=="Charlotte County, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Chatham County, NC",]$County_City <- "Chatham"
JHU[JHU$Province_State=="Chatham County, NC",]$Province_State <- "North Carolina"
JHU[JHU$Province_State=="Cherokee County, GA",]$County_City <- "Cherokee"
JHU[JHU$Province_State=="Cherokee County, GA",]$Province_State <- "Georgia"
JHU[JHU$Province_State=="Chicago, IL",]$County_City <- "Cook"
JHU[JHU$Province_State=="Chicago, IL",]$Province_State <- "Illinois"
JHU[JHU$Province_State=="Collin County, TX",]$County_City <- "Collin"
JHU[JHU$Province_State=="Collin County, TX",]$Province_State <- "Texas"
JHU[JHU$Province_State=="Delaware County, PA",]$County_City <- "Delaware"
JHU[JHU$Province_State=="Delaware County, PA",]$Province_State <- "Pennsylvania"
JHU[JHU$Province_State=="Denver County, CO",]$County_City <- "Denver"
JHU[JHU$Province_State=="Denver County, CO",]$Province_State <- "Colorado"
JHU[JHU$Province_State=="Douglas County, NE",]$County_City <- "Douglas"
JHU[JHU$Province_State=="Douglas County, NE",]$Province_State <- "Nebraska"
JHU[JHU$Province_State=="Fayette County, KY",]$County_City <- "Fayette"
JHU[JHU$Province_State=="Fayette County, KY",]$Province_State <- "Kentucky"
JHU[JHU$Province_State=="Grant County, WA",]$County_City <- "Grant"
JHU[JHU$Province_State=="Grant County, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Harrison County, KY",]$County_City <- "Harrison"
JHU[JHU$Province_State=="Harrison County, KY",]$Province_State <- "Kentucky"
JHU[JHU$Province_State=="Hillsborough, FL",]$County_City <- "Hillsborough"
JHU[JHU$Province_State=="Hillsborough, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Humboldt County, CA",]$County_City <- "Humboldt"
JHU[JHU$Province_State=="Humboldt County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Jefferson County, KY",]$County_City <- "Jefferson"
JHU[JHU$Province_State=="Jefferson County, KY",]$Province_State <- "Kentucky"
JHU[JHU$Province_State=="Jefferson Parish, LA",]$County_City <- "Jefferson Parish"
JHU[JHU$Province_State=="Jefferson Parish, LA",]$Province_State <- "Louisiana"
JHU[JHU$Province_State=="Johnson County, IA",]$County_City <- "Johnson"
JHU[JHU$Province_State=="Johnson County, IA",]$Province_State <- "Iowa"
JHU[JHU$Province_State=="Lackland, TX",]$County_City <- "Bexar"
JHU[JHU$Province_State=="Lackland, TX",]$Province_State <- "Texas"
JHU[JHU$Province_State=="Madison, WI",]$County_City <- "Dane"
JHU[JHU$Province_State=="Madison, WI",]$Province_State <- "Wisconsin"
JHU[JHU$Province_State=="Marion County, IN",]$County_City <- "Marion"
JHU[JHU$Province_State=="Marion County, IN",]$Province_State <- "Indiana"
JHU[JHU$Province_State=="Norwell County, MA",]$County_City <- "Norwell"
JHU[JHU$Province_State=="Norwell County, MA",]$Province_State <- "Massachusetts"
JHU[JHU$Province_State=="Orange County, CA",]$County_City <- "Orange"
JHU[JHU$Province_State=="Orange County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Placer County, CA",]$County_City <- "Placer"
JHU[JHU$Province_State=="Placer County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Portland, OR",]$County_City <- "Multnomah"
JHU[JHU$Province_State=="Portland, OR",]$Province_State <- "Oregon"
JHU[JHU$Province_State=="Providence, RI",]$County_City <- "Providence"
JHU[JHU$Province_State=="Providence, RI",]$Province_State <- "Rhode Island"
JHU[JHU$Province_State=="Queens County, NY",]$County_City <- "Queens"
JHU[JHU$Province_State=="Queens County, NY",]$Province_State <- "New York"
JHU[JHU$Province_State=="Ramsey County, MN",]$County_City <- "Ramsey"
JHU[JHU$Province_State=="Ramsey County, MN",]$Province_State <- "Minnesota"
JHU[JHU$Province_State=="Sacramento County, CA",]$County_City <- "Sacramento"
JHU[JHU$Province_State=="Sacramento County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="San Antonio, TX",]$County_City <- "Bexar"
JHU[JHU$Province_State=="San Antonio, TX",]$Province_State <- "Texas"
JHU[JHU$Province_State=="San Benito, CA",]$County_City <- "San Benito"
JHU[JHU$Province_State=="San Benito, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="San Diego County, CA",]$County_City <- "San Diego"
JHU[JHU$Province_State=="San Diego County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="San Mateo, CA",]$County_City <- "San Mateo"
JHU[JHU$Province_State=="San Mateo, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Santa Clara, CA",]$County_City <- "Santa Clara"
JHU[JHU$Province_State=="Santa Clara, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Sarasota, FL",]$County_City <- "Sarasota"
JHU[JHU$Province_State=="Sarasota, FL",]$Province_State <- "Florida"
JHU[JHU$Province_State=="Seattle, WA",]$County_City <- "King"
JHU[JHU$Province_State=="Seattle, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Shasta County, CA",]$County_City <- "Shasta"
JHU[JHU$Province_State=="Shasta County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Sonoma County, CA",]$County_City <- "Sonoma"
JHU[JHU$Province_State=="Sonoma County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Spartanburg County, SC",]$County_City <- "Spartanburg"
JHU[JHU$Province_State=="Spartanburg County, SC",]$Province_State <- "South Carolina"
JHU[JHU$Province_State=="Summit County, CO",]$County_City <- "Summit"
JHU[JHU$Province_State=="Summit County, CO",]$Province_State <- "Colorado"
JHU[JHU$Province_State=="Tempe, AZ",]$County_City <- "Maricopa"
JHU[JHU$Province_State=="Tempe, AZ",]$Province_State <- "Arizona"
JHU[JHU$Province_State=="Travis, CA",]$County_City <- "Solano"
JHU[JHU$Province_State=="Travis, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Umatilla, OR",]$County_City <- "Umatilla"
JHU[JHU$Province_State=="Umatilla, OR",]$Province_State <- "Oregon"
JHU[JHU$Province_State=="Unassigned Location, WA",]$Province_State <- "Washington"
JHU[JHU$Province_State=="Virgin Islands, U.S.",]$Country_Region <- "US"
JHU[JHU$Province_State=="Virgin Islands, U.S.",]$Province_State <- "Virgin Islands"
JHU[JHU$Province_State=="Wake County, NC",]$County_City <- "Wake"
JHU[JHU$Province_State=="Wake County, NC",]$Province_State <- "North Carolina"
JHU[JHU$Province_State=="Wayne County, PA",]$County_City <- "Wayne"
JHU[JHU$Province_State=="Wayne County, PA",]$Province_State <- "Pennsylvania"
JHU[JHU$Province_State=="Williamson County, TN",]$County_City <- "Williamson"
JHU[JHU$Province_State=="Williamson County, TN",]$Province_State <- "Indiana"
JHU[JHU$Province_State=="Yolo County, CA",]$County_City <- "Yolo"
JHU[JHU$Province_State=="Yolo County, CA",]$Province_State <- "California"
JHU[JHU$Province_State=="Orange, CA",]$County_City <- "Orange"
JHU[JHU$Province_State=="Orange, CA",]$Province_State <- "California"
JHU[JHU$Country_Region=="Guam",]$Province_State <- "Guam"
JHU[JHU$Country_Region=="Guam",]$Country_Region <- "US"

#Clean up Canada. Although Toronto, Montreal, etc. County_City identifiers were initially available, the recent data omitted these. All of them are thus ignored.
#Canada has a separate datapoint for Recovered. This is being dropped due to poor data quality.
JHU <- JHU[!(JHU$Province_State=="Recovered" & JHU$Country_Region=="Canada"),]
JHU[JHU$Province_State=="Toronto, ON",]$Province_State <- "Ontario"
JHU[JHU$Province_State==" Montreal, QC",]$Province_State <- "Quebec"
JHU[JHU$Province_State=="Edmonton, Alberta",]$Province_State <- "Alberta"
JHU[JHU$Province_State=="Calgary, Alberta",]$Province_State <- "Alberta"
JHU[JHU$Province_State=="London, ON",]$Province_State <- "Ontario"

#Clean up Diamond Princess. The Diamond Princess data kept on being tracked as a Country_Region. The County_City and Province_State identifiers that were initially available were thus ignored.
JHU[JHU$Province_State=="Diamond Princess",]$Country_Region <- "Diamond Princess"
JHU[JHU$Province_State=="Diamond Princess",]$Province_State <- ""
JHU[JHU$Province_State=="Diamond Princess cruise ship",]$Country_Region <- "Diamond Princess"
JHU[JHU$Province_State=="Diamond Princess cruise ship",]$Province_State <- ""
JHU[JHU$Province_State=="From Diamond Princess",]$Country_Region <- "Diamond Princess"
JHU[JHU$Province_State=="From Diamond Princess",]$Province_State <- ""
JHU[JHU$Province_State=="Lackland, TX (From Diamond Princess)",]$Country_Region <- "Diamond Princess"
JHU[JHU$Province_State=="Lackland, TX (From Diamond Princess)",]$Province_State <- ""
JHU[JHU$Province_State=="Omaha, NE (From Diamond Princess)",]$Country_Region <- "Diamond Princess"
JHU[JHU$Province_State=="Omaha, NE (From Diamond Princess)",]$Province_State <- ""
JHU[JHU$Province_State=="Travis, CA (From Diamond Princess)",]$Country_Region <- "Diamond Princess"
JHU[JHU$Province_State=="Travis, CA (From Diamond Princess)",]$Province_State <- ""
JHU[JHU$Province_State=="Unassigned Location (From Diamond Princess)",]$Country_Region <- "Diamond Princess"
JHU[JHU$Province_State=="Unassigned Location (From Diamond Princess)",]$Province_State <- ""
JHU[JHU$Country_Region=="Others" & JHU$Province_State=="Cruise Ship",]$Country_Region <- "Diamond Princess"
JHU[JHU$Country_Region=="Diamond Princess" & JHU$Province_State=="Cruise Ship",]$Province_State <- ""

#Cleanup Grand Princess
JHU[JHU$Province_State=="Grand Princess Cruise Ship",]$Province_State <- "Grand Princess"

#Cleanup South Korea
JHU[JHU$Country_Region=="South Korea",]$Country_Region <- "Korea, South"
JHU[JHU$Country_Region=="Republic of Korea",]$Country_Region <- "Korea, South"

#Cleanup Vatican City
JHU[JHU$Country_Region=="Vatican City",]$Country_Region <- "Holy See"

#Cleanup Gambia
JHU[JHU$Country_Region=="Gambia, The",]$Country_Region <- "Gambia"
JHU[JHU$Country_Region=="The Gambia",]$Country_Region <- "Gambia"

#Cleanup Moldova
JHU[JHU$Country_Region=="Republic of Moldova",]$Country_Region <- "Moldova"

#Cleanup of Republic of Congo
JHU[JHU$Country_Region=="Republic of the Congo",]$Country_Region <- "Congo (Brazzaville)"

#Cleanup of Timor-Leste
JHU[JHU$Country_Region=="East Timor",]$Country_Region <- "Timor-Leste"

#Cleanup Russia
JHU[JHU$Country_Region=="Russian Federation",]$Country_Region <- "Russia"

#Cleanup Cote d'Ivoire
JHU[JHU$Country_Region=="Ivory Coast",]$Country_Region <- "Cote d'Ivoire"

#Cleanup Bahamas
JHU[JHU$Country_Region=="The Bahamas",]$Country_Region <- "Bahamas"
JHU[JHU$Country_Region=="Bahamas, The",]$Country_Region <- "Bahamas"

#Cleanup France
JHU[JHU$Country_Region=="Martinique",]$Province_State <- "Martinique"
JHU[JHU$Country_Region=="Martinique",]$Country_Region <- "France"
JHU[JHU$Country_Region=="Saint Martin",]$Province_State <- "St Martin"
JHU[JHU$Country_Region=="Saint Martin",]$Country_Region <- "France"
JHU[JHU$Country_Region=="St. Martin",]$Province_State <- "St Martin"
JHU[JHU$Country_Region=="St. Martin",]$Country_Region <- "France"
JHU[JHU$Country_Region=="Saint Barthelemy",]$Province_State <- "Saint Barthelemy"
JHU[JHU$Country_Region=="Saint Barthelemy",]$Country_Region <- "France"
JHU[JHU$Country_Region=="Mayotte",]$Province_State <- "Mayotte"
JHU[JHU$Country_Region=="Mayotte",]$Country_Region <- "France"
JHU[JHU$Country_Region=="French Guiana",]$Province_State <- "French Guiana"
JHU[JHU$Country_Region=="French Guiana",]$Country_Region <- "France"
JHU[JHU$Country_Region=="Reunion",]$Province_State <- "Reunion"
JHU[JHU$Country_Region=="Reunion",]$Country_Region <- "France"
JHU[JHU$Country_Region=="Guadeloupe",]$Province_State <- "Guadeloupe"
JHU[JHU$Country_Region=="Guadeloupe",]$Country_Region <- "France"
JHU[JHU$Country_Region=="France" & JHU$Province_State=="Fench Guiana",]$Province_State <- "French Guiana"
JHU[JHU$Country_Region=="France" & JHU$Province_State=="",]$Province_State <- "France"

#Cleanup Netherlands
JHU[JHU$Country_Region=="Curacao",]$Province_State <- "Curacao"
JHU[JHU$Country_Region=="Curacao",]$Country_Region <- "Netherlands"
JHU[JHU$Country_Region=="Aruba",]$Province_State <- "Aruba"
JHU[JHU$Country_Region=="Aruba",]$Country_Region <- "Netherlands"
JHU[JHU$Country_Region=="Netherlands" & JHU$Province_State=="",]$Province_State <- "Netherlands"

#Cleanup Czechia
JHU[JHU$Country_Region=="Czech Republic",]$Country_Region <- "Czechia"

#Cleanup Denmark
JHU[JHU$Country_Region=="Faroe Islands",]$Province_State <- "Faroe Islands"
JHU[JHU$Country_Region=="Faroe Islands",]$Country_Region <- "Denmark"
JHU[JHU$Country_Region=="Greenland",]$Province_State <- "Greenland"
JHU[JHU$Country_Region=="Greenland",]$Country_Region <- "Denmark"
JHU[JHU$Country_Region=="Denmark" & JHU$Province_State=="",]$Province_State <- "Denmark"

#Cleanup Germany. Bavaria was only reported initially.
JHU[JHU$Country_Region=="Germany" & JHU$Province_State=="Bavaria",]$Province_State <- ""

#Cleanup Ireland
JHU[JHU$Country_Region=="Republic of Ireland",]$Country_Region <- "Ireland"

#Cleanup Iran
JHU[JHU$Country_Region=="Iran (Islamic Republic of)",]$Country_Region <- "Iran"

#Cleanup West Bank and Gaza
JHU[JHU$Country_Region=="Palestine",]$Country_Region <- "West Bank and Gaza"
JHU[JHU$Country_Region=="occupied Palestinian territory",]$Country_Region <- "West Bank and Gaza"

#Cleanup United Kingdom. No County_City data has ever been available.
JHU[JHU$Province_State=="Falkland Islands (Islas Malvinas)",]$Province_State <- "Falkland Islands (Malvinas)"
JHU[JHU$Province_State=="UK",]$Country_Region <- "United Kingdom"
JHU[JHU$Province_State=="UK",]$Province_State <- "United Kingdom"
JHU[JHU$Province_State=="United Kingdom",]$Country_Region <- "United Kingdom"
JHU[JHU$Country_Region=="UK",]$Country_Region <- "United Kingdom"
JHU[JHU$Country_Region=="Channel Islands",]$Province_State <- "Channel Islands"
JHU[JHU$Country_Region=="Channel Islands",]$Country_Region <- "United Kingdom"
JHU[JHU$Country_Region=="Jersey",]$Province_State <- "Channel Islands"
JHU[JHU$Country_Region=="Jersey",]$Country_Region <- "United Kingdom"
JHU[JHU$Country_Region=="Guernsey",]$Province_State <- "Channel Islands"
JHU[JHU$Country_Region=="Guernsey",]$Country_Region <- "United Kingdom"
JHU[JHU$Country_Region=="Gibraltar",]$Province_State <- "Gibraltar"
JHU[JHU$Country_Region=="Gibraltar",]$Country_Region <- "United Kingdom"
JHU[JHU$Country_Region=="Cayman Islands",]$Province_State <- "Cayman Islands"
JHU[JHU$Country_Region=="Cayman Islands",]$Country_Region <- "United Kingdom"
JHU <- JHU[!(JHU$Country_Region=="North Ireland"),] #Delete the single dataset prior to North Ireland being counted with rest of United Kingdom
JHU[JHU$Country_Region=="United Kingdom" & JHU$Province_State=="",]$Province_State <- "United Kingdom"

#Cleanup None Province_State
JHU[JHU$Province_State=="None",]$Province_State <- ""

#Cleanup NA County_City
JHU[is.na(JHU$County_City),]$County_City <- ""

#Cleanup Unassigned County_City
JHU[JHU$County_City=="Unknown",]$County_City <- "Unassigned"
JHU[JHU$County_City=="unassigned",]$County_City <- "Unassigned"

#Selecting and ordering columns to use in final data frame
JHU = select(JHU, County_City, Province_State, Country_Region, Last_Update, Lat, Long, Confirmed, Deaths)
#Remove empty rows
JHU <- JHU %>% filter_all(any_vars(!is.na(.)))
#Remove duplicate rows
JHU <- JHU %>% distinct()

scaling <-
  function(places,
           place,
           input_column,
           scale_factor,
           output_column) {
    #places_index <- (places[, 1] == place)
    places_index <- (places[[1]] == place)
    places_place <- places[places_index, ]
    
    for (outside in c(nrow(places_place):2)) {
      inside_start <- outside - 1
      if (inside_start < 1) {
        break
      }
      for (inside in c(inside_start:1)) {
        if (places_place[inside, input_column] == 0) {
          places_place[outside, output_column] <- NA
          break
        }
        if (places_place[outside, input_column] == 0) {
          places_place[outside, output_column] <- NA
          break
        }
        if (places_place[outside, input_column] >= scale_factor * places_place[inside, input_column]) {
          places_place[outside, output_column] <-
            (outside - inside) - (places_place[outside, input_column] / scale_factor - places_place[inside, input_column]) /
            (places_place[inside + 1, input_column] - places_place[inside, input_column])
          break
        }
      }
    }
    places[places_index, output_column] <-
      #places_place[, output_column]
      places_place[[output_column]]
    return(places)
  }

#Load population data
pop <- read.csv(file = 'population.csv')

#Prepare countries data
countries <-
  JHU %>% group_by(Country_Region, Last_Update) %>% summarize(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE)
  )
countries <- merge(countries, pop)
countries <- countries %>%
  mutate(Confirmed_scaling = NA_real_) %>%
  mutate(Deaths_scaling = NA_real_)
  # mutate(Values = NA) %>%
  # mutate(Xaxis = NA)
unique_countries <- unique(countries$Country_Region)

#Prepare states data
states <-
  JHU %>% group_by(Country_Region, Province_State, Last_Update) %>% summarize(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE)
  )
states <- states[!(states$Province_State == ""), ]
states <-
  unite(
    states,
    Country_State,
    c(Country_Region, Province_State),
    sep = " - ",
    remove = FALSE
  )
states <- states %>%
  mutate(Confirmed_scaling = NA_real_) %>%
  mutate(Deaths_scaling = NA_real_)
  # mutate(Values = NA) %>%
  # mutate(Xaxis = NA)
unique_states <- states %>%
  distinct(Country_State)

#Prepare counties data
counties <- JHU[!(JHU$County_City == ""), ]
counties[is.na(counties$Deaths),]$Deaths <- 0
counties <-
  unite(
    counties,
    Country_State,
    c(Country_Region, Province_State),
    sep = " - ",
    remove = FALSE
  )
counties <-
  unite(
    counties,
    State_County,
    c(Province_State, County_City),
    sep = " - ",
    remove = FALSE
  )
counties <- counties %>%
  mutate(Confirmed_scaling = NA_real_) %>%
  mutate(Deaths_scaling = NA_real_)
  # mutate(Values = NA) %>%
  # mutate(Xaxis = NA)
unique_counties <- counties %>%
  distinct(Country_State, State_County)

countries <- countries %>%
  arrange(Country_Region, Last_Update)

states <- states %>%
  arrange(Country_State, Last_Update)

counties <- counties %>%
  arrange(Country_Region, State_County, Last_Update)

#Calculating the doubling times for deaths and confirmed cases
print("Calculating doubling times...")
for (place in c(unique_countries,
                unique_states$Country_State,
                unique_counties$State_County)) {
  print(place)
  
  countries <- scaling(countries, place, "Deaths", 2, "Deaths_scaling")
  states <- scaling(states, place, "Deaths", 2, "Deaths_scaling")
  counties <- scaling(counties, place, "Deaths", 2, "Deaths_scaling")
  
  countries <- scaling(countries, place, "Confirmed", 2, "Confirmed_scaling")
  states <- scaling(states, place, "Confirmed", 2, "Confirmed_scaling")
  counties <- scaling(counties, place, "Confirmed", 2, "Confirmed_scaling")
}

print("Writing CSV files...")
write_csv(JHU,"jhu.csv")
write_csv(countries,"countries.csv")
write_csv(states,"states.csv")
write_csv(counties,"counties.csv")
