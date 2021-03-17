

weatherGrabberExtraordinaire = function(df,key,max,test=1,vBy="month") {
data <- df                                 # Store user given dataframe into data
key = key                                  # Store user given key into variable key.  
timeBy = vBy
Sys.setenv(DARKSKY_API_KEY = key)          # Store API Key as a local system enviorment variable for later recall
weatherNames <- c(                         # Create a list of every possible column
  "hourly.time",                           # Available for the DarkSkies API
  "hourly.summary",
  "hourly.icon",                           #    |
  "hourly.precipIntensity",                #    |
  "hourly.precipProbability",              #    |
  "hourly.precipType",                     #    |
  "hourly.temperature",                    #    |
  "hourly.apparentTemperature",            #    |
  "hourly.dewPoint",                       #    |
  "hourly.humidity",                       #    |
  "hourly.pressure",                       #    |
  "hourly.windSpeed",                      #    |
  "hourly.windGust",                       #    |
  "hourly.windBearing",                    #    |
  "hourly.cloudCover",                     #    |
  "hourly.uvIndex",                        #    |
  "hourly.visibility",                     #    |
  "hourly.ozone",                          #    |
  "daily.time",                            #    |
  "daily.summary",                         #    |
  "daily.icon",                            #    |   
  "daily.sunriseTime",                     #    |
  "daily.sunsetTime",                      #    |  
  "daily.moonPhase",                       #    |
  "daily.precipIntensity",                 #    |                   
  "daily.precipIntensityMax",              #    |
  "daily.precipIntensityMaxTime",          #    |
  "daily.precipProbability",               #    |
  "daily.precipType",                      #    |
  "daily.temperatureHigh",                 #    |
  "daily.temperatureHighTime",             #    |
  "daily.temperatureLow",                  #    |
  "daily.temperatureLowTime",              #    |
  "daily.apparentTemperatureHigh",         #    |
  "daily.apparentTemperatureHighTime",     #    |
  "daily.apparentTemperatureLow",          #    |
  "daily.apparentTemperatureLowTime",      #    |
  "daily.dewPoint",                        #    |
  "daily.humidity",                        #    |
  "daily.pressure",                        #    |
  "daily.windSpeed",                       #    |
  "daily.windGust",                        #    |
  "daily.windGustTime",                    #    |
  "daily.windBearing",                     #    |
  "daily.cloudCover",                      #    |
  "daily.uvIndex",                         #    |
  "daily.uvIndexTime",                     #    |
  "daily.visibility",                      #    |
  "daily.ozone",                           #    |
  "daily.temperatureMin",                  #    |
  "daily.temperatureMinTime",              #    |
  "daily.temperatureMax",                  #    |
  "daily.temperatureMaxTime",              #    |
  "daily.apparentTemperatureMin",          #    |
  "daily.apparentTemperatureMinTime",      #    |
  "daily.apparentTemperatureMax",          #    |
  "daily.apparentTemperatureMaxTime",      #    |
  "currently.time",                        #    |
  "currently.summary",                     #    |
  "currently.icon",                        #    |
  "currently.precipIntensity",             #    |
  "currently.precipProbability",           #    |
  "currently.precipType",                  #    |
  "currently.temperature",                 #    |
  "currently.apparentTemperature",         #    |
  "currently.dewPoint",                    #    |
  "currently.humidity",                    #    |
  "currently.pressure",                    #    |
  "currently.windSpeed",                   #    |
  "currently.windGust",                    #    |
  "currently.windBearing",                 #    |
  "currently.cloudCover",                  #    |
  "currently.uvIndex",                     #    |
  "currently.visibility",                  #    |
  "currently.ozone",                       #    |
  "Latitude",                              #    |
  "Longitude",                             #    |
  "WeatherAPIDate.Begin",                  #    |
  "WeatherAPIDate.End")                    #    |
  
  
num <- length(weatherNames)                # Count the number of names from the list above
weather <- data.frame(matrix(ncol = num,   # Create a data frame to store weather results that has the 
                             nrow = 0))    # same dimensions wide as the number of names provided
colnames(weather) <- weatherNames          # Use the list to name the columns of the empty data frame

calls = 0                                  # Begin counter

regions <- data[,c("Latitude",             # From the given user dataframe, extract only the relevent time and coordinates
                   "Longitude",            #     |
                   "WeatherAPIDate.Begin", #     |
                   "WeatherAPIDate.End")]  #     |
                                           #     |

regions <- unique(regions)                 # To reduce the number of calls, Only return unique Geo+Time combinations


for (i in seq(1,nrow(regions),1)) {                               # Begin looping through every row in data frame region.
  vDates <- seq(regions$WeatherAPIDate.Begin[i],                # Get the ith row region Begin Date
              regions$WeatherAPIDate.End[i], timeBy)           # Get the ith row region End Date by month 

for (x in seq(1,length(vDates),1)) {                                # Loop through every month in the harvest season for 
  ref = as.character(vDates[x])                                     # store the xth date for the ith month into ref variable
  
   if(calls < max) {                                                # Check if the number of calls has exceeded user threshold
      if(test==0){                                                     # If not exceeded, proceed to calling
      row  <- get_forecast_for(regions$Latitude[i],                      # Call the ith Latitude 
                               regions$Longitude[i],                     # Call the ith Longitude
                               paste(c(ref,"T12:00:00"),                 # Combine the date and convert it into
                                     collapse = ""))                     # DarkSky approved time format
     df <- data.frame(row)                                               # Store of API request into dateframe df
     df1 <- cbind(df,regions[c(i),])                                     # Bind the appropriate geo+time variables to the 
                                                                         # generated date frame
     weather <- bind_rows(weather,df1)                                   # Stack the dataframes together
     }                                                                   
     calls = calls + 1                                                   # Count added 
     
     } else {
    
          next                                                           # If user set threshold reached.  Finish the loop
   }                                                                     # without further action
  }
}
if(test==0){weather} else {calls}                                        # Return the collected data or the number of calls it will take
}




