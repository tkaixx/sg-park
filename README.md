# SGPark 

## Project Overview
Our team (DBA3702 Team 6) at the National University of Singapore developed this project to enhance drivers' experiences on the road. Drivers can easily locate nearby carparks, access real-time information on parking lot availability and carpark rates, and find nearby facilities. Additionally, users can plan their trips by reviewing historical peak hours and available parking lots at HDB and shopping mall carparks, with detailed carpark rates based on time intervals.

## Data Sources
We have gathered our data from a variety of sources, with a combination of both dynamic and static datasets.

1. SG Carpark live API (https://sgcarparks.atpeaz.com/h#google_vignette)
2. Urban Redevelopment Authority (URA)'s live API of carpark availability (https://www.ura.gov.sg/Corporate/Car-Parks)
3. Malls data (https://github.com/ValaryLim/Mall-Coordinates-Web-Scraper/blob/master/mall_coordinates_updated.csv)
4. Hawker Centre Data (https://gist.github.com/raphodn/eecc3d8f6a5a04793385f44aa8304e30)

## Features 
Our app developed using RShiny App has 4 main features.

A. Bar plot of HDB & Mall Parking lots availability against time, from 5am to 12am
- Users can select (1)location and (2)day of the week
- Bar charts are color-coded. Red bar chart indicates peak hours where lots availability is below 25% of full parking lots capacity, while green indicates otherwise.
  
B. Map Application
- Shows an overview of the number of available lots in the areas nearby.
  
C. Parking Rates Calculator
- User can easily choose their desired location and key in their planned duration, in which our pricing calculator will compute the estimated parking rates, helping users to plan for their trip efficiently.
  
D. Amenities Nearby
- Users can easily checkbox their desired amenities ranging from hawker centres, malls, carparks nearby, in which selected icons would pop up.

