# Daymet-to-data
A shiny app that allows users to access interpolated historical weather data

## What is Daymet?

Daymet is software that takes inputs from multiple ground-based weather stations and outputs estimated daily maximum and minimum temperature, precipitation, humidity and radiation for locations outside the coverage area of any one particular weather station. This gives researchers interested in remote locations access to important meteorological data dating back to 1980.

## The App
This app provides easy access to the data produced by Daymet. It features an easy to use point-and-click interface that requires no prior programming knowledge to navigate. Users are given the option to identify locations with either latitude and longitude, or zip code, and all the data from each location is compiled into a single csv file for download.

## Inputs

Users will need to upload a file containing a column for location id (string or number) and either a column for zipcode or separate columns for latitude and longitude. For example,


![alt text](./DaymetRShiny/www/header_zip.PNG "Description goes here")

## Output

Embedded is an interactive plot for basic exploration and available for download is a csv of the following format:

![alt text](./DaymetRShiny/www/output.PNG "Description goes here")
  

