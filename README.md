# COVID-19 risk and health care needs of care home residents in England 

#### Project Status: In progress

## Project Description

This analysis describes the care home landscape in England and the healthcare needs and vulnerability to COVID-19 of people permanently living in care homes. Relative to the start of the COVID-19 outbreak in England and Wales, [care homes have seen the biggest increase in deaths over time compared to deaths that have occurred in other settings](https://www.health.org.uk/news-and-comment/charts-and-infographics/deaths-from-any-cause-in-care-homes-have-increased-by-99-per-cent). Deaths in care homes from all causes have increased by 99% since the start of the outbreak.
The aim of this project is to inform medium-term decision making during the COVID-19 pandemic. 

This analysis will address:
* What is the overall risk of the care home population to COVID-19?
* In which areas are homes at particularly high risk?
* What are the potential transmission routes for COVID-19, based on how frequently residents attend hospital?
* WHat is the impact of COVID-19 on secondary care utilisation of care home residents? Are there early signs that care may be deteriorating?

## Data source

The analysis will be performed over several sprints, starting with historical data and updating the results as new data becomes available. 

We are using pseudonymised data on care home characteristics from the Care Quality Commission (similar to the publicly available [care directory on the CQC website](https://www.cqc.org.uk/files/cqc-care-directory-filters-1-april-2020)), linked to pseudonymised patient records from [Secondary Uses Service](https://digital.nhs.uk/services/secondary-uses-service-sus)a national administrative database of all inpatient admissions, A&E attendances and outpatient appointments funded by the NHS in England. Access to this data has been granted as the analysis is carried out under instruction from NHS England.

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data will be accessed in The Health Foundation's Secure Data Environment; a secure data analysis facility (accredited with the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual will be used.

We are also using public data on [deaths in care homes in England](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/numberofdeathsincarehomesnotifiedtothecarequalitycommissionengland) published by the ONS. 

To aggregate and visualise this range of data sources at different geographical levels, we are using a number of publicly available look-up tables and shape files, mostly from the Office for National Statistics:

* shape file used to create [regional](https://data.gov.uk/dataset/18991e29-872b-41e0-8fe0-1bb30d17aee8/regions-december-2016-ultra-generalised-clipped-boundaries-in-england) maps (December 2016 version, also known as local authority districts)
* shape file used to create [Lower Tier Local Authority](https://data.gov.uk/dataset/45a1aaed-503a-4259-bd3e-27ce2ddc7b16/local-authority-districts-december-2016-super-generalised-clipped-boundaries-in-the-uk) maps (December 2016 version)
* shape file used to create [Upper Tier Local Authority](https://data.gov.uk/dataset/53831348-9733-4e52-b9e6-1ddd6be94535/counties-and-unitary-authorities-december-2019-boundaries-uk-buc) maps (December 2019 version)
* look-up table to [map LSOAs (2011) to local authority districts](http://geoportal1-ons.opendata.arcgis.com/datasets/6a46e14a6c2441e3ab08c7b277335558) (February 2020 version)
* look-up table to [match LSOAs (2011) to the English Index of Multiple deprivation (2019)](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019), see Table 7
* look-up table to [match Lower Tier Local Authorities to Upper Tier Local Authorities](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) (April 2019 version)

## How does it work?

As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on similar datasets.  

### Requirements

These scripts were written under R version version 3.6.2 (2019-12-12) -- "Dark and Stormy Night".
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/)(1.3.0)
* [**tidylog**](https://cran.r-project.org/web/packages/tidylog/index.html)(0.2.0)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)
* [**rlang**](https://cran.r-project.org/web/packages/rlang/index.html) (0.4.0)
* [**comorbidity**](https://cran.r-project.org/web/packages/comorbidity/index.html) (0.5.3)
* [**data.table**](https://cran.r-project.org/web/packages/data.table/index.html) (1.12.2)
* [**broom**](https://cran.r-project.org/web/packages/broom/index.html) 
* [**maptools**](https://cran.r-project.org/web/packages/maptools/index.html) 
* [**geojsonio**](https://cran.r-project.org/web/packages/geojsonio/index.html) 

### Getting started

[Sprint 1](src/sprint1_historical) is based on historical CQC data on care homes and SUS data on care home residents from 2017:
* 1_load_data.R - reads data files from csv, saves them as Rds files
* 2_care_homes.R - cleans and analyses care home information
* 3_comorbidities.R - quantifies co-morbidites of care home residents 
* 4_residents.R -  cleans and analyses care home resident characteristics
* 5_visualisation.R - visualises the results scripts of 2-4

[Sprint 2](src/sprint2) analyses current (May 2020) CQC data on COVID deaths in care homes and current (April 2020) CQC data on care homes:
* 1_ONS_COVID_deaths.R - analyses data on all-cause and COVID-related deaths in care homes in England and produces maps
* 2_care_homes.R - cleans and analyses care home information
* 3_care_homes_visualisation.R  - visualises the results script 2

## References
* Improvement Analytics Unit briefing. Emergency admissions to hospital from care homes: how often and what for? 2019. http://www.scie-socialcareonline.org.uk/emergency-admissions-to-hospital-from-care-homes-how-often-and-what-for/r/a110f00000THg3xAAD.
* Quality Watch. Focus on: Hospital admissions from care homes. 2015. https://www.health.org.uk/publications/qualitywatch-focus-on-hospital-admissions-from-care-homes

## Authors
* **Fiona Grimm** - [@fiona_grimm](https://twitter.com/fiona_grimm) - [fiona-grimm](https://github.com/fiona-grimm)

The unnesting function is based on the R-bloggers post ["(Much) faster unnesting with data.table"](https://www.r-bloggers.com/much-faster-unnesting-with-data-table/) by Johannes B. Gruber.

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/COVID19_care_homes/blob/master/LICENSE).