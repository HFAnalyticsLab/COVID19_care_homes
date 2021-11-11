# COVID-19 risk and health care needs of care home residents in England 

#### Project Status: Completed

## Project Description

This project started in May 2020 and originally focused on describing the care home landscape in England and the healthcare needs and vulnerability to COVID-19 of people permanently living in care homes. 

Relative to the start of the COVID-19 outbreak in England and Wales, [care homes saw the biggest increase in deaths over time compared to deaths that have occurred in other settings during the](https://www.health.org.uk/news-and-comment/charts-and-infographics/deaths-from-any-cause-in-care-homes-have-increased-by-99-per-cent). Deaths in care homes from all causes have increased by 99% since the start of the outbreak.

The aim of this project is to produce analysis to inform medium-term decision making during the COVID-19 pandemic. 

## Project phases and outputs

**1.** Sprint 1 included feasibility studies and initial exploration of CQC data on care homes (care home registrations) and SUS data on care home residents from October 2017.

**2.** Analysis of COVID deaths in care homes in relation to the regional distribution of care home beds in England (May 2020). The code from [Sprint 2](src/sprint_2) was used to generate the analysis described in the Health Foundation chart series "[Do all care home residents face an equal risk of dying from COVID-19?](https://www.health.org.uk/news-and-comment/charts-and-infographics/do-all-care-home-residents-face-an-equal-risk-covid-19)" (published 22 May 2020).

**3** Analysis of hospital admissions from care homes and hopsital discharges to care homes before and during the COVID outbreak in March and April 2020.
The analysis from sprint 3 featured in the Health Foundation briefing [Adult social care and COVID-19: Assessing the impact on social care users and staff in England so far](https://www.health.org.uk/publications/report/adult-social-care-and-covid-19-assessing-the-impact-on-social-care-users-and-staff-in-england-so-far) (published July 2020). Sensitivity analyses can be found on [this site](https://hfanalyticslab.github.io/COVID19_care_homes/Sensitivity_analyses.html). 

**4.** Comprehensive analysis of elective and emergency admissions from residential and nursing homes between January and June 2020. The findings from sprint 4 were published in the [International Journal for Population Data Science](https://ijpds.org/article/view/1663). 

## Data sources

The analysis will be performed over several sprints, starting with historical data and updating the results as new data becomes available. 

### Open data
We are also using publicly available data on 
* [Deaths involving COVID-19 in the care sector, England and Wales](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/deathsinvolvingcovid19inthecaresectorenglandandwales/deathsoccurringupto1may2020andregisteredupto9may2020provisional) published by the ONS
* [The number of COVID-19 outbreaks in care homes](https://www.gov.uk/government/statistical-data-sets/covid-19-number-of-outbreaks-in-care-homes-management-information) published by PHE


### Person-level data
We are using pseudonymised data on care home characteristics from the Care Quality Commission (similar to the publicly available [care directory on the CQC website](https://www.cqc.org.uk/files/cqc-care-directory-filters-1-april-2020)), linked to longituginal information on care home residents (Master Patient Index, MPI) and to hospital records from [Secondary Uses Service](https://digital.nhs.uk/services/secondary-uses-service-sus), a national administrative database of all inpatient admissions, A&E attendances and outpatient appointments funded by the NHS in England. Access to this data has been granted as the analysis is carried out under instruction from NHS England.

Using address information from monthly MPI extracts, care home residents can be
identified by assigning Unique Property Reference Numbers (UPRN) to patient addresses
and to addresses of care homes registered with the Care Quality Commission (CQC) and
comparing them.All processing of address information, and subsequent linkage of patient
information, was carried out by the National Commissioning Data Repository (NCDR)
and the analysis of the linked dataset used ‘pseudonymised’ information in a secure
environment hosted by the Health Foundation. 

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data will be accessed in The Health Foundation's Secure Data Environment; a secure data analysis facility (accredited with the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual will be used.


### Reference data
To aggregate and visualise this range of data sources at different geographical levels, we are using a number of publicly available look-up tables and shape files, mostly from the Office for National Statistics:

* shape file used to create [regional](https://data.gov.uk/dataset/18991e29-872b-41e0-8fe0-1bb30d17aee8/regions-december-2016-ultra-generalised-clipped-boundaries-in-england) maps (December 2016 version, also known as local authority districts)
* shape file used to create [Lower Tier Local Authority](https://data.gov.uk/dataset/45a1aaed-503a-4259-bd3e-27ce2ddc7b16/local-authority-districts-december-2016-super-generalised-clipped-boundaries-in-the-uk) maps (December 2016 version)
* [Postcode to Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (February 2020) Lookup in the UK](http://geoportal1-ons.opendata.arcgis.com/datasets/6a46e14a6c2441e3ab08c7b277335558)
* look-up table to [match LSOAs (2011) to the English Index of Multiple deprivation (2019)](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019), see Table 7


## How does it work?

As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on similar datasets.  

### Requirements

These scripts were written under R version version 3.6.2 (2019-12-12) -- "Dark and Stormy Night".
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/) (1.3.0)
* [**tidylog**](https://cran.r-project.org/web/packages/tidylog/index.html) (0.2.0)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)
* [**rlang**](https://cran.r-project.org/web/packages/rlang/index.html) (0.4.0)
* [**comorbidity**](https://cran.r-project.org/web/packages/comorbidity/index.html) (0.5.3)
* [**data.table**](https://cran.r-project.org/web/packages/data.table/index.html) (1.12.2)
* [**broom**](https://cran.r-project.org/web/packages/broom/index.html) 
* [**maptools**](https://cran.r-project.org/web/packages/maptools/index.html) 
* [**geojsonio**](https://cran.r-project.org/web/packages/geojsonio/index.html) 
* [**readODS**](https://cran.r-project.org/web/packages/readODS/index.html) (1.6.7)
* [**readxl**](https://cran.r-project.org/web/packages/readxl/index.html) 
* [**zoo**](https://cran.r-project.org/web/packages/zoo/index.html) 
* [**ggrepel**](https://cran.r-project.org/web/packages/ggrepel/index.html)
* [**RColorBrewer**](https://cran.r-project.org/web/packages/RColorBrewer/index.html)

### Analysis code

#### Utility
* [functions.R](src/functions.R) - functions used across sprints

#### Sprint 1
[Sprint 1](src/sprint_1) is an analysis of historical CQC data on care homes (care home registrations) and SUS data on care home residents from October 2017:
* 1_load_data.R - reads data files from csv, saves them as Rds files
* 2_care_homes.R - cleans and analyses care home information
* 3_comorbidities.R - quantifies co-morbidites of care home residents 
* 4_residents.R -  cleans and analyses care home resident characteristics
* 5_visualisation.R - visualises the results scripts of 2-4


#### Sprint 2
Analysis of COVID deaths in care homes in relation to the regional distribution of care home beds in England, which:
1. combines current CQC data on care homes (care home registrations, April 2020) with ONS data on COVID deaths in care home residents (up to 1 May 2020) to understand how regions in England have been affected and
2. puts this into context with PHE data on the number of care homes that have reported suspected or confirmed
COVID-19 outbreaks (up to 14 May 2020)

[Code](src/sprint_2): 
* 01_ONS_COVID_deaths.R - analyses data on all-cause and COVID-related deaths in care homes in England and produces maps
* 02_care_homes.R - cleans and analyses care home information
* 03_care_homes_visualisation.R  - visualises the results from script 2
* 04_combined_vis.R - combined and visualiseses deaths in care home residents with regional distribution of care homes
* 05_outbreaks.R - analyses and visualises data on COVID-19 outbreaks in care homes 


#### Sprint 3
Analysis of hospital admissions from care homes and hopsital discharges to care homes before and during the COVID outbreak in March and April 2020.

[R code](src/sprint_3) used to clean CQC and MPI data, to analyse characteristics and admissions/discharges of *permanent* care home residents and to visualise results using the SAS code below. 

* 01_clean_CQC.R - cleans pseudonimysed care home characteristics
* 02_clean_MPI.R - cleaning pseudonimysed master patient index for care home residents and their long-term conditions
* 03_permresidents_descriptives.R - descriptive analysis of characteristics of permament care home residents
* 04_permresidents_admissions_discharges.R - analysis of hospital admissions and discharges for permanent care home residents (MPI flag only)
* 05_allresidents_admissions_discharges_regional.R - analysis of hospital admissions and discharges for all care home residents 
* 06_allresidents_admissions_admtype_covid.R - as above, but split by care home type and COVID primary diagnosis, or admission type
* 07_permlresidents_descriptives_dataviz_post-release.R - visualisation of characteristics from script 3
* 08_allresidents_admissions_discharges_dataviz_post-release.R - visualisation of admissions and discharges from script 5
* 09_sensitivity_LOS45_post-release.R - sensitivity analysis excluding long spells (leght of stay less than 45 days)
* 10_sensitivity_MPI_post-release.R - sensitivity analysis only including permanent residents with an address match
* Sensitivity_analyses.Rmd - creating visualisations for sensitivity analyses

*Sensitivity anaylses*, excluding long spells or excluding admissinos/discharges where the patient did not have an MPI adress match to a care home, can be found as html file in this folder.

[SAS code](src/sprint_3_SAS) used to count the number of hospital admissions from care homes and hospital discharges to care homes (based on both MPI care home flags and SUS source of admission and discharge destination), and to flag long-term conditions based on inpatient diagnosis codes of the previous 3 years. 

* 01_ReplicateNHSMethodology - flags hospital admissions from care homes and hospital discharges to care homes 
* 01a_ExcludeLOS45plus - generate subset needed for sensitivity analysis on length of stay
* 02_IdentifyMMPICarehomeResidents - flag care home admissions and discharges based on MPI records
* 03a_SumAdmissionsByDayMonth_AllSpells - - create summaries for count admission (MPI + SUS flag)
* 03b_SumDischargesByDayMonth_AllSpells - create summaries for discharges (MPI + SUS flag)
* 03c_SumAdmissionsByDayMonth_ExcLOS45plus - create summaries for count admission (LOS restricted)
* 03d_SumDischargesByDayMonth_ExcLOS45plus- create summaries for discharges (LOS restricted)
* 03e_SumAdmissionsByDayMonth_MPICHRes - create discharge summaries for permament residentes (MPI only)
* 03f_SumDischargesByDayMonth_MPICHRes - create admissions summaries for permament residentes (MPI only)

#### Sprint 4

[R code](src/sprint_4) used to clean CQC and MPI data, to analyse characteristics and trends in hopsital admission rates of two cohorts of *permanent* care home residents (January 2019 and 2020) and to investigate admissions reasons. 

* 01_clean_CQC.R - cleans pseudonimysed care home characteristics
* 02_clean_MPI.R - cleaning pseudonimysed master patient index for care home residents and their long-term conditions
* 03_define_cohorts.R - define cohorts and follow up period, descriptive analysis of characteristics of permament care home residents
* 04_process_admissions.R - clean and filter hospital spells for the study cohorts, descriptive analysis of admitted patients
* 04a_procedures.R - count number of admissions with cataract procedures
* 05_define_denominators.R - count number of days spent in care home during the follow up period
* 07_admission_rates_weekly.R - calculate and visualise weekly admission rates
* 08_admission_rates_March-May.R - calculate admission rates for the period between 1 March and 31 May
* 09_admission_causes.R - calculate changes in primary reasons for admissions 

## References
* Improvement Analytics Unit briefing. [Emergency admissions to hospital from care homes: how often and what for?](http://www.scie-socialcareonline.org.uk/emergency-admissions-to-hospital-from-care-homes-how-often-and-what-for/r/a110f00000THg3xAAD) 2019. .
* Quality Watch. Focus on: Hospital admissions from care homes. 2015. https://www.health.org.uk/publications/qualitywatch-focus-on-hospital-admissions-from-care-homes
* The Health Foundation COVID-19 chart series, 13 May 2020, [Care homes have seen the biggest increase in deaths since the start of the outbreak](https://www.health.org.uk/news-and-comment/charts-and-infographics/deaths-from-any-cause-in-care-homes-have-increased)
* The Health Foundation press release, 15 May 2020, [Care home deaths from COVID-19 over 50% higher than previously estimated](https://www.health.org.uk/news-and-comment/news/care-home-deaths-from-covid-19-over-50-per-cent-higher-than-previously-estimated)

## Authors
* **Fiona Grimm** - on [Twitter](https://twitter.com/fiona_grimm) or [GitHub](https://github.com/fiona-grimm)
* **Karen Hodgson** - on [Twitter](https://twitter.com/KarenHodgePodge) or [GitHub](https://github.com/KarenHodgson)
* **Richard Brine** - on [GitHub](https://github.com/richardbrine)

The unnesting function is based on the R-bloggers post ["(Much) faster unnesting with data.table"](https://www.r-bloggers.com/much-faster-unnesting-with-data-table/) by Johannes B. Gruber.

The interval join using `data.table::foverlaps()` used in sprint 3 (script 04) is based on a helpful explanation on R bloggers by Adnan Fiaz (["In between a rock and a conditional join"](https://www.r-bloggers.com/in-between-a-rock-and-a-conditional-join/)).

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/COVID19_care_homes/blob/master/LICENSE).
