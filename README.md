# Nanoblocks
Hello everyone, the following repository will try to encapsulate most of the steps of an ETL process, a basic analysis process and a dashboard. Although this wasn’t the real purpose of the project, I will perform most of the common tasks of a data analyst.
> The real purpose was to practice R and web scraping.

## ETL process:
### Extract ⛏
The extraction process for this project consisted on `scraping` data from the official catalog of the [Nanoblocks page](https://www.kawada-toys.com/).  
Also get the individual links of each products and additional information from the [Rakuten page](https://www.rakuten.co.jp/).   
The extraction was done using the programming language `R` in conjunction with some packages such as `RSelenium` and `Tidyverse`.
### Transform 🧹
The transformation process was done completely in `R` with `Tidyverse` and `Lubridate`.  
Some of the transformations consisted on:
- Removing duplicates
- Standardizing observations
- Removing spaces, symbols 
- Extracting numbers of descriptions
- Parsing to the correct format  

Among other things, making the datasets easier to analyze in the following steps.
### Load ✈
Load is the *last* step in the process of ETL. To do this I upload both of the datasets on SQL Server manually as the connector, in R, is not as good with this flavor of SQL as with the others.  
Can I use SQL to run the analysis queries for this project?
> Yes, although I will only use R to do the analysis, and maybe run the exploratory queries on SQL for practice.

## Analysis Process:
> **I am still working in this process, but the steps that I will follow are described here.⚠**  

For the analysis process, I will perform most, if not all, the process in R with the following framework:
> This framework consists of the common steps before analyzing or modeling anything.  

### View the raw data and sampling 👀
Without viewing the raw data, any project can get harder as you don’t know what you might encounter such as missing or erroneous entries.  
Also provides a quick way to get familiarity with the data, although I can avoid this step as I was the one who recollected and cleaned it.

### Get the summary statistics 🧮
Helps to know the distribution, dispersion, skewness, common values.  
This can be crucial to know which types of models, plots and how to approach the data. 

### Plot variables of interest 📊
Selecting the columns of interest to make some univariate and bivariate plots.  
This will help to know the relationship and effects of exploratory variables in the outcomes.

## Dashboarding:
Not ready at this moment, hope to create it soon.
