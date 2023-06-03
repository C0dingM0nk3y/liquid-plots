# init_externalLibs.R
#> On first run: dowlonad/install/load all required libraries from CRAN.
#> On further runs: pacmann will check if library is installed, and load it to the environment

### Dependences
#> API requests
pacman::p_load('digest') #https://www.rdocumentation.org/packages/digest/versions/0.6.29/topics/hmac
pacman::p_load('httr') #API tools #https://www.dataquest.io/blog/r-api-tutorial/
pacman::p_load('jsonlite')

#> Data Analysis 
pacman::p_load('magrittr')
pacman::p_load('stringr')
pacman::p_load("tidyverse")
#pacman::p_load("data.table")
pacman::p_load("lubridate")

#> Data visualization
pacman::p_load('kableExtra')
pacman::p_load("ggplot2")
pacman::p_load("ggrepel")
pacman::p_load("patchwork") #GUIDE: https://aosmith.rbind.io/2019/05/13/small-multiples-plot/

#general options
options(digits = 12) #max decimal digits
options(scipen=999) #scientific notation threshold