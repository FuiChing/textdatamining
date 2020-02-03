# textdatamining
TDR data mining 

setwd("C:/Users/louis/Desktop/MEGA/Working experience/United Nation/Consultant Nov 2019/R")

rm(list = ls()) 

install.packages("readtext")
install.packages("stringr")
install.packages("dplyr")
install.packages("splitstackshape")
install.packages("readxl")
install.packages('psych')

library(readtext)
library(stringr)
library(tidyverse)
library(dplyr)
library(splitstackshape)
library(readxl)
library(psych)

### data import
loi_raw <- readtext::readtext("C:/Users/louis/Desktop/MEGA/Working experience/United Nation/Consultant Nov 2019/loi/*")
loi_raw$evaluation_score <- 1                                     # Loi evaluation scores for highest (from 1 (highest) to 3 lowest)).
loi_raw$serial_number<- paste("loi_", 1:nrow(loi_raw), sep = "")  # add serial number
loi_raw$text <- gsub("Reference*.*",'',loi_raw$text)              # remove everything after references
loi_raw$text <- gsub("REFERENCE*.*",'',loi_raw$text)
loi_raw$text <- gsub('[[:punct:] ]+',' ',loi_raw$text)               # remove punct
loi_raw$text <-  gsub("http[[:alnum:][:punct:]]*", "", loi_raw$text) #remove URL / Hyperlink
loi_raw$text <- gsub("http[^[:space:]]*", "", loi_raw$text)          #remove URL / Hyperlink
loi_raw$text <- gsub('(f|ht)tp\\S+\\s*',"", loi_raw$text)            #remove URL / Hyperlink

loi_raw$text <- tolower(loi_raw$text)                               #lower the letter

loi_raw$nchar <- nchar (loi_raw$text, type = "chars", allowNA = FALSE)
hist(loi_raw$nchar)
describe(loi_raw$nchar)


#Stage1
### Disease
diseases_list <- "HIV|TB|Tuberc[uo]los[ei]s|Malaria|Influenzae|Dengue|Rab[gi]es|Trachoma|Yaws|
                  Lepr[eo]sy|Chagas|Leishmani[oa]ses|Taeniasis|neurocysticercos[ei]s|Dracuncul[oi]as[ei]s|
                  Echinococcos[ei]s|Onchocerc[oi]as[ei]s|Schistosomias[ei]s|Mycetoma|Buruli\\s+ulcer|
                  Human\\s+immunodeficiency\\s+viruses|trypanosomiasis|Sleeping\\s+sickness|Guinea\\s+worm|
                  Foodborne\\s+trematodiases|Lymphatic\\s+filariasis|river\\s+blindness|
                  Soil\\s+transmitted\\s+helminthiases"

diseases_list <- tolower(diseases_list)

loi_raw$diseases <- str_extract_all(loi_raw$text, diseases_list)  # more than one diseases
loi_diseases <- cSplit(loi_raw, "diseases", ",", "long")

loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\btb\\b", "tuberculosis")    # replace abbreviation
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "character\\(0\\)", "")        #remove "character(0)"
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "c\\(", "")                    #remove c(
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\)", "")                     #remove )
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\"", "") 
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\\\r\\\\n ", " ")
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\\\r\\\\n", " ")

### country
country_list <- "Algeria|Angola|Benin|Burkina\\s+Faso|Camero[uo]n|Comoros|
                Cape\\s+Verde|Chad|Equatorial\\s+Guine[ea]|Gabon|Gambi[ea]|Ghana|
                Guine[ea]$|Guine[ea]\\s+Bissau|Liberia|Madagascar|Mali|Mauritani[ea]|
                Mauritius|Niger$|Nigeria|Macedonia|Serbia|South\\s+Sudan|Tanzania|
                Venezuel|Moldova|Montenegro|Sao\\s+Tome\\s+and Principe|Senegal|Seychelles|
                Sierra\\s+Leone|Togo|Botswana|Burundi|Central\\s+African\\s+Republic|Congo|
                Côte\\s+d\\s+Ivoire|Congo|Eritrea|Ethiopia|Kenya|Lesotho|Malawi|Mozambique|
                Namibia|Rwanda|South\\s+Africa|Swaziland|Uganda|Tanzania|Zambia|Zimbabwe|Canada|
                Cuba|United\\s+States\\s+of\\s+America|Antigua\\s+and\\s+Barbuda|Argentina|Bahamas|
                Barbados|Belize|Brazil|Chile|Colombia|Costa\\s+Rica|Dominica$|Dominican\\s+Republic|
                El\\s+Salvador|Grenada|Guyana|Honduras|Jamaica|Mexico|Panama|Paraguay|
                Saint\\s+Kitts\\s+and\\s+Nevis|Saint\\s+Lucia|Saint\\s+Vincent\\s+and\\s+the\\s+Grenadines|
                Suriname|Trinidad\\s+and\\s+Tobago|Uruguay|Venezuela|Bolivia|Ecuador|
                Guatemala|Haiti|Nicaragua|Peru|Bahrain|Cyprus|Iran|Jordan|Kuwait|Lebanon|
                Libyan\\s+Arab\\s+Jamahiriya|Libya|Oman|Qatar|Saudi\\s+Arabia|S[iy]ria|Tunisia|
                United\\s+Arab\\s+Emirates|Afghanistan|Djibouti|Egypt|Ira[qk]|Morocco|Pakistan|
                Somalia|Sudan|Yemen|Andorra|Austria|Belgium|Croatia|Czech\\s+Republic|Denmark|
                Finland|France|Germany|Greece|Iceland|Ireland|Israel|Italy|Luxembourg|Malta|
                Monaco|Netherlands|Norway|Portugal|San\\s+Marino|Slovenia|Spain|Sweden|Switzerland|
                United\\s+Kingdom|Albania|Armenia|Azerbaijan|Bosnia\\s+and\\s+Herzegovina|Bulgaria|
                Georgia|Kyrgyzstan|Poland|Romania|Slovakia|Tajikistan|Turkey|Turkmenistan|
                Uzbekistan|Yugoslavia|Belarus|Estonia|Hungary|Kazakhstan|Latvia|Lithuania|
                Moldova|Russian|Ukraine|Indonesia|Sri\\s+Lanka|Thailand|Timor\\s+Leste|Bangladesh|
                Bhutan|South\\s+Korea|India|Maldives|Myanmar|Nepal|Australia|Brunei|Japan|New\\s+Zealand|
                Singapore|Cambodia|China|Cook\\s+Islands|Fiji|Kiribati|Lao|Malaysia|Marshall\\s+Islands|
                Micronesia|Mongolia|Nauru|Niue|Palau|Papua\\s+New\\s+Guinea|Philippines|North\\s+Korea|
                Samoa|Solomon\\s+Islands|Tonga|Tuvalu|Vanuatu|Vietnam|Viet\\s+Nam"

country_list <- tolower(country_list)
  
loi_diseases$country<- str_extract_all(loi_diseases$text, country_list)  # more than one country
loi_diseases<- cSplit(loi_diseases, "country", ",", "long")


loi_diseases$country <- str_replace_all(loi_diseases$country , "character\\(0\\)", "")        #remove "character(0)"
loi_diseases$country <- str_replace_all(loi_diseases$country , "c\\(", "")                    #remove c(
loi_diseases$country <- str_replace_all(loi_diseases$country , "\\)", "")                     #remove )
loi_diseases$country <- str_replace_all(loi_diseases$country , "\"", "") 
loi_diseases$country <- str_replace_all(loi_diseases$country , "\\\r\\\n ", "") #to check
loi_diseases$country <- str_replace_all(loi_diseases$country , "\"", "")           #remove ""
loi_diseases$country <- str_replace_all(loi_diseases$country , "\\\\r\\\\n ", " ")
 

loi_diseases <- loi_diseases[!duplicated(loi_diseases), by = c("serial_number, diseases, country")]

names(loi_diseases)


### by Region according to WHO code
WHO_AFRO = list("Algeria",
                "Angola",
                "Benin",
                "Burkina Faso",
                "Cameroon",
                "Cape Verde",
                "Chad",
                "Equatorial Guinea",
                "Gabon",
                "Gambia",
                "Ghana",
                "Guinea",
                "Guinea Bissau",
                "Liberia",
                "Madagascar",
                "Mali",
                "Mauritania",
                "Mauritius",
                "Niger",
                "Nigeria",
                "Sao Tome and Principe",
                "Senegal",
                "Seychelles",
                "Sierra Leone",
                "Togo",
                "Botswana",
                "Burundi",
                "Central African Republic",
                "Congo",
                "C??te d Ivoire",
                "Eritrea",
                "Ethiopia",
                "Kenya",
                "Lesotho",
                "Malawi",
                "Mozambique",
                "Namibia",
                "Rwanda",
                "South Africa",
                "Swaziland",
                "Uganda",
                "Zambia",
                "Zimbabwe",
                "Comoros",
                "Tanzania")
WHO_AMRO = list("Canada",
                "Cuba",
                "United States of America",
                "Antigua and Barbuda",
                "Argentina",
                "Bahamas",
                "Barbados",
                "Belize",
                "Brazil",
                "Chile",
                "Colombia",
                "Costa Rica",
                "Dominica",
                "Dominican",
                "El Salvador",
                "Grenada",
                "Guyana",
                "Honduras",
                "Jamaica",
                "Mexico",
                "Panama",
                "Paraguay",
                "Saint Kitts and Nevis",
                "Saint Lucia",
                "Saint Vincent and the Grenadines",
                "Suriname",
                "Trinidad and Tobago",
                "Uruguay",
                "Venezuela",
                "Bolivia",
                "Ecuador",
                "Guatemala",
                "Haiti",
                "Nicaragua",
                "Peru",
                "Venezuela")
WHO_EMRO = list("Bahrain",
                "Cyprus",
                "Iran",
                "Jordan",
                "Kuwait",
                "Lebanon",
                "Oman",
                "Qatar",
                "Saudi Arabia",
                "Syria",
                "Tunisia",
                "United Arab Emirates",
                "Afghanistan",
                "Djibouti",
                "Egypt",
                "Iraq",
                "Morocco",
                "Pakistan",
                "Somalia",
                "Sudan",
                "Yemen",
                "Libya")
WHO_EURO = list("Andorra",
                "Austria",
                "Belgium",
                "Croatia",
                "Czech Republic",
                "Denmark",
                "Finland",
                "France",
                "Germany",
                "Greece",
                "Iceland",
                "Ireland",
                "Israel",
                "Italy",
                "Luxembourg",
                "Malta",
                "Monaco",
                "Netherlands",
                "Norway",
                "Portugal",
                "San Marino",
                "Slovenia",
                "Spain",
                "Sweden",
                "Switzerland",
                "United Kingdom",
                "Albania",
                "Armenia",
                "Azerbaijan",
                "Bosnia and Herzegovina",
                "Bulgaria",
                "Georgia",
                "Kyrgyzstan",
                "Poland",
                "Romania",
                "Slovakia",
                "Tajikistan",
                "Macedonia",
                "Turkey",
                "Turkmenistan",
                "Uzbekistan",
                "Yugoslavia",
                "Belarus",
                "Estonia",
                "Hungary",
                "Kazakhstan",
                "Latvia",
                "Lithuania",
                "Russian Federation",
                "Ukraine",
                "Moldova",
                "Montenegro",
                "Serbia")
WHO_SEARO = list("Indonesia",
                 "Sri Lanka",
                 "Thailand",
                 "Timor Leste",
                 "Bangladesh",
                 "Bhutan",
                 "North Korea",
                 "India",
                 "Maldives",
                 "Myanmar",
                 "Nepal")
WHO_WPRO = list("Australia",
                "Brunei",
                "Japan",
                "New Zealand",
                "Singapore",
                "Cambodia",
                "China",
                "Cook Islands",
                "Fiji",
                "Kiribati",
                "Lao",
                "Malaysia",
                "Marshall Islands",
                "Micronesia",
                "Mongolia",
                "Nauru",
                "Niue",
                "Palau",
                "Papua New Guinea",
                "Philippines",
                "South Korea",
                "Samoa",
                "Solomon Islands",
                "Tonga",
                "Tuvalu",
                "Vanuatu",
                "Vietnam",
                "Viet nam")

WHO_AFRO <- tolower(WHO_AFRO)
WHO_AMRO <- tolower(WHO_AMRO)
WHO_EMRO <- tolower(WHO_EMRO)
WHO_EURO <- tolower(WHO_EURO)
WHO_SEARO <- tolower(WHO_SEARO)
WHO_WPRO <- tolower(WHO_WPRO)

loi_diseases <- mutate(loi_diseases, WHO_region = ifelse(country %in% WHO_AFRO, "WHO_AFRO",
                                                        ifelse(country %in% WHO_AMRO, "WHO_AMRO",
                                                              ifelse(country %in% WHO_EMRO, "WHO_EMRO",
                                                                    ifelse(country %in% WHO_EURO, "WHO_EURO",
                                                                          ifelse(country %in% WHO_SEARO,"WHO_SEARO",
                                                                                ifelse(country %in% WHO_WPRO,"WHO_WPRO", "NA")))))))
    
### world bank income group
wb <- read_excel("C:/Users/louis/Desktop/MEGA/Working experience/United Nation/Consultant Nov 2019/Country/CLASS.xls")
income_wb <- wb %>% select(4,8)
income_wb <- income_wb[-c(1:5),]
names(income_wb)[1] <- "country"
names(income_wb)[2] <- "World_Bank_income_group"
income_wb$country <- gsub('[[:punct:]]',' ',income_wb$country)
income_wb$country <- tolower(income_wb$country)
loi_diseases <- left_join(loi_diseases,income_wb, by = "country")


#save
write.csv(loi_diseases,"loi 20191220.csv")
