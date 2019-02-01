library(tidyverse)
library(rvest)
library(magrittr)
library(tidycensus)


# Read in MW Data ------------------------------------------------------------


url<-read_html("https://www.epi.org/minimum-wage-tracker/#/min_wage")




# Extract Headings --------------------------------------------------------

headings_main_mw<-  url %>% 
  html_nodes("tr") %>% 
  head(1) %>% 
  html_text() %>% 
  str_split("\\\n") %>% 
  flatten_chr() %>% 
  str_extract_all("(?<=\\[)(.*)(?=\\])") %>% 
  flatten_chr()  

headings_comments_mw<-  
  url %>% 
  html_nodes("tr") %>% 
  head(1) %>% 
  html_text() %>% 
  str_split("\\\n") %>% 
  flatten_chr() %>% 
  str_extract_all("(?<=\\])(.*)") %>% 
  flatten_chr() %>% 
  str_trim()


# Output Dataset ----------------------------------------------------------


dat_mw<-url %>% 
  html_nodes("tr") %>% 
  #head(15) %>% 
  html_text() %>% 
  tibble(ELEMENT=.)
  
MW<-dat_mw %>% 
separate(col=ELEMENT,sep="\\\n",into=headings_comments_mw) %>% 
slice(-1)


names<-c("STATE"
         ,"LOCALITY"
         ,"MINIMUM_WAGE"
         ,"MOST_RECENT_INCREASE_MW"
         ,"UPCOMING_INCREASES_MW"
         ,"INDEXING"
         ,"MOST_RECENT_CHANGE_MW_LAW"
         ,"NOTES_MW"
         ,"TIPPED_WAGE"
         ,"MOST_RECENT_INCREASE_TW"
         ,"UPCOMING_INCREASES_TW"
         ,"MOST_RECENT_CHANGE_TW_LAW"
         ,"NOTES_TW")


MW %<>% 
  set_colnames(names) %>% 
  mutate(MW=MINIMUM_WAGE %>% 
           str_remove_all("\\$") %>% 
           as.numeric()
         ,TW=TIPPED_WAGE %>% 
           str_remove_all("\\$") %>% 
           as.numeric()
         ,STATE=str_to_upper(STATE)
         ,LOCALITY=str_to_upper(LOCALITY)) 

dat<-MW



## define a helper function to convert "" to NA values
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

## transform all columns
dat %<>% mutate_each(funs(empty_as_na)) 


FED_MW<-dat %>% 
  filter(STATE=="FEDERAL") %>% 
  select(MW) %>% 
  pull()

FED_TW<-dat %>% 
  filter(STATE=="FEDERAL") %>% 
  select(TW) %>% pull()

dat %<>%
  filter(STATE!="FEDERAL") %>% 
  mutate(FED_MW=FED_MW,
         FED_TW=FED_TW,
         STATE=if_else(STATE=="WASHINGTON D.C.","DISTRICT OF COLUMBIA",STATE) %>% str_trim())


# FIPS Data ---------------------------------------------------------------


data("fips_codes")


fips<-fips_codes %>% 
  select(state,state_code,state_name) %>% 
  unique() %>% 
  mutate(state=str_to_upper(state)
         ,state_name=str_to_upper(state_name))

dat %<>% 
  inner_join(fips, by=c("STATE"="state_name"))

state_wages<-dat %>% 
  filter(is.na(LOCALITY)) %>% 
  select(STATE,STATE_MW=MW,STATE_TW=TW) %>%
  unique()

dat %<>% 
  inner_join(state_wages,by="STATE")  %>% 
  mutate(LOCALITY_MATCH=LOCALITY %>% 
                        str_remove_all("COUNTY") %>% 
                        str_trim()) 