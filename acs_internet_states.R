library(tidycensus)
library(tidyverse)

#load variable names 
#filter for variables focused on internet 
vars <- load_variables(2023, "acs1/cprofile", cache = TRUE) %>% 
  filter(concept != 'Comparative Social Characteristics in Puerto Rico') %>%
  filter(str_detect(label, 'COMPUTERS AND INTERNET')) %>% 
  #extract year and variable name 
  #need to call rowwise for mutate() to work with strsplit and unlist 
  rowwise() %>% 
  mutate(Year = str_extract(label, "\\d{4}"),
         Variable_Name = tail(unlist(strsplit(label, "!!")), 1)) %>% 
  #rowwise() groups data so need to ungroup() here to avoid unintended behavior 
  ungroup()

# Get comparison profile for each state in 2019-2023
states_raw <- get_acs(
  geography = "state",
  table = "CP02",
  year = 2023,
  survey = "acs1",
  cache_table = TRUE)

#inner join will filter c profile for relevant information 
states <- states_raw %>% 
  inner_join(vars, by=c('variable'='name')) %>% 
  select(Year, GEOID, NAME, Variable_Name, estimate)

#format data so variable names are column names 
states_wider <- states %>% 
  pivot_wider(names_from = "Variable_Name", values_from = "estimate") %>% 
  #convert numbers to percentage format (helps with BI tools)
  mutate(`With a computer`= `With a computer`/100,
         `With a broadband Internet subscription` = `With a broadband Internet subscription`/100) %>% 
  #include raw data to perform aggregate calculations in BI tools 
  mutate(`Households with a computer`= `With a computer`*`Total households`,
         `Households with broadband` = `With a broadband Internet subscription`*`Total households`) %>% 
  rename(State = NAME,
         `Percentage households with a computer` = `With a computer`,
         `Percentage households with broadband` = `With a broadband Internet subscription`) %>% 
  #filter out empty data for PR
  filter(GEOID != 72)

#write to Excel 
writexl::write_xlsx(list("State" = states_wider), 'state_internet_acs.xlsx') 
  
