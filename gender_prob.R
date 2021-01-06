
####################################################################################
# script to download UK gender stat by first name from 
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/babynamesinenglandandwalesfrom1996
# output:  'names.csv' with a list of first names with gender and probability 
# "R version 4.0.2 (2020-06-22)"
###################################################################################

# load libraries 
if(!require(tidyverse)) install.packages('tidyverse', repos = 'http://cran.us.r-project.org')
if(!require(rvest)) install.packages('rvest', repos = 'http://cran.us.r-project.org')
if(!require(readxl)) install.packages('readxl', repos = 'http://cran.us.r-project.org')

dl <- tempfile()
download.file(paste('https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%',
                    '2fbirthsdeathsandmarriages%2flivebirths%2fdatasets%',
                    '2fbabynamesinenglandandwalesfrom1996%2fcurrent/',
                    'babynames1996to2019.xls', sep=''), mode="wb", dl)

get_data <- function(data_file, who, gender){
  out <- read_excel(data_file, sheet = str_to_title(who), skip=3)
  # get new col names
  new_colnames <- names(out)[!startsWith(names(out),"...")]
  new_colnames <- c("name", new_colnames)
  mask <- !(out%>% slice(1) %>% unlist() == "Rank") #cols to remove
  out <- out %>%  select(which(mask)) # remove columns with Rank
  names(out) <- new_colnames #rename the cols
  out <- out %>%
    filter(name !='NA') %>% 
    mutate('Gender' = gender)
}
# grab data from the original dataset
boys <- get_data(dl, 'boys', 'male')
girls <- get_data(dl, 'girls', 'female')


# combine boys & girls in one set
all_names <- rbind(boys, girls) %>% 
  mutate(name = str_to_title(name))

# extract dups by name(unisex)
temp <- all_names %>%
  group_by(name) %>% 
  filter(n()>1) %>% 
  ungroup() %>% 
  mutate(across(!c('name','Gender'), as.numeric)) %>% 
  arrange(desc(name))
# for those with value in either name, replace NA w/ 0 for the other
temp <- temp %>%
  group_by(name) %>%
  mutate(across(where(is.numeric), ~ if (any(!is.na(.)))
    replace(., is.na(.),0)  else .)) %>% 
    ungroup()

# sum by rows and get proportions to filter
unisex <- temp %>% 
   rowwise %>% # it's a row wise operation next
  mutate( total = sum(c_across(where(is.numeric)), na.rm=TRUE )) %>% #get row sums
  group_by(name) %>% # group for proportion calc
  mutate(prop = prop.table(total)) %>% #calc proportions on total cnt
  select(c('name', 'Gender', 'total', 'prop')) %>%  #remove years
  filter(prop == max(prop)) #remove names with lower props

# names with equal props
unknown  <- unisex %>%
  group_by(name) %>% 
  filter(n()>1) %>% 
  ungroup() %>% 
  mutate(Gender = "unknown") %>% 
  unique()

uniq <- all_names %>% 
    select(name, Gender) %>% 
    group_by(name) %>% 
    filter(n()==1) %>% 
    mutate(prop = 1) %>% 
    ungroup()

# final list - combines 3 "u"s
gender <- bind_rows(uniq, unisex, unknown) %>% 
  select(-total) %>% 
  arrange(desc(name))

# cleanup
rm(temp, boys, girls, dl, get_data, all_names, uniq, unisex, unknown)

# save in R format
saveRDS(gender,  'names.RDS')


