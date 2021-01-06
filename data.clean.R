## Clear workspace 
rm(list=ls())

# Grab libraries 
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employmentbyoccupationemp04


if(!require(tidyverse)) install.packages('tidyverse', repos = 'http://cran.us.r-project.org')
if(!require(data.table)) install.packages('data.table', repos = 'http://cran.us.r-project.org')
if(!require(rvest)) install.packages('rvest', repos = 'http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', repos = 'http://cran.us.r-project.org')
if(!require(corrplot)) install.packages('corrplot', repos = 'http://cran.us.r-project.org')
if(!require(e1071)) install.packages('e1071', repos = 'http://cran.us.r-project.org')

#1. EXTRACT:  Download gender pay gap data:
  dat_url <- 'https://gender-pay-gap.service.gov.uk/viewing/download-data/2019'
  dat <- read_csv(dat_url) 
  
  dat %>% #remove extra columns
    select(-SubmittedAfterTheDeadline, -DueDate, -DateSubmitted) %>% 
    mutate(EmployerName = str_to_title(EmployerName)) -> dat
  
  saveRDS(dat,  'dat.RDS') # save in R format


#2.2 group same company submissions into one record using mean data
  #2.2.1 Convert EmployerSize - could potentially take a midpoint of the range since unequal length but not impactful
  df <- data.frame(EmployerSize=c('Less than 250', '250 to 499', '500 to 999', '1000 to 4999', '5000 to 19,999', '20,000 or more'),
                  MinEmployees = c(1,250,500, 1000, 5000, 20000))
  
  #2.2.2. Group by 'ResponsiblePerson' & remove columns that would not be in use
  temp <- dat %>%
    left_join(df, c('EmployerSize' = 'EmployerSize')) %>%
    filter(!is.na(ResponsiblePerson)) %>% #removing NA will be added back later
    group_by(ResponsiblePerson) %>% 
    summarize(
      across(where(is.numeric) & !starts_with('Min'), mean),
      SicCodes = toString(unique(SicCodes)),
      Name = toString(unique(EmployerName)),
      across(c('EmployerName','SicCodes'), ~toString(.x)),
      MinEmployees = max(MinEmployees),
      WebLink =  CompanyLinkToGPGInfo[which(MinEmployees == max(MinEmployees))])  
 
  tempNA <-  dat %>%
    select(-c(Address, CompanyNumber, CurrentName)) %>% #drop a few of fields
    rename(WebLink=CompanyLinkToGPGInfo) %>% 
    left_join(df, c('EmployerSize' = 'EmployerSize')) %>%
    filter(is.na(ResponsiblePerson))    #removed NA from temp 
    
  
  #3. Extract name and function to get gender of C level execs
  temp <- temp %>% 
    mutate(RespPerson = str_replace(str_to_title(ResponsiblePerson), "(?:(Dr|Professor|Sir)\\ )",'')) %>% #remove prefix Dr|Sir 
    extract(RespPerson, into=c('name', 'title'), '^(\\w+).*?\\(([^)]+?)\\)') %>% # https://regex101.com/r/lXLuRP/2/
    mutate(title = str_squish(str_replace_all(title, c('\\.'= '', '\\-'=' '))))
   
   
    # 3.1 assign C-level titles   
    # titles <- temp[!duplicated(temp[,'title']), c('name', 'title')]
   titles <- temp[!duplicated(temp[,'title']),  'title']
          # https://regex101.com/r/h2m3Ut/5/
   regx <- "^(?!.*\\b(?:Hr|Staff|Human|People|Personnel|Talent|Sales|Compensation|Benefits|Reward|Compliance|Vice)).*\\b(?:Ceo|Cfo|Cheif|Chief|Head|Chair|Chairman|Founder|Partner|President)\\b.*$"
   titles$CLevel <- grepl(regx, titles$title, ignore.case=T, perl=T)
   
    # 3.2.  build a gender db from UK office of national statistics dataset @ https://www.ons.gov.uk
  if (!file.exists('names.RDS')){
      source('gender_prob.R')
  }else{
      gender <- readRDS('names.RDS')} #if RDS file exists, gender_prob.R script won't run
   
   names <-  temp[!duplicated(temp[,'name']), 'name'] %>% 
     left_join(gender, c('name' = 'name')) 
   
   #3.3  add industry section  
   #scrape industry codes and add section (one of 21 SiC)
   
   url <- 'https://resources.companieshouse.gov.uk/sic/'
   list <-  read_html(url) %>% 
     html_nodes('table') %>% 
     html_table()
   
   #convert nested list to df
   df <- data.frame(Reduce(rbind, list))
   
   #add an industry SIC section to lookup table
   df %>% 
     mutate(section=replace(Description, !grepl('Section', Code), NA))  %>%
     fill(section) -> df
   
   # 4. LOAD
   
     # 4.1.  add C-level & gender back to temp 
    temp <- temp %>% 
      left_join(titles, c('title' = 'title')) %>% 
      left_join(names, c('name' = 'name'))
    
    # 4.2 stack back vertically with tempNA
    temp <- tempNA %>%
      select(-EmployerSize) %>% 
      bind_rows(select(temp, -c(Name, prop)))
    
    
    # 4.3. Add section to 'temp' based on prevalent codes
    dat.cleaned <- temp %>%
      ungroup %>% 
      mutate(row = row_number()) %>% #keep track of original rows
      separate_rows(SicCodes, sep = ',\n') %>% #sep rows by code
      left_join(df, c('SicCodes' = 'Code')) %>% #join by code
      group_by(row) %>% #group 
      mutate(SicCodes = toString(SicCodes), #write multiple codes to string
             Description = toString(Description), #write multiple desc to string
             section = {nm = sort(table(section), decreasing = TRUE)[1];if(!is.na(nm)) names(nm) else NA}) %>%   #add section
      unique() %>%   #remove dupe rows
      ungroup() %>% 
      select(-row) %>% 
      arrange(desc(EmployerName))
    
    #############################
    #  FEATURE ENGINEERING
    ###############################
    
  #  5.1 re-engineer quartile variables by adding gender skew 
    cleaned.reshaped <- dat.cleaned %>%
      select(!c(DiffMedianHourlyPercent, SicCodes, DiffMeanBonusPercent, DiffMedianBonusPercent, MaleBonusPercent, FemaleBonusPercent,
               WebLink, ResponsiblePerson, name, title, CLevel, Description))
    
    cleaned.reshaped <- cleaned.reshaped %>% 
      mutate(pctMaleLQ = MaleLowerQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
             pctMaleLMQ = MaleLowerMiddleQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
             pctMaleUMQ = MaleUpperMiddleQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
             pctMaleTQ = MaleTopQuartile/(MaleLowerQuartile + MaleLowerMiddleQuartile + MaleUpperMiddleQuartile + MaleTopQuartile),
             pctFemaleLQ = FemaleLowerQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
             pctFemaleLMQ = FemaleLowerMiddleQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
             pctFemaleUMQ = FemaleUpperMiddleQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
             pctFemaleTQ = FemaleTopQuartile/(FemaleLowerQuartile + FemaleLowerMiddleQuartile + FemaleUpperMiddleQuartile + FemaleTopQuartile),
             SkewGenderLQ = pctMaleLQ-pctFemaleLQ,
             SkewGenderLMQ = pctMaleLMQ-pctFemaleLMQ,
             SkewGenderUMQ = pctMaleUMQ-pctFemaleUMQ,
             SkewGenderTQ = pctMaleTQ-pctFemaleTQ)
    
    
  # 5.2  addition and reduction of predictors
    predictors <- cleaned.reshaped %>% 
      select(!c(DiffMeanHourlyPercent))
    nearZeroVar(predictors, saveMetrics = TRUE)
    
    # :  Add Dummy variables for industry and run nearZero
    dummyModel <- dummyVars(~ Gender + section, data = cleaned.reshaped, levelsOnly = TRUE )
    dummies <- data.frame(predict(dummyModel, newdata = cleaned.reshaped))
    nearZeroVar(dummies, saveMetrics = TRUE) # to print
    k <- nearZeroVar(temp) # dummy cols w/ near-zero var to remove
    dummies <- dummies[-k] #remove
    
  
  # 4. corr plot
    # blended <- cbind(select(cleaned.reshaped,where(is.numeric)), dummies) %>% 
    #   drop_na()
    # correlations <-  cor(blended)
    # corrplot(correlations, order = 'hclust')
    # 
    
  
    
    # 4.5  transformation: predictor skewness 
    temp <- round(data.frame(apply(select(predictors, where(is.numeric)), 2, skewness)), digits = 2)
    c('Skew Values')-> names(temp)
    
    
    # 4.4. pca
   
    pcaObject <- prcomp(select(cleaned.reshaped, where(is.numeric)), center = TRUE, scale. = TRUE)
    pctVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
    
    
    # 4.5 ALL OF THE ABOVE BoxCox for skewness, Center and scale, 'pca' all together
    reshaped.transformed <-  cleaned.reshaped %>% 
      select(!c(DiffMeanHourlyPercent, EmployerName)) %>% 
      select(where(is.numeric))
    reshaped.transformed <- cbind(reshaped.transformed, dummies)
    
    trans <- preProcess(reshaped.transformed, method = c('BoxCox', 'center', 'scale', 'pca'))
    trans
    # apply the transformation
    reshaped.transformed <- predict(trans, reshaped.transformed)
    
    # cleaned.reshaped <- cleaned.reshaped %>% 
    #   mutate(across(c(MinEmployees, Gender, section), as.factor))
    
    # clean up and save to working directory
    rm(df, temp, tempNA, regx, list, dat_url, url, names, titles, dummies, dummyModel, pcaObject, predictors, trans, k , pctVariance)
    
    saveRDS(dat.cleaned, 'cleaned.rds')
    saveRDS(cleaned.reshaped, 'reshaped.rds')
    saveRDS(reshaped.transformed, 'transformed.rds')
  
    
  # NOT USED, but great idea :(
  #  harvest gender from nameberry.com website 
  # function to pull
  # gender_from_name <- function(name){
  #   name_url <- paste('https://nameberry.com/babyname/',  name, sep = '')
  #   gender <- read_html(name_url) %>% 
  #     html_nodes('span.meta-section span a') %>% 
  #     html_text(trim=TRUE) 
  #   return (gender)
  # }
  # 
  # names <- titles  %>%
  #   filter(CLevel==TRUE) %>% 
  #   select(name) %>% 
  #   unique()  
  # 
  # names$gender <- sapply(names$name, gender_from_name)

  


