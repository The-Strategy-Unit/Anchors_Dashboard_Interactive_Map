library(readxl)
library(tidyverse)
library(fingertipsR)

#Reference files----
##LA to ICB----
DfLAICB <- read_excel("../Reference/x-integrated-care-board-boundary-changes-22-23.xlsx",
                      sheet= 'LSOA mapper',
                      skip = 3) %>%
  filter(str_detect(LSOA11CD,'^E'))

##LSOA populations----
DfPop <- read_excel('../Reference/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx',
                    sheet= 'Mid-2020 Persons',
                    skip = 4) %>%
  filter(str_detect(`LSOA Code`,'^E')) %>% 
  mutate(`Not applicable` = 'Not applicable')

##Bring together & pivot----
DfLKUP <- DfLAICB %>%
  left_join(DfPop, by = c('LSOA11CD' = 'LSOA Code')) %>%
  group_by(UTLA21CD, ICB22ons, ICB22NM) %>%
  summarise(Age5         = sum(case_when(is.na(Age5)         ~ 1, TRUE ~ Age5)),
            AgeUnder16   = sum(case_when(is.na(AgeUnder16)   ~ 1, TRUE ~ AgeUnder16)),
            Age15to16    = sum(case_when(is.na(Age15to16)    ~ 1, TRUE ~ Age15to16)),
            Age16to17    = sum(case_when(is.na(Age16to17)    ~ 1, TRUE ~ Age16to17)),
            Age16AndOver = sum(case_when(is.na(Age16AndOver) ~ 1, TRUE ~ Age16AndOver)),   
            Age16to64    = sum(case_when(is.na(Age16to64)    ~ 1, TRUE ~ Age16to64)),
            AgeAll       = sum(case_when(is.na(`All Ages`)   ~ 1, TRUE ~ `All Ages`))) %>%
  group_by(UTLA21CD) %>%
  mutate(Age5All         = sum(case_when(is.na(Age5)         ~ 1, TRUE ~ Age5)),
         AgeUnder16All   = sum(case_when(is.na(AgeUnder16)   ~ 1, TRUE ~ AgeUnder16)),
         Age15to16All    = sum(case_when(is.na(Age15to16)    ~ 1, TRUE ~ Age15to16)),
         Age16to17All    = sum(case_when(is.na(Age16to17)    ~ 1, TRUE ~ Age16to17)),
         Age16AndOverAll = sum(case_when(is.na(Age16AndOver) ~ 1, TRUE ~ Age16AndOver)),   
         Age16to64All    = sum(case_when(is.na(Age16to64)    ~ 1, TRUE ~ Age16to64)),
         AgeAllAll       = sum(case_when(is.na(AgeAll)       ~ 1, TRUE ~ AgeAll))) %>%
  mutate(PerAge5All         = Age5/Age5All,
         PerAgeUnder16All   = AgeUnder16/AgeUnder16All,
         PerAge15to16All    = Age15to16/Age15to16All,
         PerAge16to17All    = Age16to17/Age16to17All,  
         PerAge16AndOverAll = Age16AndOver/Age16AndOverAll,
         PerAge16to64All    = Age16to64/Age16to64All,
         PerAgeAllAll       = AgeAll/AgeAllAll)

lookup <- cbind(
  DfLKUP %>% 
    pivot_longer(-c(1,2,3), names_to = 'ageGroup', values_to = 'pop') %>% 
    filter(ageGroup %in% c('Age5All',
                           'AgeUnder16All',
                           'Age15to16All',
                           'Age16to17All',
                           'Age16AndOverAll',
                           'Age16to64All',
                           'AgeAllAll')),
  
  DfLKUP %>% 
    pivot_longer(-c(1,2,3), names_to = 'ageGroup', values_to = 'proportion') %>% 
    filter(str_detect(ageGroup, 'Per'))) %>% 
  select(UTLA21CD...1, ICB22ons...2, ICB22NM...3, ageGroup...9, pop, proportion) %>% 
  rename(UTLA21CD = UTLA21CD...1, ICB22ons = ICB22ons...2, ICB22NM = ICB22NM...3 ,ageGroup = ageGroup...9)

##Reference info----
additional_info <- read_xlsx('JW_2022_05_03 - Indicator Info.xlsx')%>% 
  mutate(scale_order = case_when(Order == 'High = good' ~ 'asc',
                                 Order =='Low = good' ~ 'desc')) %>% 
  rename('description' = `What is this`) %>% 
  select(-`My Ref`) %>% 
  drop_na()

#Fingertips data----
##Create reference table----
indicators_base <- tibble(sheet = excel_sheets('Anchors impact framework.xlsx')) %>% 
  filter(!sheet %in% c('Dashboard', 'Contents', 'List', 'Living wage', 'Fuel poverty','Work illness','Educational attainment'
  )) %>% 
  mutate(id = '-',
         time = '-')

for(i in 1:nrow(indicators_base)){
  df <- read_xlsx('Anchors impact framework.xlsx', sheet = indicators_base$sheet[i])
  indicatorID <- df$`Indicator ID`[1]
  timePeriod <- max(df$`Time period`, na.rm = T)
  timePeriodSortable <- max(df$`Time period Sortable`, na.rm = T)
  indicators_base$id[i] <- indicatorID
  indicators_base$time[i] <- timePeriod
  indicators_base$timesort[i] <- timePeriodSortable
  indicators_base$age[i] <- unique(df$Age)
  indicators_base$data[[i]] <- df 
}

#manual update for most recent info
indicators_base$time <- c('2018/19', 
                          '2018/19',
                          '2020/21',
                          #''
                          '2014/15',
                          '2021',
                          '2021/22',
                          '2020',
                          '2021/22',
                          '2021',
                          '2021',
                          '2019',
                          'Mar 2015 - Feb 2016',
                          '2020')

#manual update to get ages in format we need them
indicators_base$ageGroup <- c('PerAge5All', #8 different categories in total
                              'PerAge5All',
                              'PerAgeUnder16All',
                              #'PerAge15to16All',
                              'PerAge15to16All',
                              'PerAge16to17All',
                              'PerAge16AndOverAll',
                              'PerAge16to64All',
                              'PerAge16to64All',
                              'PerAge16to64All',
                              'PerAge16AndOverAll',
                              'PerAgeAllAll',
                              'PerAge16AndOverAll',
                              'PerAgeAllAll')

##Populate it from fingertips----
for(i in 1:nrow(indicators_base)){
  indicators_base$data[[i]] <- fingertips_data(IndicatorID = indicators_base$id[i], AreaTypeID = 'All')
}

indicators_base <- indicators_base %>% unnest()

##Counties----
###Filter for relevant data----
indicators_data_Counties <- indicators_base %>% filter(AreaType =='Counties & UAs (from Apr 2021)' & 
                                           id %in% c('91133',
                                                     '92899',
                                                     '93351',
                                                     '93701',
                                                     '93203',
                                                     '22301')
                                         |AreaType =='Counties & UAs (2020/21)' &
                                           id %in% c('90631',
                                                     '90632')
                                         # |AreaType =='Counties & UAs (2019/20)' &
                                         #   id %in% c('22301')
                                         |AreaType =='Counties & UAs (pre Apr 2019)' &
                                           id %in% c('92924',
                                                     '91136',
                                                     '11601'),
                                         time == Timeperiod,
                                         Sex %in% c('Persons', 'Not applicable'),
                                         !str_sub(AreaCode, 1,3) %in% c('E12', 'E92'))

##Districts----
###Filter for relevant data----
indicators_data_Districts <- indicators_base %>% filter(AreaType =='Districts & UAs (2020/21)' &
                                                         id %in% c('92199')
                                                       |AreaType =='Districts & UAs (from Apr 2021)' &
                                                         id %in% c('91126',
                                                                   '93268'),
                                                       time == Timeperiod,
                                                       Sex %in% c('Persons', 'Not applicable'),
                                                       !str_sub(AreaCode, 1,3) %in% c('E12', 'E92'))

###Translate E07s----
indicators_data_Districts <- indicators_data_Districts %>%
  left_join(DfLAICB %>% filter(substr(LAD21CD,1,3) == 'E07') %>%
              group_by(LAD21CD, UTLA21CD) %>%
              summarise(FID = sum(FID)),
            by = c('AreaCode' = 'LAD21CD')) %>%
  mutate(AreaCodeDistricts = case_when(substr(AreaCode,1,3) == 'E07' ~ UTLA21CD,
                                       TRUE ~ AreaCode))

###Group by the new AreaCodeDistricts----
#before we group lets calculate any missing values...
indicators_data_Districts <- indicators_data_Districts %>% 
  mutate(Denominator = case_when(is.na(Denominator) ~ Count*100/Value,
                                 TRUE ~ Denominator)) %>% 
  group_by(sheet,
           id,
           ageGroup,
           AreaCodeDistricts) %>% 
  summarise(Value = sum(Value),
            Count = sum(Count),
            Denominator = sum(Denominator)) %>% 
  rename(AreaCode = AreaCodeDistricts) %>% 
  ungroup()

##Bring Counties & Districts back together. Add on pop figures and ICB then calculate missing values and filter out those still unobtainable----
indicators_data_Together <- rbind(indicators_data_Counties %>% 
                                    select(sheet, id, AreaCode, ageGroup, Value, Count, Denominator),
                                  indicators_data_Districts %>% 
                                    select(sheet, id, AreaCode, ageGroup, Value, Count, Denominator)) %>% 
  left_join(lookup,
            by = c('AreaCode' = 'UTLA21CD', 
                  'ageGroup' = 'ageGroup')) %>% 
  left_join(additional_info %>% select(Ref, val_type), by = c(id = 'Ref')) %>%
  mutate(ValueAdj = ifelse(val_type %in% c('percentage'), Value/100, Value),
         Denominator2 = ifelse(is.na(Denominator), pop, Denominator),
         Count2 = ifelse(is.na(Count) & !is.na(Denominator2) & !is.na(Value), ValueAdj*Denominator2, Count),
         Value2 = ifelse(is.na(ValueAdj) & (!is.na(Denominator2) | !is.na(Count2)), Count2/Denominator2, ValueAdj),
         #keeping, just because took a while and for checking purposes
           # Value_na = case_when (is.na(Value) ~ 1, TRUE ~ 0),
           # Denominator_na = case_when (is.na(Denominator) ~ 1, TRUE ~ 0),
           # Count_na = case_when (is.na(Count) ~ 1, TRUE ~ 0),
           # ValueAdj_na = case_when (is.na(ValueAdj) ~ 1, TRUE ~ 0),
           # Denominator2_na = case_when (is.na(Denominator2) ~ 1, TRUE ~ 0),
           # Count2_na = case_when (is.na(Count2) ~ 1, TRUE ~ 0),
           # Value2_na = case_when (is.na(Value2) ~ 1, TRUE ~ 0)
         ) %>% 
         #filter(is.na(Denominator2) | is.na(Count2)) %>% #this is kept in for testing different scenario
         filter(!is.na(Denominator2) & !is.na(Count2)) %>% 
  mutate(Count3 = Count2*proportion,
         Denominator3 = Denominator2*proportion) %>% 
  group_by(id, sheet, ICB22ons, ICB22NM,val_type) %>% 
  summarise(Count3 = sum(Count3),
            Denominator3 = sum(Denominator3)) %>% 
  mutate(value = case_when(val_type %in% c('percentage') ~ Count3/Denominator3*100, 
                           id == 91133 ~ Count3/Denominator3*1000,
                           TRUE ~ Count3/Denominator3))
            
##Add in fuel poverty----
fuel_poverty <- readRDS('fuel_poverty.rds') %>% 
  rename('value' = percentage) %>% 
  mutate(sheet = 'fuel poverty',
         id = 'fuel poverty')

##Add in below living wage----
below_living_wage <- readRDS('below_living_wage.rds') %>% 
  rename('value' = percent) %>% 
  mutate(sheet = 'below living wage',
         id = 'below living wage')

#Create final output map data----
map_data <- bind_rows(indicators_data_Together,
                      fuel_poverty,
                      below_living_wage) %>% 
  select(-numerator, -denominator) %>% 
  left_join(additional_info %>% select(-val_type), by = c(id = 'Ref')) %>% 
  mutate(`Indicator Name` = factor(`Indicator Name`)) %>% 
  mutate(`Indicator Name` = fct_relevel(`Indicator Name`, 
                                        c('Early years development',
                                          'Early years development - free school meals',
                                          'Children in low income households',
                                          #'Educational attainment',
                                          'Educational attainment - free school meals',
                                          'Young people not in education/employment/training',
                                          'People self-reporting low wellbeing',
                                          'Unemployment',
                                          'Economic inactivity',
                                          'Jobseeker long-term claimants',
                                          'Average weekly earnings',
                                          #'Work-related illness',
                                          'Income deprivation',
                                          'Jobs below living wage',
                                          'Living in fuel poverty',
                                          'Utilisation of outdoor space',
                                          'Air quality'))) %>% 
  arrange(`Indicator Name`)

saveRDS(map_data,'map_data.rds')
