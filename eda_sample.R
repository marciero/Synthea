### Data manipulations and EDA with Synthea SyntheticMass sample data

library(tidyverse)
library(lubridate)
library(broom)
library(broom.mixed)
library(janitor)


patients <- read_csv("sample_data/patients.csv")
encounters <- read_csv("sample_data/encounters.csv")
conditions <- read_csv("sample_data/conditions.csv")
observations <- read_csv("sample_data/observations.csv")
providers <- read_csv("sample_data/providers.csv")

#

n_distinct(observations$PATIENT)

library(skimr)
skim(patients)
class(patients$BIRTHDATE)

years(floor((today() - ymd(20110101))/365.25))  ## check this does what we think

ages <- patients %>% select(Id, BIRTHDATE) %>%
    transmute(Id, dob = BIRTHDATE) %>%
    mutate(age = year(years(floor((today() - dob)/365.25))))

ages %>% ggplot(aes(age)) +
    geom_histogram(color = "white")

## Form age column in patients

patients_age <- patients %>%
    select(Id, BIRTHDATE, DEATHDATE, GENDER, ZIP, HEALTHCARE_EXPENSES, HEALTHCARE_COVERAGE) %>%
    mutate(age = year(years(floor((today() - BIRTHDATE)/365.25))))

## Filter patients with no stop date conditions (ultimately will map to chronic conditions)

conditions %>% filter(is.na(STOP)) %>% skim()
conditions %>% filter(is.na(STOP)) %>% select(PATIENT) %>%  n_distinct()

conditions_chronic <- conditions %>% filter(is.na(STOP)) %>% group_nest(PATIENT)

## Want to count encounters  of each type for each patient
encounters_table <- encounters %>%
    group_by(PATIENT, ENCOUNTERCLASS)  %>%
    summarize(count = n()) %>% pivot_wider(names_from = ENCOUNTERCLASS, values_from = count) %>%
    replace(is.na(.),0)

####
####  Now merge encounters_table, patient_nest, conditions_chronic,

df <- conditions_chronic %>%
    inner_join(patients_age, by = c("PATIENT" = "Id")) %>%
    inner_join(encounters_table)

## Okay as first pass but want to filter encounters for a give period of time; say one or two years.
## As first pass just take same period for all patients.
## Also filter out deceased
##

encounters_table <- encounters %>% filter(year(START) > "2018" & year(START) < "2020") %>%
    group_by(PATIENT, ENCOUNTERCLASS)  %>%
    summarize(count = n()) %>% pivot_wider(names_from = ENCOUNTERCLASS, values_from = count) %>%
    replace(is.na(.),0)

patients_age <- patients %>% filter(is.na(DEATHDATE)) %>%
    select(Id, BIRTHDATE, DEATHDATE, GENDER, ZIP, HEALTHCARE_EXPENSES, HEALTHCARE_COVERAGE) %>%
    mutate(age = year(years(floor((today() - BIRTHDATE)/365.25))))

df <- conditions_chronic %>%
    inner_join(patients_age, by = c("PATIENT" = "Id")) %>%
    inner_join(encounters_table, by = "PATIENT" )

### Look at the SNOMED to ICD 10 .tsv file from https://www.nlm.nih.gov/healthit/snomedct/us_edition.html

snmed <- read_tsv("SNOMED_ICD/snmd-icd.tsv")

## See if the  code in conditions table matches with the code in snmed
## Do a join and see if the names match

conditions_check <- conditions %>% select(CODE, DESCRIPTION)

snmed_ICD <- snmed %>% mutate(CODE = referencedComponentId) %>%
    select(CODE, referencedComponentId, referencedComponentName, mapTarget, mapTargetName)

## with a few exceptions things seem to match up

conditions_ICD <- conditions %>%
     left_join(snmed_ICD,  by = "CODE")



