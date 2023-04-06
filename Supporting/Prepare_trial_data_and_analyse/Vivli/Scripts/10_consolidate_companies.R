# 10_consolidate_companies


## Read in all companies data and clean-up ----
source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")
#library(stringdist)

cmpn_vars <- c("bi.Rds", "bi_new.Rds", "tak.Rds", "rch.Rds") # Lilly not added as added in script 91
cmpnies <- as.list(cmpn_vars)


## Where add new data want to check that existing rename spreadsheet covers the variable names
 bi_new_namecheck <- readRDS("Processed_data/bi_new_partial.Rds")
 bi_new_namecheck <- map(bi_new_namecheck, ~ .x %>% names())
 bi_new_namecheck <- stack(bi_new_namecheck)
 bi_new_namecheck <- as_tibble(bi_new_namecheck)
 names(bi_new_namecheck) <- c( "varname", "tblname")
 bi_new_namecheck$company <-  "bi_new"
cmpnies <- map(cmpnies, ~ readRDS(paste0("Processed_data/", .x)))
names(cmpnies) <- str_replace(cmpn_vars,
                              fixed(".Rds"), "")


# check which names are common to all
# Check all table names the same, they are 
tbl_names <- map(cmpnies, names) 
do.call(rbind, tbl_names)
cmpnies <- transpose(cmpnies)

## rename variables
all_vars <- map(cmpnies, function(table_name){
  map(table_name, names) %>% stack()
})
all_vars <- bind_rows(all_vars, .id = "tblname")

names(all_vars) <- c("tblname", "varname", "company")
tbl_order <- c("conmed" = 5, "demo" = 4, "labs" = 7, "rand" = 1, "bp" = 2, "bmi" = 3, "medhist" = 6)

all_vars <- all_vars %>% 
  mutate(tbl_order = tbl_order[tblname]) %>% 
  arrange(tbl_order, varname) %>% 
  distinct(varname, .keep_all = TRUE)

write_csv(all_vars, file = "Scratch_data/review_varnames.csv")


## read in rename
# remove where dont want the variable (study)
all_vars_rename <- read_csv("Created_metadata/reviewED_varnames.csv") %>% 
  select(varname, new_varname) %>% 
  na.omit()

names_lkp <- all_vars_rename$new_varname
names(names_lkp) <- all_vars_rename$varname


## Apply rename to tables
cmpnies2 <- map(cmpnies, function(table_type){
  map(table_type, function (each_table){
    # select only variables that are in rename list
    each_table <- each_table[names(each_table) %in% names(names_lkp)]
    # next rename variables
    names(each_table) <- names_lkp[names(each_table)]
    # next convert everything to a character
    # each_table <- each_table %>% mutate_all(as.character)
    each_table
  })
})

## COnvert to character
cmpnies2 <- map(cmpnies2, function(table_type){
  map(table_type, function (each_table){
    # next convert everything to a character
    each_table %>% mutate_all(as.character)
  })
})

cmpnies2 <- map(cmpnies2, function(table_type) bind_rows(table_type, .id = "company"))
cmpnies <- cmpnies2
rm(cmpnies2)
saveRDS(cmpnies, "Processed_data/all_sponsors_transposed.Rds")

