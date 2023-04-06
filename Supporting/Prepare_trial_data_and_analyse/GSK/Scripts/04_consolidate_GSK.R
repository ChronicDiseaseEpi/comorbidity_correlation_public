#04_consolidate_GSK 

# This is the same script as 09_consolidate_companies but edited for GSK only  

## Read in all companies data and clean-up ----
source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

cmpnies_orig <- readRDS("GSK_processed_data/gsk.Rds")
cmpnies_new  <- readRDS("GSK_processed_data/gsk_new.Rds")

## change names for rand and lab in old trials
# cmpnies_orig$rand <- cmpnies_orig$rand %>% 
#   rename(tpatt = trt)

# cmpnies_orig$labs <- cmpnies_orig$labs %>% 
#   rename(ulc = lbstnrhi,
#          llc = lbstnrlo ,
#          labstd  = lbstresn,
#          labnmx = lbtest,
#          labnm = lbtestcd,
#          labstdu = lbstunit)

cmpnies <- list(gsk = cmpnies_orig,
                gsk_new = cmpnies_new)
rm(cmpnies_new, cmpnies_orig)

# check which names are common to all
# Check all table names the same, they are 
tbl_names <- map(cmpnies, names) 
do.call(rbind, tbl_names)
cmpnies <- transpose(cmpnies)

## copnsolidate across old and new
# cmpnies <- map(cmpnies, ~ bind_rows(orig = .x$gsk, new = .x$gsk_new, .id = "gsk_which"))
# map(cmpnies, ~ .x %>% group_by(gsk_which, trial) %>% slice(1) %>% ungroup() %>% as.data.frame())

## rename variables, loops acrss gsk and gsk new and within these across each tables
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

write_csv(all_vars, "GSK_scratch_data/review_varnames.csv")


## read in rename
# remove where dont want the variable (study)
## Renames multiple column names to a single column name
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
saveRDS(cmpnies, "GSK_processed_data/GSK_transposed.Rds")







## CHECK 
gsk <- readRDS("GSK_processed_data/GSK_transposed.Rds")

# Check how many trials, should = 31
n_distinct(gsk$conmed$trial)  # 29
n_distinct(gsk$demo$trial)    # 31
n_distinct(gsk$labs$trial)    # 25
n_distinct(gsk$medhist$trial) # 22 -> now 23 (25/08/22)
n_distinct(gsk$bp$trial)      # 28
n_distinct(gsk$bmi$trial)     # 29
n_distinct(gsk$rand$trial)    # 31

sum(is.na(gsk$demo$age)) # 0 (25/08/22)

map_int(gsk, ~ sum(.x$trial %>% unique() %>% length()))

# conmed    demo    labs medhist      bp     bmi    rand 
# 29      31      25      23      28      29      31 
