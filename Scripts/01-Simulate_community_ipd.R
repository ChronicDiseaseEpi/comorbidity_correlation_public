#' ---
#' title: "01: Simulate community IPD"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

#' This script loads in the community aggregate data and simulates individual-level
#' patient data (IPD) for each index condition by sex.
#' 
#' Because of the way the data was exported, the aggregate level data is in three parts:
#' the 1st file "SAIL_aggregate_data.html" contains the counts for each comorbididty 
#' strata and the 2nd file "SAIL_no_comorbidities_strata" has the counts for the number
#' of individuals for each index condition with no comorbidities. The 3rd file
#' is for dementia only. At a later date we discovered schizophrenia had been wrongly 
#' identified hence we removed schizophrenia as a comorbidity and re-simulated the IPD
#' for dementia with 5 of the original comorbidities and a replacement for schizophrenia.
#' 



# Load in packages and functions
source("Scripts/00-Functions_and_packages.R")



# Read in Data and fix names ---------------------------------------------------

# Read in html of SAIL aggregate data  
aggr_data <- readHTMLTable("Data/SAIL_aggregate_data.html", header = T, trim = T)

# Name list items using first two columns
aggr_data <- map(aggr_data, ~ set_names(.x, str_sub(names(.x), 3, -3)))

# Remove new line and carriage return from df column names 
aggr_data <- map(aggr_data, ~  .x %>% 
                   rename_all(~ str_replace_all(.x, c("\\\n" = "", "\\\r" = "")))) 

# Set names as index condition and sex
cond <- map_chr(aggr_data, ~ paste0(.x$index_condition, "_", .x$sex) %>% unique())

# Remove leading spaces and text within brackets
cond <- gsub("\\s*\\([^\\)]+\\)","", cond) 

# Replace comas and spaces with dash, then remove double dashes
cond <- cond %>% str_replace_all(pattern = c(" |,"), replacement = c("_")) %>%
  str_replace_all("__", "_")

names(aggr_data) <- cond

# Remove Migraine as no longer part of community - trial comparison (7/11/22)
aggr_data <- aggr_data[-c(15, 16)]
names(aggr_data)

# Remove conditions we don't have in trials
aggr_data <- aggr_data[-c(3, 4, 10, 13, 14, 27, 28)]
names(aggr_data) # 21



# Examine how many strata have fewer than 50 individuals
aggr_data2 <- map(aggr_data, ~ .x %>% 
                    mutate(n = if_else(n == "<=5", 3, as.double(n)),
                           n2 = n) %>% 
                    group_by(n <=50) %>% 
                    summarise(n2 = sum(n2)) %>% 
                    ungroup())
map_dbl(aggr_data2, ~ .x$n2[2] / sum(.x$n2)) %>% 
  sort()

## Plot variation to variation; note for the mean and the standard deviation there
## are equations for the standard errors - https://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
# simga_mu = s/n^0.5
# simga_s^2 = s^2 * (2/(n-1))^0.5
# However, can also plot by sampling
MakePlot <- function(n_stratum = 1000){
  b <- map(1:10, ~ tibble(smpl = rep(.x, n_stratum), res = rnorm(n = n_stratum)))
  b <- bind_rows(b) 
  b_plot <- ggplot(b, aes(x = res, group = smpl)) +
    geom_density()
  b_plot
}
xmn <- map(c(10, 50, 100, 1000, 5000), MakePlot)
xmn <- map2(xmn, c(10, 50, 100, 1000, 5000),
            ~ .x + ggtitle(paste0("N in stratum = ", .y)))
cowplot::plot_grid(plotlist = xmn)


# Confirm that selected commonest comorbidites for both sexes, not separate ones.
aggr_data_chk <- tibble(index_sex = names(aggr_data), comos = map_chr(aggr_data, ~ names(.x) %>% sort() %>% paste(collapse = ", ")))
aggr_data_chk <- aggr_data_chk %>% 
  separate(index_sex, into = c("index", "sex"), sep = "_") %>% 
  spread(sex, comos)

# All same in men and women
aggr_data_chk %>% filter(is.na(Female) | is.na(Male) | Female != Male)

# Reduces to eight distinct combinations of comorbidites
aggr_data_chk %>% group_by(Male) %>% summarise(n = length(Male), conds = paste(index, collapse = ", ")) %>% ungroup()



# Add strata of patients with no comorbidities ---------------------------------

# Read in strata for individuals with zero comorbidities
no_como <- read_csv("Data/SAIL_no_comorbidities_strata.csv")
new_aggr_data <- aggr_data


for (i in seq_along(aggr_data)) {
  
  # Get single IPD table 
  ipd <- aggr_data[[i]]
  
  # Get condition and sex from IPD
  cond <- unique(ipd$index_condition)
  sexx <- unique(ipd$sex)
  
  # Filter no como df to match the indexed table from aggr data
  no <- no_como %>% filter(index_condition %in% cond & sex %in% sexx)
  
  # Match class types for row binding 
  no$age_m <- as.character(no$age_m)
  no$age_s <- as.character(no$age_s)
  no$n <- as.character(no$n)
  
  # Comorbidity column names 
  old_como <- names(no)[2:7]
  new_como <- names(ipd)[3:8]
  
  # Rename comorbidity columns
  no <- no %>% 
    rename_with(~new_como[which(all_of(old_como) ==.x)], .cols = old_como)
  
  # Bind rows 
  ipd <- ipd %>% bind_rows(no)
  
  # Add back into aggr data 
  new_aggr_data[[i]] <- ipd
  
}

# Check
n_aggr     <- map(aggr_data, ~ nrow(.x)) %>% unlist
n_new_aggr <- map(new_aggr_data, ~ nrow(.x)) %>% unlist 
xmn        <- data.frame(old = n_aggr, new = n_new_aggr)
xmn        <- xmn %>% mutate(diff = new-old) 

# Save list of aggregated dfs as rds
saveRDS(new_aggr_data, "Data/community_aggregate_data.rds")
rm(aggr_data, new_aggr_data)



# Simulate IPD -----------------------------------------------------------------

# Simulate IPD for all conditions by sex
set.seed(1)
aggr_data <- readRDS("Data/community_aggregate_data.rds")

# Restrict to conditions we have in trials + remove dementia (simulated separately)
cond              <- names(aggr_data)[-c(3, 4, 6, 7, 10, 13, 14, 27, 28)]
all_ipd_df        <- lapply(cond, simulate_IPD, mydata = aggr_data)
names(all_ipd_df) <- cond

# Note: There are NAs in some of the simulated data sets
sum(aggr_data$Dementia_Male$n == "<=5")
sum(aggr_data$Dementia_Female$n == "<=5")

# NAs do NOT correspond with the number of <=5



# Do the above for dementia on its own -----------------------------------------

# Dementia was re-simulated without Schizophrenia as it had been wrongly identified

# Read in aggregated dementia data (09/11/22 - new data with schizophrenia removed)
new_dem <- read_csv("Data/dementia_no_schz_rand_sd.csv")

# Simulate random number of patients for combinations where n < 5
new_dem <- new_dem %>% 
  mutate(n = ifelse(n == "<=5", sample(1:5, sum(new_dem$n == "<=5"), replace = TRUE), n))
  
# Correct class types
new_dem$age_m <- as.numeric(new_dem$age_m)
new_dem$age_s <- as.numeric(new_dem$age_s)
new_dem$n     <- as.integer(new_dem$n)

# Set up IPD df 
new_dem_sim   <- new_dem[rep(1:length(new_dem$n), times = new_dem$n),]
new_dem_sim$n <- NULL

# Generate IPD - Normal distribution truncated at 0 to prevent -ve ages
new_dem_sim$age_sim <- rtruncnorm(nrow(new_dem_sim), a = 0, b = Inf, mean = new_dem_sim$age_m, sd = new_dem_sim$age_s)

# Check correct length 
sum(new_dem$n)    # 13887
nrow(new_dem_sim) # 13887

# Save simulated ipd by sex 
cond <- unique(new_dem_sim$index_condition)
cond <- cond %>% 
  str_replace_all(" ", "_") %>%
  str_remove_all("[()]")

write_csv(new_dem_sim %>% filter(sex %in% "Female"), paste0("Data/", cond, "_Female", "_ipd.csv"))
write_csv(new_dem_sim %>% filter(sex %in% "Male"), paste0("Data/", cond, "_Male", "_ipd.csv"))
