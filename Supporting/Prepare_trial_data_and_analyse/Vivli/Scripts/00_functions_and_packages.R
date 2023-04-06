# 00_functions_and_packages



# Packages ---------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(haven)
library(readxl)
library(stringdist)
library(rstan)



# Functions --------------------------------------------------------------------

ExtractLabel <- function(x, return_object = FALSE, ncharprint = 40) {
  lbls <- map(x, ~ attr(.x, "label"))
  names(lbls) <- names(x)
  lbls <- unlist(lbls)
  # Following code adds in name if no label
  y <- names(x)
  names(y) <- y
  y[names(lbls)] <- lbls
  lbls <- y
  # lbls[is.null(lbls)] <- ""
  if(return_object) lbls else{
    lbls[] <-   str_sub(lbls, 1, pmin(ncharprint, str_length(lbls)))
    print(as.data.frame(lbls))
  }
}


names_to_lower <- function(df) {
  names(df) <- tolower(names(df))
  df
}


FirstTenValues <- function(x) {
  x <- unique(x)[1:10]
  x [!is.na(x)]
}


FirstTenLabelled <- function (x){
  a <- ExtractLabel(x, return_object = TRUE)
  names(x) <- paste(names(a), a, sep = " - ")
  map(x, FirstTenValues)
}


CreateVersionSnapshot <- function(commitmessage = "No message with this commit"){
  ## This function takes the contents of scripts and saves toa  folder with a datetime stamp
  ## It optionally takes a commit message describing the snapshot. IN any case it write it to a log file
  ## in the scripts_versions folder
  date_time <- Sys.time()
  date_time <- str_replace_all(date_time, "-|:|\\s", "")
  to_folder <- paste0("Scripts_versions/snapshot", date_time, "/")
  dir.create(to_folder)
  file.copy(from = list.files("Scripts", full.names = TRUE), to = to_folder)
  write_lines(x = paste0(date_time, "|", commitmessage), file = "Scripts_versions/log.txt", 
              append = TRUE)
}


# Create function to take common words and separate words in label
MakeStringSimilarDiffs <- function (x){
  # Create list with vector where each word is an element
  x1 <- str_split(x, pattern = "\\b")
  # Identify words common to all fields
  common_words <- reduce(x1, intersect)
  # Identify words unique to fields
  diff_words <- map(x1, ~ setdiff(.x, common_words))
  diff_words <- map_chr(diff_words, paste, collapse = "")
  diff_words <- diff_words[diff_words != ""] %>%  str_trim()
  paste(c(x[1], diff_words), collapse = "|")
}
# MakeStringSimilarDiffs(c("Haemoglobin (g/Dl)", "Haemoglobin (mmol/L)", "Haemoglobin (invalid one)"))


# Extract labels from each table
CreateTableNameLabels <- function(table_list){
  # Argument is a list of tables of the same kind
  a <- map(table_list, ExtractLabel, return_object = TRUE)
  a <- map(a, ~ tibble(label = .x, varname = names(.x)))
  a <- bind_rows(a, .id = "trial")
  a %>% 
    spread(key = varname, value = label)
}


RenameQuick <- function(x, original, new) {
  names(x)[names(x) == original] <- new
  x
}


# Choose a vector of labels to apply, take all labels separating with a pipe
# If label is null, return variable name
SelectLabelRow <- function(table_labels, chosen_row = 2){
  all_lbls <- map_chr(table_labels, function(x) {
    x <- na.omit(x)
    if(length(x) == 0) return ("")
    x <- x %>% unique() %>% sort() %>% paste(collapse = "|")
    # x <- MakeStringSimilarDiffs(x)
    x
  })
  a <- all_lbls
  a[1] <- "Trial ID from extaction script"
  a[a == ""] <- names(a)[a ==""]
  a
}


# Apply labels to chosen vector
MakeLabels <- function(mydf, labelvector){
  # Order dataframe so that is the same as label vector so that matches
  mydf <- mydf[, names(labelvector)]
  # Applies labels to  each column
  mydf[] <- map2(mydf, labelvector, function(x, y) {
    attr(x, "label") <- y
    x
  })
  mydf
}


# Wrap CreateTableNameLabels, SelectLabelRow, bind rows and MakeLabels into a single convenience function
BindRowsWLabels <- function(mydf, chosen_row = 1) {
  labelvector <- SelectLabelRow(CreateTableNameLabels(mydf), chosen_row)
  mydf <- bind_rows(mydf, .id = "trial")
  MakeLabels(mydf, labelvector)
}


# COunt overservations per trial
CountPerTrial <- function (x) tapply(x$trial, x$trial, length)

# Convert agebands to age
BreakBand <- function(y) {
  break_point <- str_locate(y, fixed("-"))[, "start"]
  first_cut  <- as.integer(str_sub(y, 1, break_point-1))
  second_cut <- as.integer(str_sub(y, break_point + 1))
  mean_age <- first_cut + (second_cut-first_cut)/2
  as.integer(mean_age)
}

ConvertAge <- function(x) {
  case_when(
    is.na(x) ~ NA_integer_,
    str_detect(x, fixed("-")) & !is.na(x) ~ BreakBand(x),
    str_detect(x, "<") & !is.na(x) ~ str_extract(x, "[0-9]{1,}") %>% as.integer() - 1L,
    str_detect(x, ">") & !is.na(x) ~ str_extract(x, "[0-9]{1,}") %>% as.integer() + 1L,
    TRUE ~ 9999999L
  )
}


# Helper function to find file in E drive 
FindFx <- function(ptrn, onlyzip = TRUE){
## prints list of files and 
  a <- list.files("E:/",
                  pattern = ptrn,
                  recursive = TRUE,
                  full.names = TRUE)
  
  if(onlyzip == TRUE) a <- a[str_detect(a, "zip$")]
  a
}


# locate positions of strings within dataframe and return as dataframe of different length----
str_locate_df <- function(mydf, string = "(C|c)reatinine"){
  # mydf$pos <- seq_along(mydf$text)
  ## returns a dataframe with ALL the positions for a given string
  testnames <- all(c("text", "pos") %in% names(mydf) )
  if(!testnames) stop("dataframe must conain text variable")
  if(any(duplicated(mydf$pos))) stop("Position must be unique for each row")
  a <- str_locate_all(mydf$text, string) 
  names(a) <- seq_along(a)
  hits <- !map_lgl(a, ~ nrow(.x) ==0L)
  if(all(!hits)) stop("No hits for this string")
  print(paste0("There are ", sum(hits), " hits for this string"))
  a <- a[hits]
  a <- map(a, as.tibble)
  a <- bind_rows(a, .id = "pos")
  mydf %>% 
    inner_join(a %>% mutate(pos = as.integer(pos)))
}


CombineTextFinds <- function(res = positions_text, proto_dict = proto_dict, char_before = 200, char_after = char_before){
  # Takes dataframe of text positions and retrieves text, 
  # concatenating any adjacent text
  ## Take new start and end position
  res2 <- res %>% 
    mutate(new_start = start -char_before,
           new_end = end + char_after,
           res = str_sub(text, new_start, new_end)) %>% 
    select(-text) %>% 
    distinct()
  
  ## Detect where text overlaps
  res3 <- res2 %>%
    group_by(document, page) %>% 
    mutate(start_diff = lag(new_end) - new_start) %>% 
    ungroup() %>% 
    mutate(should_collapse = if_else(!is.na(start_diff) | !is.na(lead(start_diff)), TRUE, FALSE),
           res2 = if_else(!is.na(start_diff), str_sub(res, start_diff + 2), res)) %>% 
    ungroup()
  
  ## Collapse overlapping text
  res4_col <- res3 %>% 
    filter(should_collapse) %>% 
    group_by(pos, page) %>% 
    summarise(res2 = paste(res2, collapse = "_")) %>% 
    ungroup()
  
  ## Identify non-overlapping text
  res4_not <- res3 %>% 
    filter(!should_collapse) %>% 
    select(pos, page, res2) %>% 
    distinct()
  
  ## Combine all to a single dataframe
  res4both <- bind_rows(res4_col, res4_not)
  
  ## Add document name and type back in
  res5 <- proto_dict %>% 
    select(-text) %>% 
    distinct() %>% 
    inner_join(res4both)
  res5
}


## Function to make density plots or histogranms, if don't specificy the bindwidth it makes density plots ----
MakeDensity <- function(x = cmpnies$bp, my_param = "dbp", binwidth = NULL, tolog = FALSE){
  x <- x %>% 
    filter(param == my_param, !is.na(value))
  
  if(tolog ==TRUE) x$value <- log(x$value)
  
  density_x <- tapply(x$value, x$trial, density, cut = 0, simplify = FALSE)
  # tapply(demo2$age, demo2$trial_sex, sd, na.rm = TRUE)
  density_x <- map(density_x, ~ .x[c("x", "y")] %>% as.data.frame())
  density_x <- bind_rows(density_x, .id = "trial")
  
  
  pdf(paste0("Outputs/plot_", my_param, "_logged"[tolog], "_distribution.pdf"))
  for(i in unique(density_x$trial)){
    print(i)
    if(is.null(binwidth)){
      plot_x <- ggplot(density_x %>%  filter(trial == i),
                       aes(x = x, y = y)) +
        geom_smooth(se = FALSE)  +
        ggtitle(i)
    } else{
      plot_x <- ggplot(x %>%  filter(trial == i, param == my_param),
                       aes(x = value)) +
        geom_histogram(binwidth = binwidth)  +
        ggtitle(i)
    }
    print(plot_x)
    
  }
  dev.off()
}


MakeDensityLab <- function(x, my_param, binwidth = NULL, tolog = FALSE){
  if(tolog ==TRUE) x$value <- log(x$value)
  
  pdf(paste0("Outputs/plot_", my_param, "_logged"[tolog], "_distribution.pdf"))
  for(i in unique(density_x$trial)){
    print(i)
    if(is.null(binwidth)){
      plot_x <- ggplot(density_x %>%  filter(trial == i),
                       aes(x = x, y = y)) +
        geom_smooth(se = FALSE)  +
        ggtitle(i)
    } else{
      plot_x <- ggplot(x %>%  filter(trial == i, param == my_param),
                       aes(x = value)) +
        geom_histogram(binwidth = binwidth)  +
        ggtitle(i)
    }
    print(plot_x)
    
  }
  dev.off()
}


# Get upper trianlge of the correlation matrix 
get_upper_tri <- function(cormat) {
  
  cormat[lower.tri(cormat)] <- NA
  
  return(cormat)
}


# Create heatmap from omega matrix 
matrix_heatmap <- function(omega_m, om_std_er, como_names, title_text) {
  
  # Turn vector into 6x6 matrix with comorbidities as row/col names
  dim(omega_m) <- c(6,6)
  dimnames(omega_m) <- list(como_names, como_names)
  
  # Round correlation 
  omega_m <- as.matrix(round(omega_m, 2))
  
  # Select upper triangle and melt 
  omega_up <- get_upper_tri(omega_m)
  melt_omega <- as.data.frame(as.table(omega_up)) %>% 
    rename(value = Freq) %>% 
    filter(!is.na(value))
  
  # Turn vector of omega estimates and std errors inot 6x6 matrix
  dim(om_std_er)      <- c(6, 6)
  dimnames(om_std_er) <- list(como_names, como_names)
  std_m               <- as.matrix(round(om_std_er, 3))
  std_low             <- get_upper_tri(std_m)
  
  melt_std <- as.data.frame(as.table(omega_up)) %>% 
    rename(value = Freq) %>% 
    filter(!is.na(value))
  
  # Remove std errors = 0 and below threshold
  melt_std <- melt_std %>% filter(!value == 0 & value > 0.001)
  
  # Plot 
  ggplot(melt_omega, aes(x = Var2, y = Var1, fill = value)) + 
    geom_tile() + 
    geom_text(aes(Var2, Var1, label = value), colour = "black", size = 4) +
    geom_text(data = melt_std, aes(Var2, Var1, label = value),
              colour = "dark grey", size = 3, nudge_y = -0.3) + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Correlation") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.65),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.2, 0.7)) + 
    ggtitle(title_text) + 
    coord_fixed()
}


