

## libraries
library(dplyr)
library(here) 

## load data 
cdbg_init = read.csv(here("data/Community_Development_Block_Grant_Grantee_Areas_new coding - Filtered and subsampled .csv"))

## clean up cols
colnames(cdbg_init) = gsub("\\.+", "_", colnames(cdbg_init))

## filter out non-coded ones
cdbg_coded = cdbg_init[1:153, ] %>%
      rename(dist_method = DISTRIBUTION_METHOD_rj_note_these_were_coded_more_consistently_later_but_coding_guide_not_followed_closely_eg_different_variations_of_first_come_first_serve_spelling_so_goal_here_is_to_fill_in_blank_rows_and_for_non_blank_ones_to_double_check_whether_you_agree_with_the_distribution_method_coding_based_on_the_website_and_then_standardize_the_name_based_on_the_Code_sheet_) %>%
      filter(dist_method != "") # blank ones are ones with unrecoverable website or unfindable 

table(cdbg_coded$dist_method, useNA = "always")

cdbg_coded = cdbg_coded %>%
        mutate(method_is_fc = ifelse(grepl("FCFS|First come", dist_method), TRUE, FALSE),
               method_is_points = ifelse(grepl("Points", dist_method), TRUE, FALSE),
               method_is_comm = ifelse(grepl("Comm", dist_method), TRUE, FALSE),
               method_is_lott = ifelse(grepl("Lottery", dist_method), TRUE, FALSE),
               method_notdisc = ifelse(dist_method == "M", TRUE, FALSE),
               method_other = ifelse(dist_method == "Other", TRUE, FALSE)) # CODE OTHER EXCLUSIVELY

colSums(cdbg_coded[, grep("^method", colnames(cdbg_coded), value = TRUE)])
colMeans(cdbg_coded[, grep("^method", colnames(cdbg_coded), value = TRUE)])



