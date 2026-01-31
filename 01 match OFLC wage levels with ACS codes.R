# Tag OFLC occupations with the best ACS OCCSOC code to match with microdata.

# Load ACS data
ddi <- read_ipums_ddi("usa_00069.xml")
occupation_5yrdata <- read_ipums_micro(ddi)

# Find the unique occupations (OCCSOC codes) in the ACS data
occupation_totals <- occupation_5yrdata %>%
  group_by(OCCSOC) %>%
  summarise(total = sum(PERWT)) %>%
  arrange(desc(total))

# Reformat ACS OCCSOC to match up with SOC codes 
occupation_totals$OCCSOC <- sub("^(.{2})", "\\1-", occupation_totals$OCCSOC)

# For occupations formatted like "15-10XX" or "29-1XXX", add new variable replacing X's with zero
# occupation_totals <- occupation_totals %>%
#   mutate(OCCSOC2 = str_replace_all(OCCSOC, "X", "0"))

occupation_totals <- occupation_totals %>%
  mutate(
    OCCSOC_chr = as.character(OCCSOC),
    
    Full_SOC = if_else(
      !str_detect(OCCSOC_chr, "X"),
      OCCSOC_chr,
      NA_character_
    ),
    
    SOC2 = if_else(
      str_detect(OCCSOC_chr, "X$") & !str_detect(OCCSOC_chr, "XX$"),
      str_replace(OCCSOC_chr, "X$", "0"),
      NA_character_
    ),
    
    SOC3 = if_else(
      str_detect(OCCSOC_chr, "XX$") & !str_detect(OCCSOC_chr, "XXX$"),
      str_replace(OCCSOC_chr, "XX$", "00"),
      NA_character_
    ),
    
    SOC4 = if_else(
      str_detect(OCCSOC_chr, "XXX$"),
      str_replace(OCCSOC_chr, "XXX$", "000"),
      NA_character_
    )
  ) %>%
  select(-OCCSOC_chr)

# Load the latest OFLC wage levels 
oflc_wage_levels_24_25<- read.csv("OFLC_Wages_2024-25/OFLC FY 2025.csv")

# Create a list of all the occupations for which OFLC produces wage levels
oflc_occupations_list <- oflc_wage_levels_24_25 %>%
  group_by(SocCode) %>%
  summarise(n = n())


# Load names files that has Broad and Minor Occupations 
soc_names <- read_excel("OFLC_Wages_2024-25/Wage Year 2024-25 Appendix A, Job Zone, and Education.xlsx", sheet = "All SOC Codes (Job Zones)")

soc_names_cut <- soc_names %>% select(`SOC Code`, `SOC Broad Occupation`, `SOC Minor Occupation`) %>%
  distinct()

# Merge by detailed, broad, and minor groups. Then manually add remaining matches and combine together. 
oflc_occupations_list <- oflc_occupations_list %>% left_join(soc_names_cut, by = c("SocCode" = "SOC Code"))

direct_matches <- oflc_occupations_list %>%
  left_join(occupation_totals, by = c("SocCode" = "Full_SOC")) %>%
  filter(!is.na(total)) %>%
  mutate(ACS_occupation = SocCode) %>%
  select(-SOC2, -SOC3, -SOC4, -total)

secondary_matches <- oflc_occupations_list %>%
  left_join(occupation_totals, by = c("SOC Broad Occupation" = "SOC2")) %>%
  filter(!is.na(OCCSOC)) %>%
  mutate(ACS_occupation = OCCSOC) %>%
  select(-Full_SOC, -SOC3, -SOC4, -total)

tertiary_matches <- oflc_occupations_list %>%
  left_join(occupation_totals, by = c("SOC Minor Occupation" = "SOC3")) %>%
  filter(!is.na(OCCSOC)) %>%
  mutate(ACS_occupation = OCCSOC) %>%
  select(-Full_SOC, -SOC2, -SOC4, -total)
  

quaternary_matches <- oflc_occupations_list %>%
  left_join(occupation_totals, by = c("SOC Minor Occupation" = "SOC4")) %>%
  filter(!is.na(OCCSOC)) %>%
  mutate(ACS_occupation = OCCSOC) %>%
  select(-Full_SOC, -SOC2, -SOC3, -total)

manual_matches <- oflc_occupations_list %>%
  mutate(OCCSOC = case_when(SocCode == "11-9131" ~ "11-91XX",
                                       SocCode == "11-9171" ~ "11-91XX",
                                       SocCode == "11-9179"~ "11-91XX",
                                       SocCode == "11-9199" ~ "11-91XX",
                                       SocCode == "29-1161" ~ "29-11XX", 
                                       SocCode == "29-1171" ~ "29-11XX",
                                       SocCode == "29-1221" ~ "29-12XX", # Custom ACS group
                                       SocCode == "29-1222" ~ "29-12XX", # Custom ACS group
                                       SocCode == "29-1223" ~ "29-12XX", # Custom ACS group
                                       SocCode == "29-1224" ~ "29-12XX", # Custom ACS group
                                       SocCode == "29-1229" ~ "29-12XX", # Custom ACS group
                                       SocCode == "51-9141" ~ "51-91XX", 
                                       SocCode == "51-9192" ~ "51-91XX",
                                       SocCode == "51-9193"~ "51-91XX",
                                       SocCode == "51-9199" ~ "51-91XX",
                                       TRUE ~ NA)) %>%
  mutate(ACS_occupation = OCCSOC) %>%
  filter(!is.na(ACS_occupation))
  


combined_occupations <- rbind(direct_matches, secondary_matches,
                              tertiary_matches, quaternary_matches,
                              manual_matches)

# Check that there are no OFLC codes that aren't assigned an ACS code

combined_occupations %>% filter(is.na(ACS_occupation))

  
  