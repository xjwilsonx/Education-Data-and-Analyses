library(educationdata)
library(dplyr)
library(tidyverse)
library(devtools)
# Political Wrangling ####
# from MIT https://github.com/MEDSL
# data<-read.csv("2024-president-state.csv")
# state_lean_2024 <- data %>%
#   # the beloved two party system
#   filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>%
#   select(state, state_fips, party_simplified, votes, totalvotes) %>%
#   # calc percentage
#   mutate(pct = votes / totalvotes) %>%
#   # pivot
#   pivot_wider(names_from = party_simplified, values_from = c(votes, pct)) %>%
#   # calc margin
#   mutate(margin = pct_DEMOCRAT - pct_REPUBLICAN)
# 
# # # 2. 5 level grouping
# final_political_groups <- state_lean_2024 %>%
#   mutate(political_tier = case_when(
# #these are somewhat well established thresholds
#     margin >  0.15 ~ "Solid Blue",
#     margin >  0.03 ~ "Lean Blue",
#     margin >= -0.03 & margin <= 0.03 ~ "Battleground",
#     margin < -0.03 & margin >= -0.15 ~ "Lean Red",
#     margin < -0.15 ~ "Solid Red"
#   )) %>%
#   # factor
#   mutate(political_tier = factor(political_tier,
#                                  levels = c("Solid Red", "Lean Red", "Battleground", "Lean Blue", "Solid Blue"))) |>
#   select(state, political_tier)
# 
# 
# # this is how i print the csv to text to use in script
# dput(final_political_groups)

poli<- structure(list(state = c("ALASKA", "ALABAMA", "ARKANSAS", "ARIZONA", 
                                "CALIFORNIA", "COLORADO", "CONNECTICUT", "DISTRICT OF COLUMBIA", 
                                "DELAWARE", "FLORIDA", "GEORGIA", "HAWAII", "IOWA", "IDAHO", 
                                "ILLINOIS", "INDIANA", "KANSAS", "KENTUCKY", "LOUISIANA", "MASSACHUSETTS", 
                                "MARYLAND", "MAINE", "MICHIGAN", "MINNESOTA", "MISSOURI", "MISSISSIPPI", 
                                "MONTANA", "NORTH CAROLINA", "NORTH DAKOTA", "NEBRASKA", "NEW HAMPSHIRE", 
                                "NEW JERSEY", "NEW MEXICO", "NEVADA", "NEW YORK", "OHIO", "OKLAHOMA", 
                                "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", 
                                "TENNESSEE", "TEXAS", "UTAH", "VIRGINIA", "VERMONT", "WASHINGTON", 
                                "WISCONSIN", "WEST VIRGINIA", "WYOMING"),
                      political_tier = structure(c(2L, 
1L, 1L, 2L, 5L, 4L, 4L, 5L, 4L, 2L, 3L, 5L, 2L, 1L, 4L, 1L, 1L, 
 1L, 1L, 5L, 5L, 4L, 3L, 4L, 1L, 1L, 1L, 2L, 1L, 1L, 3L, 4L, 4L, 
2L, 4L, 2L, 1L, 4L, 3L, 4L, 1L, 1L, 1L, 2L, 1L, 4L, 5L, 5L, 3L, 
1L, 1L), levels = c("Solid Red", "Lean Red", "Battleground", 
  "Lean Blue", "Solid Blue"), class = "factor")), row.names = c(NA, 
 -51L), class = c("tbl_df", "tbl", "data.frame"))


# educationdata CCD API calls ####
raw_file <- "lea_data_merged_march26.rda"


if(!file.exists(raw_file)){
  # 1. Get Directory Info (Names, Cities, Locales) for the most recent year
# We pull this once to act as our "master list" of names/locations
  dist_directory <- get_education_data(level = "school-districts",
                                     source = "ccd",
                                     topic = "directory",
                                     filters = list(year = 2014:2024),
                                     add_labels = TRUE) |>
    mutate(
      # capitalize
      fips = toupper(fips)
    )

  # 2024 is now ready

  
  
# Small Area Income and Poverty Estimates (SAIPE) ####

# The U.S. Census Bureau’s Small Area Income and Poverty Estimates program produces single-year
  # estimates of income and poverty for all U.S. states and counties as well as estimates of
  # school-age children in poverty for all 13,000+ school districts.
  
  saipe_data <- get_education_data(level = "school-districts",
                                   source = "saipe",
                                   filters = list(year = 2014:2024),
                                   add_labels = TRUE) 
  
  
  # ppexp <- get_education_data(
  #   level = "school-districts",
  #   source = "ccd",
  #   topic = "finance",
  #   filters = list(year = 2014)
  # )

  # exp <- ppexp |>
  #   filter(
  #     exp_current_elsec_total>1000,
  #     enrollment_fall_school >101
  #   ) |>
  #   select(year, leaid, exp_current_elsec_total, enrollment_fall_school) |>
  #   mutate(per_pupil_spending = as.numeric(exp_current_elsec_total)/ as.numeric(enrollment_fall_school))
  # # apparently exp_current_elsec_total = Current expenditures for elementary/secondary education

  # summary(exp$per_pupil_spending)
  # hist(exp$per_pupil_spending, breaks = 100)
  # quantile(exp$per_pupil_spending, 
  #          c(.01, .05, .10, .25, .50, .75, .90, .95, .99), 
  #          na.rm = TRUE)


  
  # Save the objects as a list
  save(dist_directory, saipe_data, file = raw_file)

  
} else 

{
load(raw_file)
}
  
# 2: Data Cleaning and Merging ####

  saipe_clean <- saipe_data |> 
    mutate(
      poverty_pct = est_population_5_17_poverty_pct
    ) |>
    select(
      leaid, year, poverty_pct
    )
  names(saipe_clean)
unique(dist_directory$state_location)
table(dist_directory$state_location)
x <- dist_directory %>% filter(
  state_location == ""
)
directory_clean <- dist_directory |>
  filter(
    
    !grepl('VI|PR|MP|GU',state_location),
    state_location !="",
    # these filters exclude non-traditional school districts 
    
    # no virtual schools, must have  one building


    number_of_schools >=1,
    
    # No Psychiatric or Mental Health Facilities
    # districts for the deaf and blind are excluded here 
# these word boundaries are better than excluding Blindville schools by accident 
!grepl('\\bPsychiatric\\b|\\bMental\\b|\\bDeaf\\b|\\bBlind\\b|\\bVirtual\\b|\\bCharter\\b', 
      lea_name,
       ignore.case=TRUE),
    
    # must have enrollment data 
    !(enrollment == 0 | enrollment==""| is.na(enrollment)| enrollment <100),
    
    # must have teacher data 
    
    teachers_total_fte >2,

  #  staff_total_fte >1

    
  ) 


# since the geo info might change, I need to pull that from 2023 or slice max by group (leaid)

recent_geo <- directory_clean |>
  group_by(leaid) |>
  slice_max(year) |>
  ungroup() |>
  select(
    leaid, lea_name, latitude, longitude, fips, city_location, city_mailing, state_leaid, state_location
  )

data_clean <- directory_clean |> 
  select(
    year,
    leaid,
    enrollment,
    teachers_total_fte,
    school_administrators_fte,
    # data quality issues with these two metrics
    # school_admin_support_staff_fte,
    # school_staff_total_fte,
    staff_total_fte,
    urban_centric_locale,
    number_of_schools
  ) |>
  inner_join(
    recent_geo,
    by = "leaid"
  ) |> left_join(
    saipe_clean,
    by = c("leaid", "year")
  )|> left_join(poli,
                by = c("fips" = "state")) |>
  mutate(
    
    school_administrators_fte = if_else(school_administrators_fte <= 1,
       NA_real_,
        school_administrators_fte),
    # #not enough good data here
    # school_admin_support_staff_fte = if_else(school_admin_support_staff_fte <= 1,
    #    NA_real_,
    #     school_admin_support_staff_fte),
    # # not enough good data here 
    # school_staff_total_fte = if_else(school_staff_total_fte <= 1,
    #    NA_real_,
    #    school_staff_total_fte),
    
    size = case_when(
      enrollment < 2500 ~ "Small",
      enrollment >= 2500 & enrollment < 10000 ~ "Medium",
      enrollment >= 10000 & enrollment < 25000 ~ "Large",
      enrollment >= 25000 ~ "Very Large",
      TRUE ~ NA_character_
    ),

# this ntile function bins poverty % intp 5 groups
    poverty_group = ntile(poverty_pct,
       5),
# label groups  
    poverty_group = case_match(poverty_group,
      1 ~ "Very Low",
      2 ~ "Low",
      3 ~ "Moderate",
      4 ~ "High",
      5 ~ "Very High",
      .default = "Unknown"
    ),
    
    locale_major = case_when(
      #  Inside a principal city (Large, Midsize, Small).
    grepl("City", urban_centric_locale, ignore.case = TRUE) ~ "City",
    # Outside a principal city, but inside an urbanized area.
grepl("Suburb", urban_centric_locale, ignore.case = TRUE) ~ "Suburb",
    # Inside an "urban cluster" (smaller than a city) and far from cities.
  grepl("Town", urban_centric_locale, ignore.case = TRUE) ~ "Town",
    # Census-defined rural territory.
 grepl("Rural", urban_centric_locale, ignore.case = TRUE) ~ "Rural",
TRUE ~ "Other/Unknown"
  ),
   
      # here lie the most important metrics 
      # Zero missing, median of 12 students per teacher That p100 of 232 is almost certainly a single bad record 
    students_per_teacher =  enrollment /teachers_total_fte,

    # ~4% gone) from your earlier <1 → NA decision. Median 268 students per administrator
    #  The p100 of 26,625 is extreme — that's one district claiming essentially no administrators for tens of thousands of students.
students_per_school_admin = enrollment/ school_administrators_fte,

    # this is a poor metric , lots of outliers and missing data (14%)
    # student_admin_support_ratio = enrollment /school_admin_support_staff_fte,

# missing about 12%
    # student/total_school_staff_student_ratio = enrollment/ school_staff_total_fte,

    # this looks to be great metric with minimal outliers and missing data
    students_per_total_staff = enrollment/ staff_total_fte
    
  ) |>
  
  # Metric Filters ####
filter(
  # outliers exist due to SPED coops and special districts
  # some report irregular staffing
  # 1% = 2.9
  # 99% = 13.0
  between(students_per_total_staff, 2, 50))



write.csv(
  data_clean,
  

 "lea_longitudinal_staffing.csv",
 row.names= FALSE
)
