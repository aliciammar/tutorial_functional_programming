#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Teaches to organize the functions in a different file
# - Source functions into other scripts
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

## Load packages
library(tidyverse)

## Load functions
source("R/utils.R")

# 2. Load data ------------------------------------------------------------

## Load data
trees_tbl <- as_tibble(trees) |> 
    select(-Volume)

# 3. Calculate volume -----------------------------------------------------

## -> Convert girth from inches to centimeters
## -> Convert height from feet to meters
## -> Calculate volume in m3
trees_tbl |> 
    mutate(
        Girth_cm = convert_in_to_cm(Girth),
        Height_m = convert_ft_to_m(Height),
        Volume_m3 = calculate_volume(Girth_cm, Height_m) # You can use variables created earlier within the same function in this function!
    )
