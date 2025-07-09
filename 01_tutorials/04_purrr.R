#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Teaches you how to use purrr to iterate over lists
# - Compares for loop vs purrr::map()
# - Use of anonymous functions
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

pak::pak("ranger")

library(tidyverse)
library(ranger)

# 2. Load data ------------------------------------------------------------

iris_tbl <- as_tibble(iris) # An R tibble is just a new/improved version of an R data frame that is easier to work with, but functionally it's the same thing as a data frame

# 3. Simple example -------------------------------------------------------

## Iterate over the list to create the message "This fruit is a <fruit_name>"
objects_list <- list("peach", "pear", "cherry", "strawberry", "blackberry")

## 3.1. Using for loop ---------------------------

## Create an empty list
messages_list <- list()

for (i in 1:length(objects_list)) { # This will iterate the code below over every item in objects_list one-by-one
    messages_list[[i]] <- str_glue("This is a {objects_list[[i]]}") # Objects in the list are written as [[1]], [[2]], etc., so to reference a certain item, such as "pear", you must call for [[2]]. 
}

## 3.2. Using functional programming (more intuitive than the for loop option)-------------
messages_list <- map(
    objects_list,
    \(fruit) str_glue("This fruit is a {fruit}")
)

# 4. Model by species -----------------------------------------------------

## Create a different linear model for each Iris species

## 4.1. Using for loop ----------------------------

## Unique Species
species_vec <- unique(iris_tbl$Species)

## Create empty list
filtered_lst <- list()

## Iterate to filter each species
for (species in 1:length(species_vec)) {
    
    ## Filter observations to only keep rows where the Species column in the tibble matches current species in the loop; save the filtered result in "iris_filtered"
    iris_filtered <- iris_tbl |> 
        filter(
            Species == species_vec[species]
        )
    ## Calculate linear model to predict petal length based on sepal length; save the lm in "iris_filtered_lm"
    iris_filtered_lm <- lm(Petal.Length ~ Sepal.Length, data = iris_filtered)
    ## Get summary
    filtered_lst[[species]] <- summary(iris_filtered_lm)
    
}

filtered_lst

## 4.2. Using functional programming (in two steps) --------------

## Create a function
calculate_iris_lm <- function(data) {
    
    lm(Petal.Length ~ Sepal.Length, data = data) |> 
        summary()
    
}

## Create list
iris_species_list <- iris_tbl |> 
    split(iris_tbl$Species)

## Iterate over the list
map(
    iris_species_list,
    calculate_iris_lm
)

## 4.3. In one step ---------------------------------
iris_tbl |> 
    split(iris_tbl$Species) |> 
    map(calculate_iris_lm)

## 4.4. Using anonymous function (don't need to use the "calculate_iris_lm" function we created) --------------------
iris_tbl |> 
    split(iris_tbl$Species) |> # Divides the data frame into groups based on species--each element of the list will be a data frame of rows for ONE species
    map( # From the purrr package
        \(data) lm(Petal.Length ~ Sepal.Length, data = data) |> 
            summary()
    )

# 5. Iterating 2 inputs --------------------------------------------------

## 5.1. Simple example ----------------------------

## Sum over the lists
fruits <- list("peach", "pear", "cherry", "strawberry", "blackberry")
colors <- list("orange", "green", "red", "red", "black")
round <- c("round", "not round", "round", "not round", "round")

## Iterate to create the sentence
map2_chr( # The "_chr" makes the output into a character vector instead of a list
    .x = fruits,
    .y = colors, 
    \(fruit, color) str_glue("The color of the {fruit} is {color}.") # "str_glue" is the function you want to apply
)

## Iterate over 3 vectors
pmap(
    .l = list( # *When you have more than 2 vectors you need to wrap them in a list
        fruits,
        colors,
        round
    ),
    \(fruit, color, round) str_glue("The color of the {fruit} is {color} and is {round}.") 
)

## 5.2. A bigger example --------------------------

## Create a grid of parameters
params_tbl <- expand_grid(
    ntree = c(100, 200, 500, 1000), # Not sure what this is
    mtry  = 1:4 # Only use 1 variable to create each tree, so there is a 1 tree per predictor variable
)

## Create a Random Forest model for each parameter
iris_rf_list <- map2(
    .x = params_tbl$ntree, # The 100, 200, 500, 1000 from above
    .y = params_tbl$mtry, # The 1, 2, 3, 4 from above specifying how many variables to use per tree
    \(ntree, mtry) ranger(
        formula   = Sepal.Length ~ ., # Writing just a "." instead of variable names just means you want to use every variable (aside from the dependent variable) to predict your predicted variable
        data      = iris_tbl,
        num.trees = ntree,
        mtry      = mtry 
    )
)

## Extract r.squared
map_dbl(
    iris_rf_list,
    \(rf_model) rf_model$r.squared
)

## Add as a new column
params_tbl |> 
    mutate(
        rsq = map_dbl(
            iris_rf_list,
            \(rf_model) rf_model$r.squared
        )
    ) |> 
    arrange(desc(rsq))

