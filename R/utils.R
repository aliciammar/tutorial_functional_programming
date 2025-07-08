## FUNCTION: Convert girth from inches to centimeters
convert_in_to_cm <- function(x) {
    x * 2.54
}


## FUNCTION: Convert height from feet to meters
convert_ft_to_m <- function(x) {
    x * 0.3048
}


## FUNCTION: Calculate volume in cubic meters
calculate_volume <- function(diameter, height) {
    pi / 4 * (diameter / 100)^2 * height
}


## FUNCTION: Summarize iris data set
calc_iris_mean <- function(data) {
    data |> 
        summarize(
            across(
                where(is.numeric), mean
            ),
            .by = Species
        ) |> 
        pivot_longer(
            cols      = where(is.numeric),
            names_to  = "measure",
            values_to = "mean"
        ) |> 
        group_by(measure) |> # **when you create groups with group_by(), you must ungroup them when you don't need them anymore
        arrange(
            desc(mean),
            .by_group = TRUE
        ) |> 
        ungroup()
}


## FUNCTION: Summarize mean of numeric variables
calc_numeric_mean <- function(data, group) {
    data |> 
        summarize(
            across(
                where(is.numeric), \(x) mean(x, na.rm = TRUE) # An "anonymous function"--one you only use once
            ),
            .by = {{ group }} # Curly braces tell R to look for "group" in the function's arguments rather than looking for a variable called "group" in the data set
        ) |> 
        pivot_longer(
            cols      = where(is.numeric),
            names_to  = "measure",
            values_to = "mean"
        ) |> 
        group_by(measure) |> # **when you create groups with group_by(), you must ungroup them when you don't need them anymore
        arrange(
            desc(mean),
            .by_group = TRUE
        ) |> 
        ungroup()
}