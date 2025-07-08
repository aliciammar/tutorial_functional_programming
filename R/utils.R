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
