# get a pallete of distinctive colors
#' Color Pallete
#'
#' The function gives a list of mentioned number of distinct color hex values
#'
#' @param n - number of distinct colors to be chosen
#'
#' @return list - list of `n` distinct hex values representing colors
#' @import RColorBrewer

get_pallete <- function(n) {
  # https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
  # Maximum distinct colors you can get is 28

  message("User want a color pallete with ", n, " distinct colors")
  qual_col_pals = RColorBrewer::brewer.pal.info[brewer.pal.info$category == 'qual' &
                                                  brewer.pal.info$colorblind == T, ]
  col_vector = unlist(mapply(
    brewer.pal,
    qual_col_pals$maxcolors,
    rownames(qual_col_pals)
  ))
  col_vector <- col_vector[1:n]
  return(col_vector)
}

#' Create Pie chart
#'
#' This function create a pie from the distinct color returned by the
#' get_pallete() function
#'
#' @param l - list of hex values defining the colors
#' @param n - number of distinct colors chosen
#'
#' @return pie
#' @import RColorBrewer

get_pie <- function(l, n) {
  # filepath <- system.file("inst/extdata/Num.csv", package = "colPal")
  # num <- read.csv(filepath)
  # n <- num[1]
  pie(rep(1, n), col = sample(l, n))
}

#' Color Pie
#'
#' This function takes number of colors from user and creates a pie out of it
#' @param n
#'
#' @return plots a color pie, list of color codes (hex format)
#' @export
#'
get_color_pie <- function(number_of_colors) {
  colList <- get_pallete(number_of_colors)
  get_pie(colList, length(colList))
  return(colList)
}
