# perhaps add another function to name the output dataframe
#' Load all images into one
#'
#' @title Image Loader
#'
#' @param y Folder where images are stored
#'
#'
#' @return returns a dataframe of images
#' @export
#'
#' @examples
#' load_images(here("Images/"))
load_images <- function(y) {
  # x in this case is the name of the directory with the images
  # images would be a great example
  working <- here::here()
  # return file list, full.names allows passage of the entire file paths
  return1 <- list.files(y, full.names = TRUE)
  # prints proof of concept
  print(return1)
  # full paths, if needed
  return2 <- paste(working, return1, sep = "/")
  # assign back to global environment
  images <<- data.frame("local_path" = return1, "global_path" = return2)
}

# perhaps add another function to name the output dataframe
#' Load all images into one
#'
#' @title Convert and Import
#'
#' @param X Folder where images are stored
#'
#'
#' @return returns a dataframe of images
#' @export
#'
#' @examples
#' load_images(here("Images/"))
convert_and_import<-function(x){
  dir.create("converted")
  purrr::map(.x = x$local_path, .f=lower_converter)
  converted_images<<-data.frame(local_path= list.files("converted", full.names = TRUE), old_local_path = images$local_path)
}

#function called by convert and import
lower_converter<-function(x){
  Z <- magick::image_read(x)
  ZZbop <- magick::image_convert(Z, matte = TRUE, format = "png")
   #removes corrupted metadata from files
  ZZtop<-magick::image_strip(ZZbop)
  magick::image_write(ZZtop, paste("converted/", stringi::stri_rand_strings(1, 
                                                                            5, pattern = "[A-Za-z0-9]"), ".png", sep = ""), format = "png")
}


# reasonably fast, somewhat annoying to parse through a magick pointer
#' Measure image information and OCR (Optical character recognition)
#'
#' @title Measure Image
#'
#' @param images Image to be read in
#'
#' @details This function returns a unified dataframe that takes your loaded images dataframe and returns a dataframe with image measurements and an OCR reading of the text from the image.
#' @return Returns a dataframe called "measured_images" that is the meta data for the images and an OCR of image text
#' @export
#'
#' @examples
#' measure_images(here("Images/image_1.png"))
measure_images <- function(images) {
  library(magrittr)
  ml_images<-images$local_path%>%
    purrr::map( ~ magick::image_read(.))
  measured_images<<-purrr::map_df(1:length(ml_images), ~ data.frame(
    a = .x,
    text = magick::image_ocr(ml_images[[.x]]),
    info = magick::image_info(ml_images[[.x]])))
  print("OCR results may be misleading if images include no text")
}

# this is horribly slow
#' Get image fluency
#'
#' @title Fluency
#'
#' @param image Image to be read in
#'
#'
#' @return Returns image contrast, similarity, symmetry, complexity
#' @export
#' @details this function implements multiple tests from the Imagefluency package returning a single dataframe, this function can take quite a while to run.
#' @examples
#' fluency(here("Images/image_1.png"))
fluency <- function(image) {
  fl_images <- image$local_path %>%
    purrr::map( ~ imagefluency::img_read(.))
  fluency_results <- purrr::map_df(1:length(fl_images), ~ data.frame(
    a = .x,
    b = imagefluency::img_contrast(fl_images[[.x]]),
    c = imagefluency::img_self_similarity(fl_images[[.x]]),
    d = imagefluency::img_symmetry(fl_images[[.x]]),
    e = imagefluency::img_complexity(fl_images[[.x]])
  ))
  fluency_results<<-unique(fluency_results)
}


# symmetry function
#' Gets image symmetry
#'
#' @title Image Symmetry
#'
#' @param images Folder where images are stored
#'
#' @details Detects symmetry in an image along three axes.
#' @details Closer to zero means more symmetrical, positive means the image has more ink left or up, negative the opposite
#' @details The diagonal method has defocused areas along the line y=x, the priority is reading symmetry not in the center of the image
#' @return Returns a dataframe with horizontal, vertical, and diagonal symmetry
#' @export
#'
#' @examples
#' symmetry(here("Images/"))
symmetry_analysis <- function(images) {
  ml_images<-images$local_path%>%
    purrr::map( ~ magick::image_read(.))
  symmetry_images<<-purrr::map_df(1:length(ml_images), ~ data.frame(
    a = .x,
    symmetry_lower(ml_images[[.x]])))
  print(symmetry_images)
}


# thirds function
#' Gets image symmetry
#'
#' @title Rule of Thirds
#'
#' @param images Folder where images are stored
#'
#' @details Detects intensity of the use of the thirds on a traditional photographic layout
#' @details Positive means there is more activity in the third versus the full image, negative means less
#' @return Returns a dataframe with scores for each third versus the image
#' @export
#'
#' @examples
#' symmetry(here("Images/"))
thirds_images <- function(images) {
  ml_images<-images$local_path%>%
    purrr::map( ~ magick::image_read(.))
  thirds_results_images<-purrr::map_df(1:length(ml_images), ~ data.frame(
    a = .x,
    low_hor = low1(ml_images[[.x]]),
    high_hor = high1(ml_images[[.x]]),
    left_vert = left1(ml_images[[.x]]),
    right_vert = right1(ml_images[[.x]])))
  thirds_results<-thirds_results_images%>%mutate(vert_focal = ifelse(low_hor>high_hor, "Low", "High"))
  thirds_results<<-thirds_results%>%mutate(hor_focal = ifelse(left_vert>right_vert, "Left", "Right"))
  print(thirds_results)
}


# edge analysis
#' Performs edge analysis
#'
#' @title Edge Analysis
#'
#' @param images Folder where images are stored
#' @details this function uses the same 16 cell grid for image segmentation used in the symmetry function
#' @details underlying math in this function is from a mean of canny edges detected per cell and deviation of them
#' @details the mean would speak to the total value of "ink" in the zone and the deviation may inform the character of the edges
#'
#' @return Returns a dataframe consisting of images, PQ, ST
#' @export
#'
#' @examples
#' load_images(here("Images/"))
edge_analysis <- function(images) {
  ml_images<-images$local_path%>%
    purrr::map( ~ magick::image_read(.))
  edged_R<-purrr::map_df(1:length(ml_images), ~ data.frame(
    d = edge_lower(ml_images[[.x]])))
  edged_images<<-dplyr::distinct(edged_R, .keep_all=TRUE)
  print("edge analysis complete")
}

#' Function to extract image colors
#'
#' @title Image plotter
#'
#' @param X does a thing
#' @details the color class is a function that yields tertiary color regions, see https://en.wikipedia.org/wiki/Tertiary_color
#' @details a segmented version of colors could be available in the next zone
#'
#' @return returns a dataframe of images
#' @export
#'
#' @examples
#' colors() # This is unclear
color_analysis<- function(x){
  c_images<-x$local_path
  colors_results<-purrr::map_df(.x = c_images, .f=lower_colors)
  colors_results<<-colors_results%>%mutate(hue_region = ifelse(mean_blue>mean_red & mean_red >= mean_green, "Violet", ifelse(mean_blue>mean_red & mean_blue < mean_green, "Spring green",
                                                                                                                             ifelse(mean_green > mean_red & mean_red >= mean_blue, "Chartreuse", ifelse(mean_green > mean_red & mean_green > mean_blue, "Azure",
                                                                                                                                                                                                        ifelse(mean_red >= mean_green & mean_green >= mean_blue, "Orange", "Rose"))))))
}

#' function that allows you to pass alpha to a GG plot that also encodes other things

#' @title Image Plot Export
#' @description This function is designed to simplify passing arguments into a ggplot with geom_image to produce an image plot
#' @param D is where the data is
#' @param X is the X var
#' @param Y is the Y var
#' @param A is the alpha
#' @export
imageplot_output <-function(Q,X,Y,A){
  transparent <- function(img) {
    B <- paste(A, "*a", sep = "")
    magick::image_fx(img, expression = B, channel = "alpha")}
  G<-paste("ggplot(",Q, ",aes(",X,",",Y,"))+ggimage::geom_image(image =",Q,"$local_path, image_fun=transparent)", sep = "")
  eval(parse(text=G))}


#completed lower-order functions
low1 <- function(x){
  library(dplyr)
  rudy2 <- magick::image_canny(x)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)
  # vert 1
  V1 <- ZZZZZ %>%
    dplyr::filter(x > .16666 * Q & x < .5 * Q)
  V2 <- ZZZZZ %>%
    dplyr::filter(x > .5 * Q & x < Q)
  V3 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .16666 * Q)
  # vert 2
  V4 <- ZZZZZ %>%
    dplyr::filter(x > .5 & x < .83333 * Q)
  V5 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5 * Q)
  V6 <- ZZZZZ %>%
    dplyr::filter(x > .83333 * Q & x < Q)
  # horz 1
  H1 <- ZZZZZ %>%
    dplyr::filter(y > .16666 * P & y < .5 * P)
  H2 <- ZZZZZ %>%
    dplyr::filter(y > .5 * P & y < P)
  H3 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .16666 * P)
  # horz 2
  H4 <- ZZZZZ %>%
    dplyr::filter(y > .5 & y < .83333 * P)
  H5 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .5 * P)
  H6 <- ZZZZZ %>%
    dplyr::filter(y > .83333 * P & y < P)
  L <- mean(H5$value)
  W <- mean(H6$value)
  Q <- mean(H4$value)
  I <- ((Q + Q + Q + W) / 4)
  low_hor <- L - I
  print(low_hor)
}
high1 <- function(x){

  rudy2 <- magick::image_canny(x)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)
  # vert 1
  V1 <- ZZZZZ %>%
    dplyr::filter(x > .16666 * Q & x < .5 * Q)
  V2 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < max(x))
  V3 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .16666 * Q)
  # vert 2
  V4 <- ZZZZZ %>%
    dplyr::filter(x > .5 & x < .83333 * Q)
  V5 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5 * Q)
  V6 <- ZZZZZ %>%
    dplyr::filter(x > .83333 * max(x) & x < max(x))
  # horz 1
  H1 <- ZZZZZ %>%
    dplyr::filter(y > .16666 * P & y < .5 * P)
  H2 <- ZZZZZ %>%
    dplyr::filter(y > .5 * P & y < P)
  H3 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .16666 * P)
  # horz 2
  H4 <- ZZZZZ %>%
    dplyr::filter(y > .5 & y < .83333 * P)
  H5 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .5 * P)
  H6 <- ZZZZZ %>%
    dplyr::filter(y > .83333 * P & y < P)
  L <- mean(H1$value)
  W <- mean(H2$value)
  Q <- mean(H3$value)
  I <- ((Q + Q + Q + W) / 4)
  high_hor <- L - I
  print(high_hor)
}
left1 <- function(x){
  rudy2 <- magick::image_canny(x)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)

  # vert 1
  V1 <- ZZZZZ %>%
    dplyr::filter(x > .16666 * Q & x < .5 * Q)
  V2 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < max(x))
  V3 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .16666 * Q)
  # vert 2
  V4 <- ZZZZZ %>%
    dplyr::filter(x > .5 & x < .83333 * Q)
  V5 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5 * Q)
  V6 <- ZZZZZ %>%
    dplyr::filter(x > .83333 * max(x) & x < max(x))
  # horz 1
  H1 <- ZZZZZ %>%
    dplyr::filter(y > .16666 * P & y < .5 * P)
  H2 <- ZZZZZ %>%
    dplyr::filter(y > .5 * P & y < P)
  H3 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .16666 * P)
  # horz 2
  H4 <- ZZZZZ %>%
    dplyr::filter(y > .5 & y < .83333 * P)
  H5 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .5 * P)
  H6 <- ZZZZZ %>%
    dplyr::filter(y > .83333 * P & y < P)


  L <- mean(V1$value)
  W <- mean(V2$value)
  Q <- mean(V3$value)
  I <- ((Q + Q + Q + W) / 4)
  left_vert <- L - I
  print(left_vert)
}
right1 <- function(x){
  rudy2 <- magick::image_canny(x)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)


  # vert 1
  V1 <- ZZZZZ %>%
    dplyr::filter(x > .16666 * Q & x < .5 * Q)
  V2 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < max(x))
  V3 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .16666 * Q)
  # vert 2
  V4 <- ZZZZZ %>%
    dplyr::filter(x > .5 & x < .83333 * Q)
  V5 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5 * Q)
  V6 <- ZZZZZ %>%
    dplyr::filter(x > .83333 * max(x) & x < max(x))
  # horz 1
  H1 <- ZZZZZ %>%
    dplyr::filter(y > .16666 * P & y < .5 * P)
  H2 <- ZZZZZ %>%
    dplyr::filter(y > .5 * P & y < P)
  H3 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .16666 * P)
  # horz 2
  H4 <- ZZZZZ %>%
    dplyr::filter(y > .5 & y < .83333 * P)
  H5 <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .5 * P)
  H6 <- ZZZZZ %>%
    dplyr::filter(y > .83333 * P & y < P)

  L <- mean(V4$value)
  W <- mean(V5$value)
  Q <- mean(V6$value)
  I <- ((Q + Q + Q + W) / 4)
  right_vert <<- L - I
  print(right_vert)
}

edge_lower<- function(x){
  library(moments)
  rudy2 <- magick::image_canny(x)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  dim(ZZZZZ)
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)
  # select quarter regions
  # regions start in the upper left and head for bottom right

  # select regions
  # TOP ROW
  R1 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .25 * Q) %>%
    dplyr::filter(y > 0 & y < .25 * P)

  R2 <- ZZZZZ %>%
    dplyr::filter(x > .25 * Q & x < .5 * Q) %>%
    dplyr::filter(y > 0 & y < .25 * P)

  R3 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < .75 * max(x)) %>%
    dplyr::filter(y > 0 & y < .25 * P)

  R4 <- ZZZZZ %>%
    dplyr::filter(x > .75 * max(x) & x < max(x)) %>%
    dplyr::filter(y > 0 & y < .25 * P)

  # UPPER MIDDLE ROW
  R5 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .25 * Q) %>%
    dplyr::filter(y > .25 & y < .5 * P)

  R6 <- ZZZZZ %>%
    dplyr::filter(x > .25 * Q & x < .5 * Q) %>%
    dplyr::filter(y > .25 & y < .5 * P)

  R7 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < .75 * max(x)) %>%
    dplyr::filter(y > .25 & y < .5 * P)

  R8 <- ZZZZZ %>%
    dplyr::filter(x > .75 * max(x) & x < max(x)) %>%
    dplyr::filter(y > .25 & y < .5 * P)

  # LOWER MIDDLE ROW
  R9 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .25 * Q) %>%
    dplyr::filter(y > .5 * P & y < .75 * P)

  R10 <- ZZZZZ %>%
    dplyr::filter(x > .25 * Q & x < .5 * Q) %>%
    dplyr::filter(y > .5 * P & y < .75 * P)

  R11 <- ZZZZZ %>%
    dplyr::filter(x > .5 * Q & x < .75 * Q) %>%
    dplyr::filter(y > .5 * P & y < .75 * P)

  R12 <- ZZZZZ %>%
    dplyr::filter(x > .75 * max(x) & x < max(x)) %>%
    dplyr::filter(y > .5 * P & y < .75 * P)

  # bottom row
  R13 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .25 * Q) %>%
    dplyr::filter(y > .75 * P & y < P)

  R14 <- ZZZZZ %>%
    dplyr::filter(x > .25 * Q & x < .5 * Q) %>%
    dplyr::filter(y > .75 * P & y < P)

  R15 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < .75 * max(x)) %>%
    dplyr::filter(y > .75 * P & y < P)

  R16 <- ZZZZZ %>%
    dplyr::filter(x > .75 * max(x) & x < max(x)) %>%
    dplyr::filter(y > .75 * P & y < P)

  # sums of each region for canny edge
  R1_edge <- sum(R1$value)
  R2_edge <- sum(R2$value)
  R3_edge <- sum(R3$value)
  R4_edge <- sum(R4$value)
  R5_edge <- sum(R5$value)
  R6_edge <- sum(R6$value)
  R7_edge <- sum(R7$value)
  R8_edge <- sum(R8$value)
  R9_edge <- sum(R9$value)
  R10_edge <- sum(R10$value)
  R11_edge <- sum(R11$value)
  R12_edge <- sum(R12$value)
  R13_edge <- sum(R13$value)
  R14_edge <- sum(R14$value)
  R15_edge <- sum(R15$value)
  R16_edge <- sum(R16$value)

  # deviation for each region
  R1_edge_dev <- sd(R1$value)
  R2_edge_dev <- sd(R2$value)
  R3_edge_dev <- sd(R3$value)
  R4_edge_dev <- sd(R4$value)
  R5_edge_dev <- sd(R5$value)
  R6_edge_dev <- sd(R6$value)
  R7_edge_dev <- sd(R7$value)
  R8_edge_dev <- sd(R8$value)
  R9_edge_dev <- sd(R9$value)
  R10_edge_dev <- sd(R10$value)
  R11_edge_dev <- sd(R11$value)
  R12_edge_dev <- sd(R12$value)
  R13_edge_dev <- sd(R13$value)
  R14_edge_dev <- sd(R14$value)
  R15_edge_dev <- sd(R15$value)
  R16_edge_dev <- sd(R16$value)

return<-data.frame(
    R1_edge, R1_edge_dev,
    R2_edge, R2_edge_dev,
    R3_edge, R3_edge_dev,
    R4_edge, R4_edge_dev,
    R5_edge, R5_edge_dev,
    R6_edge, R6_edge_dev,
    R7_edge, R7_edge_dev,
    R8_edge, R8_edge_dev,
    R9_edge, R9_edge_dev,
    R10_edge, R10_edge_dev,
    R11_edge, R11_edge_dev,
    R12_edge, R12_edge_dev,
    R13_edge, R13_edge_dev,
    R14_edge, R14_edge_dev,
    R15_edge, R15_edge_dev,
    R16_edge, R16_edge_dev,
    kurtosis(R1$value),
    kurtosis(R2$value),
    kurtosis(R3$value),
    kurtosis(R4$value),
    kurtosis(R5$value),
    kurtosis(R6$value),
    kurtosis(R7$value),
    kurtosis(R8$value),
    kurtosis(R9$value),
    kurtosis(R10$value),
    kurtosis(R11$value),
    kurtosis(R12$value),
    kurtosis(R13$value),
    kurtosis(R14$value),
    kurtosis(R15$value),
    kurtosis(R16$value),
    skewness(R1$value),
    skewness(R2$value),
    skewness(R3$value),
    skewness(R4$value),
    skewness(R5$value),
    skewness(R6$value),
    skewness(R7$value),
    skewness(R8$value),
    skewness(R9$value),
    skewness(R10$value),
    skewness(R11$value),
    skewness(R12$value),
    skewness(R13$value),
    skewness(R14$value),
    skewness(R15$value),
    skewness(R16$value)
  )
  edge_results <- data.frame(return)
}

symmetry_lower <- function(x) {
  library(dplyr)
  # this routine segments the image into 16 regions and calculates symmetry
  rudy2 <- magick::image_canny(x)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)

  # y axis symmetry
  top <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .5 * P)

  bottom <- ZZZZZ %>%
    dplyr::filter(y > .5 * P & y == P)

  balance <- mean(top$value) - mean(bottom$value)
  horiz <- balance
  sd_top <- sd(top$value)
  sd_bottom <- sd(bottom$value)

  # x axis symmetry
  left <- ZZZZZ %>%
    dplyr::filter(x < .5 * max(y))

  right <- ZZZZZ %>%
    dplyr::filter(x > .5)

  balance <- mean(left$value) - mean(right$value)
  vert <- balance
  sd_left <- sd(left$value)
  sd_right <- sd(right$value)

  # TRIANGLE FOLD
  T1 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5 * Q) %>%
    dplyr::filter(y > 0 & y < .5 * P)
  T2 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .25 * Q) %>%
    dplyr::filter(y > .5 * P & y < .75 * P)
  T3 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .125 * Q) %>%
    dplyr::filter(y > .75 * P & y < .875 * P)
  T4 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < .75 * max(x))  %>%
    dplyr::filter(y > 0 & y < .25 * P)
  T5 <- ZZZZZ %>%
    dplyr::filter(x > .75 * max(x) & x < .875 * max(x)) %>%
    dplyr::filter(y > 0 * y & y < .125 * P)

  # bottom right big
  T8 <- ZZZZZ %>%
    dplyr::filter(x > .5 * max(x) & x < max(x)) %>%
    dplyr::filter(y > .5 * P & y < P)
  # upper right middle
  T9 <- ZZZZZ %>%
    dplyr::filter(x > .75 * max(x) & x < max(x)) %>%
    dplyr::filter(y < .5 * P & y > .25 * P)
  # upper right small
  T10 <- ZZZZZ %>%
    dplyr::filter(x > .25 * Q & x < .5 * Q) %>%
    dplyr::filter(y > .75 * P & y < P)

  T11 <- ZZZZZ %>%
    dplyr::filter(x < .125 * Q & x < .25 * Q) %>%
    dplyr::filter(y > .875 * P & y < P)
  T12 <- ZZZZZ %>%
    dplyr::filter(x > .875 * max(x) & x < max(x)) %>%
    dplyr::filter(y > .125 * P & y < .25 * P)

  A <- mean(T1$value) - mean(T8$value)
  B <- mean(T4$value) - mean(T9$value)
  C <- mean(T2$value) - mean(T10$value)
  D <- mean(T3$value) - mean(T11$value)
  E <- mean(T5$value) - mean(T12$value)
  G <- B + C + ((D + E) / 2) / 3
  H <- A + ((B + C) / 2) + ((D + E) / 2) / 3

  symmetry <- data.frame(horiz, sd_top, sd_bottom, vert, sd_left, sd_right,
                         central_diagonal = A, corners_diagonal = G,
                         diagonal_overall = H
  )
}

#function that is recursively called by colors_analysis
lower_colors <- function(x) {
  loader <- colordistance::loadImage(x, sample.size = 5000)
  plot <- loader$filtered.rgb.2d
  plot2 <- data.frame(plot)
  mean_red <- mean(plot2$r * 255)
  deviation_red <- sd(plot2$r * 255)
  mean_blue <- mean(plot2$b * 255)
  deviation_blue <- sd(plot2$b * 255)
  mean_green <- mean(plot2$g * 255)
  deviation_green <- sd(plot2$g * 255)

  # hsv colorset - im skeptical

  plot3 <- loader$filtered.hsv.2d
  plot4 <- data.frame(plot3)
  mean_hue <- mean(plot4$h)
  deviation_hue <- sd(plot4$h)
  mean_saturation <- mean(plot4$s)
  deviation_saturation <- sd(plot4$s)
  mean_value <- mean(plot4$v)
  deviation_value <- sd(plot4$v)

  # luminance
  luminance <- (mean_red + mean_blue + mean_green) / 3
  # brightness with deviation of brightness
  lum_contrast <- (deviation_red + deviation_blue + deviation_green) / 3


  # push to global environment
  colors_results <<- data.frame(
    mean_red, deviation_red, mean_blue, deviation_blue, mean_green, deviation_green,
    mean_hue, deviation_hue, mean_saturation, deviation_saturation, mean_value, deviation_saturation, luminance, lum_contrast
  )
}
