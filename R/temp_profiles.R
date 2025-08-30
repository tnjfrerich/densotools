#' Load L2H profile data
#'
#' Given a directory of L2H data files, this function collects all of the data
#' and stores it in a tibble. Unused channels are removed automatically.
#'
#' @param folder_path Path to the folder of data files. These files should end in "txt"
#' @return A tibble of the L2H profile data
#' @export
load_profile_data <- function(folder_path) {
  files <- dir(folder_path)[tools::file_ext(dir(folder_path)) == "txt"]

  # read all files, clean, and store
  data <- tibble::tibble()
  for (i in 1:length(files)) {
    profile_data <- readr::read_csv(paste0(folder_path, "\\\\", files[i]), skip = 1) %>% tibble::as_tibble()
    profile_data <- profile_data %>%
      dplyr::mutate(
        `Relative Time` = seq(from = 0, to = (nrow(profile_data) - 1) / 2, by = 0.5),
        tester = i,
      ) %>% dplyr::rename(time = `Relative Time`)

    # ignore unused channels
    good_channels <- !logical(1)
    for (j in 2:ncol(profile_data)) {
      good_channels <- append(good_channels, !any(profile_data[, j] == 1271.7))
    }
    profile_data <- profile_data[, good_channels]

    # add data to data frame
    data <- dplyr::bind_rows(
      data,
      profile_data
    )
  }

  return(data)
}
