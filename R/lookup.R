#' Look up GAEZ v5 variable information
#'
#' Searches for GAEZ v5 variables by code or name. Supports exact matching on
#' variable codes and data codes, as well as fuzzy matching on variable names.
#' When multiple matches are found in non-interactive mode, the first match is
#' automatically selected.
#'
#' @param user_input Character string - Variable code, data code, or variable name
#'   to search for. Case-insensitive for name searches.
#'
#' @return A tibble with one row containing variable information with columns:
#'   \itemize{
#'     \item \code{theme_number}: Theme number (1-6)
#'     \item \code{theme_name}: Descriptive theme name
#'     \item \code{sub_theme_name}: Sub-theme classification
#'     \item \code{variable_code}: Variable code (e.g., "RES05-YX")
#'     \item \code{variable_name}: Full variable name/description
#'     \item \code{data_folder}: Folder location ("MAP" or "MAPSET")
#'     \item \code{data_code}: Data code used in file naming
#'   }
#'
#' @examples
#' \dontrun{
#' # Exact match by variable code
#' lookup_gaez_variable("RES05-YX")
#'
#' # Exact match by data code
#' lookup_gaez_variable("RES05-YXX")
#'
#' # Fuzzy search by name
#' lookup_gaez_variable("yield")
#' lookup_gaez_variable("temperature")
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_detect str_to_lower
lookup_gaez_variable <- function(user_input) {
  if (missing(user_input) || is.null(user_input) || user_input == "") {
    stop(
      "Please provide a variable code, data code, or variable name to search for.",
      call. = FALSE
    )
  }

  # First, try exact matches in variable_code and data_code columns
  exact_matches <- gaez_variables |>
    filter(variable_code == user_input | data_code == user_input)

  if (nrow(exact_matches) == 1) {
    message(paste("Found exact match for variable code:", user_input))
    return(exact_matches)
  }

  if (nrow(exact_matches) > 1) {
    message("Multiple exact matches found in variable/data codes:")
    exact_matches |>
      mutate(option = seq_len(n())) |>
      select(option, variable_code, variable_name) |>
      print()

    # For automated testing or non-interactive mode, select the first match
    if (interactive() && !isTRUE(getOption("gaez_testing_mode", FALSE))) {
      choice <- as.numeric(readline("Please enter the number of your choice: "))

      if (is.na(choice) || choice < 1 || choice > nrow(exact_matches)) {
        stop("Invalid choice. Please run the function again.", call. = FALSE)
      }

      return(exact_matches[choice, ])
    } else {
      message("Non-interactive mode: selecting first match")
      return(exact_matches[1, ])
    }
  }

  # If no exact matches, search in variable_name column (case-insensitive, partial matches)
  name_matches <- gaez_variables |>
    filter(str_detect(str_to_lower(variable_name), str_to_lower(user_input)))

  if (nrow(name_matches) == 0) {
    stop(
      paste(
        "No matches found for:",
        user_input,
        "\nTry searching for terms like 'yield', 'suitability', 'temperature', etc."
      ),
      call. = FALSE
    )
  }

  if (nrow(name_matches) == 1) {
    message(paste("Found match in variable names for:", user_input))
    return(name_matches)
  }

  # Multiple matches in variable names
  message(paste("Multiple matches found for:", user_input))
  name_matches |>
    mutate(option = seq_len(n())) |>
    select(option, variable_code, variable_name) |>
    print()

  # For automated testing or non-interactive mode, select the first match
  if (interactive() && !isTRUE(getOption("gaez_testing_mode", FALSE))) {
    choice <- as.numeric(readline("Please enter the number of your choice: "))

    if (is.na(choice) || choice < 1 || choice > nrow(name_matches)) {
      stop("Invalid choice. Please run the function again.", call. = FALSE)
    }

    return(name_matches[choice, ])
  } else {
    message("Non-interactive mode: selecting first match")
    return(name_matches[1, ])
  }
}


#' Look up GAEZ v5 crop codes
#'
#' Searches for GAEZ v5 crop codes by crop name or code. Different GAEZ themes
#' use different crop code systems (3-letter vs 4-letter codes). This function
#' searches within the specified theme and returns the appropriate crop code.
#'
#' @param crop_name Character string - Crop name or code to search for.
#'   Case-insensitive. Supports partial matching on crop names.
#' @param theme Numeric - GAEZ theme number (3, 4, 5, or 6). Default is 4.
#'   \itemize{
#'     \item Theme 3: Module II (4-letter codes, e.g., "WHEA", "MAIZ")
#'     \item Theme 4: Module V (3-letter codes, e.g., "WHE", "MZE")
#'     \item Theme 5: Module VI - Actual yields (3-letter codes)
#'     \item Theme 6: Module VI - Yield gaps (3-letter codes)
#'   }
#'
#' @return Character string - The GAEZ crop code (e.g., "MZE" for maize in theme 4)
#'
#' @examples
#' \dontrun{
#' # Look up maize in theme 4 (3-letter code)
#' lookup_gaez_crop("maize", theme = 4)  # Returns "MZE"
#'
#' # Look up maize in theme 3 (4-letter code)
#' lookup_gaez_crop("maize", theme = 3)  # Returns "MAIZ"
#'
#' # Exact code match
#' lookup_gaez_crop("MZE", theme = 4)
#'
#' # Partial name match
#' lookup_gaez_crop("pearl", theme = 4)  # Finds "Pearl millet"
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_to_upper str_to_lower str_detect
lookup_gaez_crop <- function(crop_name, theme = 4) {
  if (missing(crop_name) || is.null(crop_name) || crop_name == "") {
    stop("Please provide a crop name to search for.", call. = FALSE)
  }

  # Validate theme
  if (!theme %in% c(3, 4, 5, 6)) {
    stop("Theme must be 3, 4, 5, or 6", call. = FALSE)
  }

  # Use only the relevant crop table based on theme
  crop_table <- gaez_crops |>
    filter(gaez_theme == theme)

  if (nrow(crop_table) == 0) {
    stop(paste("No crops found for theme", theme), call. = FALSE)
  }

  # First, try exact match in crop code
  code_matches <- crop_table |>
    filter(str_to_upper(gaez_crop_code) == str_to_upper(crop_name))

  if (nrow(code_matches) == 1) {
    message(paste("Found exact crop code match:", code_matches$gaez_crop_code))
    return(code_matches$gaez_crop_code)
  }

  # Search in crop names (case-insensitive, partial matches)
  name_matches <- crop_table |>
    filter(str_detect(str_to_lower(name), str_to_lower(crop_name)))

  if (nrow(name_matches) == 0) {
    stop(
      paste(
        "No crop matches found for:",
        crop_name,
        "\nUse list_gaez_crops(theme =", theme, ") to see all available crops."
      ),
      call. = FALSE
    )
  }

  if (nrow(name_matches) == 1) {
    message(
      paste(
        "Found crop match:",
        name_matches$name,
        "->",
        name_matches$gaez_crop_code
      )
    )
    return(name_matches$gaez_crop_code)
  }

  # Multiple matches - show options
  message(paste("Multiple crop matches found for:", crop_name))
  name_matches |>
    mutate(option = seq_len(n())) |>
    select(option, gaez_crop_code, name, gaez_crop_group) |>
    print()

  # For automated testing or non-interactive mode, select the first match
  if (interactive() && !isTRUE(getOption("gaez_testing_mode", FALSE))) {
    choice <- as.numeric(readline("Please enter the number of your choice: "))

    if (is.na(choice) || choice < 1 || choice > nrow(name_matches)) {
      stop("Invalid choice. Please run the function again.", call. = FALSE)
    }

    selected_crop <- name_matches[choice, ]
  } else {
    message("Non-interactive mode: selecting first crop match")
    selected_crop <- name_matches[1, ]
  }

  message(
    paste(
      "Selected:",
      selected_crop$name,
      "->",
      selected_crop$gaez_crop_code
    )
  )
  return(selected_crop$gaez_crop_code)
}
