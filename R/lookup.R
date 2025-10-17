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
#' @importFrom dplyr filter mutate select n
#' @importFrom stringr str_detect str_to_lower
lookup_gaez_variable <- function(user_input) {
  if (missing(user_input) || is.null(user_input) || user_input == "") {
    stop(
      "Please provide a variable code, data code, or variable name to search for.",
      call. = FALSE
    )
  }

  # Try exact matches (case-insensitive) in variable_code and data_code
  exact_matches <- gaez_variables |>
    filter(
      toupper(variable_code) == toupper(user_input) |
      toupper(data_code) == toupper(user_input)
    )

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
#' When multiple matches are found, uses intelligent matching to select the best
#' option based on exact matches, word boundaries, and string distance.
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
#' @param interactive Logical - Whether to prompt for user input when multiple
#'   matches are found. Default is TRUE. Set to FALSE to automatically select
#'   the best match without prompting.
#'
#' @return Character string - The GAEZ crop code (e.g., "MZE" for maize in theme 4)
#'
#' @details
#' ## Match Selection Algorithm
#' When multiple crops match the input, they are ranked by quality:
#' \enumerate{
#'   \item **Exact matches** (case-insensitive): "wheat" matches "Wheat" exactly
#'   \item **Word-boundary matches**: "wheat" starts the word "Wheat" (higher priority
#'     than matching within "Buckwheat")
#'   \item **String distance**: Closest match using Levenshtein distance
#' }
#'
#' ## Interactive vs Auto-Select Mode
#' \itemize{
#'   \item **Default (interactive = TRUE)**: Prompts user to select from a numbered
#'     list when multiple matches are found
#'   \item **Auto-select (interactive = FALSE)**: Automatically selects the best match
#'     based on match quality without prompting
#' }
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
#' # Partial name match - intelligent selection with user prompt (default)
#' lookup_gaez_crop("wheat", theme = 4)  # Prompts to choose between matches
#'
#' # Auto-select best match without prompting
#' lookup_gaez_crop("wheat", theme = 4, interactive = FALSE)
#' # Auto-selects "Wheat" (exact match)
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select n arrange desc
#' @importFrom stringr str_to_upper str_to_lower str_detect
lookup_gaez_crop <- function(crop_name, theme = 4, interactive = TRUE) {
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

  # Calculate match quality for intelligent selection
  crop_name_lower <- str_to_lower(crop_name)

  name_matches <- name_matches |>
    mutate(
      # Is it an exact match (case-insensitive)?
      exact_match = str_to_lower(name) == crop_name_lower,
      # Does the input start a word in the name (word boundary)?
      word_start_match = str_detect(
        str_to_lower(name),
        paste0("\\b", crop_name_lower)
      ),
      # String distance using base R (Levenshtein distance)
      string_dist = sapply(str_to_lower(name), function(x) {
        adist(crop_name_lower, x)[1, 1]
      })
    ) |>
    # Sort by match quality: exact > word_start > string distance
    arrange(desc(exact_match), desc(word_start_match), string_dist)

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

  # Multiple matches found
  # Check if we should use interactive mode or auto-select
  # Use function parameter if provided, otherwise R session's interactive state
  use_interactive <- interactive & interactive()

  # Auto-select mode - select best match without prompting
  if (!use_interactive) {
    # Select the best match (first after sorting by quality)
    selected_crop <- name_matches[1, ]

    # Provide informative message about why this was selected
    if (selected_crop$exact_match) {
      match_reason <- "exact match"
    } else if (selected_crop$word_start_match) {
      match_reason <- paste0("best word-boundary match (distance: ", selected_crop$string_dist, ")")
    } else {
      match_reason <- paste0("closest match (distance: ", selected_crop$string_dist, ")")
    }

    message(
      paste0(
        "Auto-selected: ", selected_crop$name, " -> ", selected_crop$gaez_crop_code,
        " (", match_reason, ")"
      )
    )
    return(selected_crop$gaez_crop_code)
  }

  # Interactive mode - show options for user selection
  message(paste("Multiple crop matches found for:", crop_name))

  # Display matches without the quality scoring columns
  name_matches_display <- name_matches |>
    mutate(option = seq_len(n())) |>
    select(option, gaez_crop_code, name, gaez_crop_group)

  print(name_matches_display)

  # Prompt user for selection
  choice <- as.numeric(readline("Please enter the number of your choice: "))

  if (is.na(choice) || choice < 1 || choice > nrow(name_matches)) {
    stop("Invalid choice. Please run the function again.", call. = FALSE)
  }

  selected_crop <- name_matches[choice, ]

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
