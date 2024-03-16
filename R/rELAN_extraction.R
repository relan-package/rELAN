#' Extract annotations from an ELAN XML file
#'
#' This function extracts annotations from an ELAN XML file and returns a data frame with adjusted time slots.
#'
#' @param file_path The file path of the ELAN XML file.
#' @param wide_format A boolean indicating whether to return the data frame in wide format (default: FALSE).
#'
#' @return A data frame containing the extracted annotations with adjusted time slots.
#'
#' @importFrom magrittr %>%
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_text xml_find_first
#' @importFrom dplyr tibble filter group_by ungroup mutate select summarise across coalesce
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' annotations_df <- extract_annotations("path/to/elan/file.eaf", wide_format = FALSE)
#'
#' @export
extract_annotations <- function(file_path, wide_format = FALSE) {
  # Read XML file
  elan_xml <- xml2::read_xml(file_path)

  # Validate input
  if (!inherits(elan_xml, "xml_document")) {
    stop("Input must be a valid ELAN XML file.")
  }

  # Extract TIME_SLOT_ID to TIME_VALUE mapping
  time_slots <- xml2::xml_find_all(elan_xml, "//TIME_ORDER/TIME_SLOT")
  time_slot_to_value <- setNames(
    sapply(time_slots, function(node) xml2::xml_attr(node, "TIME_VALUE")),
    sapply(time_slots, function(node) xml2::xml_attr(node, "TIME_SLOT_ID"))
  )

  # Process alignable annotations for time slot references
  alignable_annotations <- xml2::xml_find_all(elan_xml, "//ALIGNABLE_ANNOTATION")

  annotations <- xml2::xml_find_all(elan_xml, "//TIER/*/*")

  data <- lapply(annotations, function(node) {
    tier_node <- xml2::xml_parent(xml2::xml_parent(node))

    # Extracting TIME_SLOT_REFs
    time_slot_ref1 <- xml2::xml_attr(node, "TIME_SLOT_REF1")
    time_slot_ref2 <- xml2::xml_attr(node, "TIME_SLOT_REF2")

    # Looking up TIME_VALUES
    time_value1 <- time_slot_to_value[time_slot_ref1]
    time_value2 <- time_slot_to_value[time_slot_ref2]

    dplyr::tibble(
      LANG_REF = xml2::xml_attr(tier_node, "DEFAULT_LOCALE"),
      LINGUISTIC_TYPE_REF = xml2::xml_attr(tier_node, "LINGUISTIC_TYPE_REF"),
      PARENT_REF = xml2::xml_attr(tier_node, "PARENT_REF"),
      TIER_ID = xml2::xml_attr(tier_node, "TIER_ID"),
      ANNOTATION_ID = xml2::xml_attr(node, "ANNOTATION_ID"),
      ANNOTATION_REF = xml2::xml_attr(node, "ANNOTATION_REF"),
      PREVIOUS_ANNOTATION = xml2::xml_attr(node, "PREVIOUS_ANNOTATION"),
      TIME_SLOT_REF1 = xml2::xml_attr(node, "TIME_SLOT_REF1"),
      TIME_SLOT_REF2 = xml2::xml_attr(node, "TIME_SLOT_REF2"),
      TIME_SLOT_REF1_VALUE = as.numeric(time_value1),
      TIME_SLOT_REF2_VALUE = as.numeric(time_value2),
      ANNOTATION_VALUE = xml2::xml_text(xml2::xml_find_first(node, ".//ANNOTATION_VALUE")),
      DURATION = as.numeric(time_value2) - as.numeric(time_value1)
    )
  })

  # Preparing the data frame from the list of annotations
  annotations_df <- do.call(rbind, data) %>% dplyr::as_tibble()

  # Function to find parent time slots
  find_parent_time_slots <- function(annotation_id, df) {
    parent_annotation <- df %>% filter(ANNOTATION_ID == annotation_id)
    # Check if parent annotation exists
    if (nrow(parent_annotation) == 0) {
      return(list(TIME_SLOT_REF1_VALUE = NA, TIME_SLOT_REF2_VALUE = NA))
    } else {
      return(list(
        TIME_SLOT_REF1_VALUE = as.numeric(parent_annotation$TIME_SLOT_REF1_VALUE),
        TIME_SLOT_REF2_VALUE = as.numeric(parent_annotation$TIME_SLOT_REF2_VALUE)
      ))
    }
  }

  # Group annotations by ANNOTATION_REF and TIER_ID, and count them
  annotations_with_counts <- annotations_df %>%
    dplyr::group_by(ANNOTATION_REF, TIER_ID) %>%
    dplyr::mutate(count = n(),
                  rank = row_number()) %>% # Rank of each annotation within its group by index
    dplyr::ungroup()

  max_iterations <- 10
  iterations <- 0

  annotations_with_adjusted_time_slots <- annotations_with_counts

  while (iterations < max_iterations &&
         (any(is.na(annotations_with_adjusted_time_slots$TIME_SLOT_REF1_VALUE)) ||
          any(is.na(annotations_with_adjusted_time_slots$TIME_SLOT_REF2_VALUE)))) {

    annotations_with_adjusted_time_slots <- annotations_with_adjusted_time_slots %>%
      dplyr::rowwise() %>%
      dplyr::mutate(parent_time_slots = list(find_parent_time_slots(ANNOTATION_REF, annotations_with_adjusted_time_slots)),
             TIME_SLOT_REF1_VALUE = ifelse(is.na(TIME_SLOT_REF1_VALUE) & !is.na(parent_time_slots$TIME_SLOT_REF1_VALUE),
                                           parent_time_slots$TIME_SLOT_REF1_VALUE + (parent_time_slots$TIME_SLOT_REF2_VALUE - parent_time_slots$TIME_SLOT_REF1_VALUE) * (rank - 1) / count,
                                           TIME_SLOT_REF1_VALUE),
             TIME_SLOT_REF2_VALUE = ifelse(is.na(TIME_SLOT_REF2_VALUE) & !is.na(parent_time_slots$TIME_SLOT_REF2_VALUE),
                                           parent_time_slots$TIME_SLOT_REF1_VALUE + (parent_time_slots$TIME_SLOT_REF2_VALUE - parent_time_slots$TIME_SLOT_REF1_VALUE) * rank / count,
                                           TIME_SLOT_REF2_VALUE),
             DURATION = ifelse(is.na(TIME_SLOT_REF1_VALUE) | is.na(TIME_SLOT_REF2_VALUE),
                               NA,
                               TIME_SLOT_REF2_VALUE - TIME_SLOT_REF1_VALUE)) %>%
      dplyr::select(-parent_time_slots) %>%
      dplyr::ungroup()

    iterations <- iterations + 1
  }

  if (wide_format) {
    annotations_with_adjusted_time_slots <- annotations_with_adjusted_time_slots %>%
      tidyr::pivot_wider(
        names_from = TIER_ID,
        values_from = ANNOTATION_VALUE,
        values_fill = list(ANNOTATION_VALUE = NA)) %>%
      # Deselect columns to prepare df for row merge
      dplyr::select(-c(LANG_REF, LINGUISTIC_TYPE_REF, PARENT_REF,
                       ANNOTATION_REF, ANNOTATION_ID, PREVIOUS_ANNOTATION,
                       TIME_SLOT_REF1, TIME_SLOT_REF2)) %>%
      # Merge rows with ANNOTATION_VALUEs replacing NAs
      dplyr::group_by(TIME_SLOT_REF1_VALUE, TIME_SLOT_REF2_VALUE) %>%
      dplyr::summarise(across(tidyselect::everything(),
                              ~dplyr::coalesce(.x) %>%
                                `[`(!is.na(.)) %>%
                                `[`(1) )) %>%
      dplyr::ungroup()
  }

  return(annotations_with_adjusted_time_slots)
}
