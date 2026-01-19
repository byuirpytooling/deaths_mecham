# Tell R CMD check these are column names to avoid notes
utils::globalVariables(c("link_text", "subsection", "year", "file_size", "file_type"))

#' Scrape all CDC Vital Statistics sections
#'
#' Downloads and combines all the main CDC Vital Statistics sections
#' into a single tibble. Uses `get_html_page()` and `scrape_cdc_section()`.
#'
#' @param url Character string with the CDC Vital Stats page URL.
#'
#' @return A tibble with all file links and metadata for all sections.
#'
#' @examples
#' \dontrun{
#' all_cdc_data <- scrape_all_sections("https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm")
#' }
#'
#' @importFrom dplyr bind_rows mutate across everything
#' @export
scrape_all_sections <- function(url) {
  page <- get_html_page(url)
  
  # Define sections and their subsection names
  sections <- list(
    Births              = c("User Guide", "U.S. Data", "U.S. Territories"),
    Period_cohort       = c("User Guide", "U.S. Data", "U.S. Territories"),
    Birth_Cohort        = c("User Guide", "U.S. Data", "U.S. Territories"),
    `matched-multiple`  = c("User Guide", "U.S. Data"),  # The page has a dash on this anchor.
    Mortality_Multiple  = c("User Guide", "U.S. Data", "U.S. Territories"),
    Fetal_Death         = c("User Guide", "U.S. Data", "U.S. Territories")
  )
  
  # Scrape all sections and convert all columns to character
  all_data <- purrr::imap_dfr(sections, function(subs, anchor_id) {
    scrape_cdc_section(page, anchor_id, tolower(anchor_id), subs) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  })
  
  return(all_data)
}
