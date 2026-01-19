#' Scrape file links from a CDC Vital Statistics section
#'
#' Extracts downloadable file links from a CDC Vital Statistics page section
#' identified by an anchor ID. The function navigates the HTML structure,
#' collects links from listScroll elements, and returns a tidy tibble with
#' metadata about each file.
#'
#' @param page An HTML document returned by \code{rvest::read_html()}.
#' @param anchor_id Character string giving the HTML anchor ID for the section.
#' @param section_name Human-readable name of the section.
#' @param subsection_names Character vector of subsection names. Must match
#'   the number of listScroll elements found in the section.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{section}{Section name}
#'   \item{subsection}{Subsection name}
#'   \item{link_text}{Text of the download link}
#'   \item{year}{Extracted year or leading label}
#'   \item{file_size}{File size string, if present}
#'   \item{url}{Absolute URL to the file}
#'   \item{file_type}{File extension}
#' }
#'
#' @details
#' The function assumes the CDC page structure uses \code{.listScroll}
#' containers and that the anchor is nested three levels below the section
#' root. Changes to page structure may require updating the DOM traversal.
#'
#' @importFrom rvest html_element html_elements html_text2 html_attr url_absolute
#' @importFrom xml2 xml_parent
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_extract str_remove_all
#' @importFrom purrr map2_dfr
#' @importFrom tibble tibble
#' @importFrom magrittr %>%

#'
#' @export
scrape_cdc_section <- function(page, anchor_id, section_name, subsection_names) {
    anchor <- page %>% html_element(paste0("a#", anchor_id))
    stopifnot(!is.na(anchor))

    section <- anchor %>%
        xml_parent() %>%
        xml_parent() %>%
        xml_parent()

    list_scrolls <- section %>% html_elements(".listScroll")
    stopifnot(length(list_scrolls) == length(subsection_names))

    files <- purrr::map2_dfr(list_scrolls, subsection_names, ~ {
        links <- .x %>% html_elements("a")
        tibble(
            link_text = html_text2(links),
            url = html_attr(links, "href"),
            subsection = .y
        )
    }) %>%
        filter(!is.na(url)) %>%
        mutate(
            url = url_absolute(url, "https://www.cdc.gov"),
            file_type = str_extract(url, "\\.[a-zA-Z0-9]+$"),
            section = section_name,
            year = str_extract(link_text, "^[^ ]+"),
            file_size = str_extract(link_text, "\\(([^)]+)\\)$") %>%
                str_remove_all("[()]")
        ) %>%
        select(section, subsection, link_text, year, file_size, url, file_type)

    files
}
