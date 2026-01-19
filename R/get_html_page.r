#' Get an HTML page from a URL
#'
#' Downloads a web page using \pkg{httr2} and returns it as an HTML document.
#' This is useful for passing directly to scraping functions like \code{scrape_cdc_section()}.
#'
#' @param url Character string containing the URL of the web page to retrieve.
#'
#' @return An HTML document of class \code{xml_document}, ready for use with
#'   \pkg{rvest} functions.
#'
#' @details
#' The function performs a GET request using \pkg{httr2}, sets a user agent
#' to mimic a browser, and parses the response into an HTML document.
#'
#' @examples
#' \dontrun{
#' page <- get_html_page("https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm")
#' births <- scrape_cdc_section(
#'     page, "Births", "birth",
#'     c("User Guide", "U.S. Data", "U.S. Territories")
#' )
#' }
#'
#' @importFrom httr2 request req_user_agent req_perform resp_body_html
#' @export
get_html_page <- function(url) {
    request(url) %>%
        req_user_agent("Mozilla/5.0 (compatible; R scraper)") %>%
        req_perform() %>%
        resp_body_html()
}
