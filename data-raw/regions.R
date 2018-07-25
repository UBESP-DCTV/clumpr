italian_region <- xml2::read_html('https://it.wikipedia.org/wiki/Regioni_d%27Italia') %>%
  xml2::xml_find_all('.//table') %>%
  rvest::html_table(fill = TRUE) %>%
  .[[4]] %>%
  tibble::as_tibble() %>%
  dplyr::filter(Regione != 'Italia') %>%
  .[['Regione']] %>%
  stringr::str_replace_all('\\[[0-9]\\]', '') %>%
  stringr::str_to_lower() %>%
  sort()


regions <- list(
  italy = italian_region
)
use_data(regions, overwrite = TRUE)
