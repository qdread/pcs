#' Parse rider profile information from HTML code
#'
#' \code{parse_rider_profile} parses HTML code of rider's profile page
#' for personal information.
#'
#' @param rider_html HTML code of rider's profile page
#' @return Rider profile information (see \code{rider_profiles_men} documentation)
parse_rider_profile <- function(rider_html)
{
  # Rider name
  rider_name<-
    rider_html %>%
    rvest::html_nodes('h1') %>%
    rvest::html_text() 
  
  rider <- stringr::str_squish(rider_name[[1]][1])
  
  # Rider team. Currently active riders have a team name but retired riders do not
  team <- rider_html %>%
    rvest::html_nodes('h2') %>%
    rvest::html_text() 
  
  if (length(team) == 0) team <- 'none'
  
  jumbled <- rider_html %>%
    rvest::html_nodes(xpath = '/html/body/div[1]/div[1]/div[8]') %>%
    rvest::html_text()
  
  if (stringr::str_detect(jumbled, "Date of birth:")){
    dob <- regmatches(jumbled, regexec("Date of birth:\\s*(.*?)\\s*\n", jumbled))[[1]][2] %>%
      stringr::str_remove("th|nd|rd|st") %>%
      stringr::str_remove("\\([0-9]+\\)") %>%
      stringr::str_squish() %>%
      readr::parse_date(., format = "%d%B%Y")
  } else {
    dob <- NA
  }
  
  if (stringr::str_detect(jumbled, "Nationality")){
    nationality <- regmatches(jumbled, regexec("Nationality:\\s*(.*?)\\s*\n", jumbled))[[1]][2]
  } else {
    nationality <- NA
  }
  
  if (stringr::str_detect(jumbled, "Weight")){
    weight <- regmatches(jumbled, regexec("Weight:\\s*(.*?)\\s*kg", jumbled))[[1]][2]
  } else {
    weight <- NA
  }
  
  if (stringr::str_detect(jumbled, "Height")){
    height <- regmatches(jumbled, regexec("Height:\\s*(.*?)\\s*m", jumbled))[[1]][2]
  } else {
    height <- NA
  }
  
  if (stringr::str_detect(jumbled, "Place of birth:")){
    pob <- regmatches(jumbled, regexec("Place of birth:\\s*(.*?)\\s*\n", jumbled))[[1]][2]
  } else {
    pob <- NA
  }
  
  # Scores for six specialties are the first six numbers after the word Specialties
  numbers_extract <- gsub('.*.Specialties', '', jumbled) %>% 
    stringr::str_extract_all(pattern ="[0-9]+")
  six_numbers <- numbers_extract[[1]][1:6] %>%
    as.numeric() %>%
    setNames(c('Onedayraces', 'GC', 'TT', 'Sprint', 'Climber', 'Hills')) 
  
  out <- tibble::tibble(rider = rider,
                dob = dob,
                nationality = nationality,
                pob = pob,
                current_team = team,
                weight = as.numeric(weight),
                height = as.numeric(height),
                tibble::as_tibble(t(six_numbers)))
  
  return(out)
}
