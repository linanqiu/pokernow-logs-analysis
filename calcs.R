library(tidyverse)
library(zoo)

parse_entries <- function(entries) {
  # starting stack
  s_stacks <- entries[str_detect(entries, 'Player stacks:')]
  s_stacks <- (s_stacks %>% str_remove('Player stacks:') %>% str_split(' \\| '))[[1]] %>% str_trim()
  m_stacks <- str_match(s_stacks, '\"(.*) @ (.*)\" \\((.*)\\)')
  stacks <- tibble(name = m_stacks[, 2], id = m_stacks[, 3], stack = as.numeric(m_stacks[, 4]))
  
  # collections
  s_collects <- entries[str_detect(entries, 'collected')]
  m_collects <- str_match(s_collects, '\"(.*) @ (.*)\" collected ([0-9]+) from pot(.*)')
  collects <- tibble(name = m_collects[, 2], id = m_collects[, 3], collect = as.numeric(m_collects[, 4]))
  
  # buyins
  s_parts <- entries[str_detect(entries, 'participation')]
  m_parts <- str_match(s_parts, '.*\"(.*) @ (.*)\" participation .* ([0-9]+)')
  parts <- tibble(name = m_parts[, 2], id = m_parts[, 3], part = as.numeric(m_parts[, 4]))
  
  # stand ups
  s_stands <- entries[str_detect(entries, 'stand up with')]
  m_stands <- str_match(s_stands, '.*\"(.*) @ (.*)\" stand up with .* ([0-9]+)')
  stands <- tibble(name = m_stands[, 2], id = m_stands[, 3], stand = as.numeric(m_stands[, 4]))
  
  # sit back
  s_sitbacks <- entries[str_detect(entries, 'sit back with')]
  m_sitbacks <- str_match(s_sitbacks, '.*\"(.*) @ (.*)\" sit back with .* ([0-9]+)')
  sitbacks <- tibble(name = m_sitbacks[, 2], id = m_sitbacks[, 3], sitback = as.numeric(m_sitbacks[, 4]))
  
  # admin buyin
  s_adds <- entries[str_detect(entries, 'admin queued the stack change .* adding [0-9]+ chips')]
  m_adds <- str_match(s_adds, '.*\"(.*) @ (.*)\" adding ([0-9]+) chips')
  adds <- tibble(name = m_adds[, 2], id = m_adds[, 3], add = as.numeric(m_adds[, 4]))
  
  # quits
  s_quits <- entries[str_detect(entries, 'quits the game')]
  m_quits <- str_match(s_quits, '.*\"(.*) @ (.*)\" quits the game .* ([0-9]+)')
  quits <- tibble(name = m_quits[, 2], id = m_quits[, 3], quit = as.numeric(m_quits[, 4]))
  
  list(stacks = stacks, collects = collects, parts = parts, stands = stands, sitbacks = sitbacks, adds = adds, quits = quits)
}

calc_deltas <- function(stacks_curr, stacks_next, parts, stands, sitbacks, adds, quits) {
  if(is_null(stacks_next)) {
    return(NULL)
  }
  
  delta <- stacks_curr %>% 
    full_join(stacks_next, by = 'id', suffix = c('_curr', '_next')) %>% 
    select(-starts_with('name')) %>% 
    mutate(stack_curr = replace_na(stack_curr, 0), stack_next = replace_na(stack_next, 0)) %>% 
    mutate(delta = stack_next - stack_curr) %>% 
    # participation / buyins
    full_join(parts, by = 'id') %>% 
    select(-starts_with('name')) %>% 
    mutate(part = replace_na(part, 0)) %>% 
    mutate(delta = delta - part) %>% 
    # stand ups
    full_join(stands, by = 'id') %>% 
    select(-starts_with('name')) %>% 
    mutate(stand = replace_na(stand, 0)) %>% 
    mutate(delta = delta + stand) %>% 
    # sitbacks
    full_join(sitbacks, by = 'id') %>% 
    select(-starts_with('name')) %>% 
    mutate(sitback = replace_na(sitback, 0)) %>% 
    mutate(delta = delta - sitback) %>% 
    # adds
    full_join(adds, by = 'id') %>% 
    select(-starts_with('name')) %>% 
    mutate(add = replace_na(add, 0)) %>% 
    mutate(delta = delta - add) %>% 
    # quits
    full_join(quits, by = 'id') %>% 
    select(-starts_with('name')) %>% 
    mutate(quit = replace_na(quit, 0)) %>% 
    mutate(delta = delta + quit)
  
  delta
}