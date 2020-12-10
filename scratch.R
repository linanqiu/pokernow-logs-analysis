library(tidyverse)
library(ggrepel)
library(zoo)

d <- read_csv('logs.csv')

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

d_hands <- d %>% 
  arrange(order) %>% 
  mutate(hand = map_dbl(entry, function(e) {
    if(str_detect(e, '-- starting hand')) {
      m <- str_match(e, '-- starting hand #([0-9]+)')
      hand <- as.numeric(m[, 2])
      hand
    } else {
      NA
    }
  })) %>% 
  mutate(hand = na.locf0(hand)) %>% 
  drop_na() %>% 
  group_by(hand) %>% 
  nest(data = -hand)

d_parsed <- d_hands %>% 
  mutate(parsed = map(data, function(d) {
    parse_entries(d$entry)
  })) %>% 
  select(-data) %>% 
  unnest_wider(parsed)
d_parsed$stacks_curr <- d_parsed$stacks
d_parsed$stacks_next <- lead(d_parsed$stacks)
d_parsed

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

d_deltas <- d_parsed %>% 
  mutate(deltas = pmap(list(stacks_curr, stacks_next, parts, stands, sitbacks, adds, quits), calc_deltas)) %>% 
  mutate(delta_sum = map_dbl(deltas, function(d) sum(d$delta)))

names_id <- d_deltas %>% 
  ungroup() %>% 
  select(stacks) %>% 
  unnest(stacks) %>% 
  select(name, id) %>% 
  unique()

id_names <- names_id %>% 
  group_by(id) %>% 
  summarize(names = str_c(name, collapse = ' / '))
id_names_list <- setNames(as.list(id_names$names), id_names$id)

ids_to_names <- function(ids) {
  Vectorize(function(id) id_names_list[[id]])(ids)
}

deltas <- d_deltas %>% 
  select(hand, deltas, delta_sum) %>% 
  unnest(deltas) %>% 
  select(hand, id, delta) %>% 
  mutate(name = ids_to_names(id)) %>% 
  select(-id) %>% 
  ungroup()
deltas

levels <- deltas %>% 
  pivot_wider(names_from = name, values_from = delta, values_fill = 0) %>% 
  mutate_at(vars(-hand), cumsum)
levels

levels %>% pivot_longer(-hand) %>% group_by(name) %>% summarize(last(value))

levels %>% 
  pivot_longer(cols = -hand) %>% 
  mutate(label = if_else(hand == max(hand), name, NA_character_)) %>% 
  ggplot(mapping = aes(x = hand, y = value, color = name)) +
  geom_line() +
  geom_label_repel(aes(label = label), nudge_x = 1, alpha = 0.5, na.rm = TRUE) +
  ggtitle('profit / loss') +
  xlab('hand') +
  ylab('winnings') +
  theme_bw()

deltas %>% 
  group_by(name) %>% 
  summarize(mean(delta), sd(delta))


deltas



tib <- tibble(
  `Total Hands` = max(d_deltas$hand) %>% as.character(),
  `Start Time` = min(d$at) %>% as.character(),
  `End Time` = max(d$at) %>% as.character()
)
tib <- as_tibble(cbind(nms = names(tib), t(tib)))
colnames(tib) <- c('Name', 'Value')
tib




deltas_test <- d_deltas$deltas[[11]] %>% select(id, delta)


calc_delta_transfer <- function(d) {
  d <- d %>% select(id, delta)
  deltas_pos <- d %>% filter(delta > 0) %>% mutate(delta_prop = delta / sum(delta)) %>% rename(to = id) %>% select(-delta)
  deltas_neg <- d %>% filter(delta <= 0) %>% rename(from = id)
  deltas_neg %>% 
    mutate(to = map(from, function(from) { deltas_pos })) %>% 
    unnest(to) %>% 
    mutate(delta = -1 * delta * delta_prop) %>% 
    select(-delta_prop) %>% 
    filter(delta != 0)
}

d_trans <- d_deltas %>% 
  filter(!map_lgl(deltas, is.null)) %>% 
  mutate(transfers = map(deltas, calc_delta_transfer))
d_trans <- d_trans %>% select(transfers)
transfers <- d_trans$transfers %>% 
  bind_rows() %>% 
  group_by(from, to) %>% 
  summarize(delta = sum(delta)) %>% 
  mutate(from = ids_to_names(from), to = ids_to_names(to)) %>% 
  ungroup() %>% 
  arrange(from, to)

transfers

library(hrbrthemes)

transfers %>% 
  ggplot(mapping = aes(x = to, y = from, fill = delta)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdOrYl") +
  geom_text(aes(label = round(delta, 0)))


library(igraph)

adjl <- transfers %>% mutate(weight = delta) 

g <- graph_from_data_frame(adjl)
g

p <- page_rank(g)
p
p$vector
p$value




transfers

id_names_gender <- id_names %>% 
  mutate(gender = map_chr(names, function(n) {
    if(n %in% c('Jen', 'CreamPuff', 'Carin')) {
      'F'
    } else {
      'M'
    }
  }))

name_to_gender <- function(n) {
  id_names_gender[id_names_gender$names == n, ]$gender[1]
}

transfers %>% 
  mutate(from_g = map_chr(from, name_to_gender), to_g = map_chr(to, name_to_gender)) %>% 
  select(-c(from, to)) %>% 
  group_by(from_g, to_g) %>% 
  summarize(delta = sum(delta) / n())



deltas_w <- deltas %>% pivot_wider(names_from = name, values_from = delta)
deltas_w[deltas_w$Gavyn < 0, ] %>% 
  summarize_at(vars(-hand), function(x) sum(x, na.rm = TRUE))

deltas_w %>% 
  ggplot(mapping = aes(x = Jen, y = Tim)) +
  geom_point()

library(corrr)

deltas_w[, c(2:length(deltas_w))] %>% correlate(use = 'pairwise.complete.obs') %>% 
  pivot_longer(cols = -term) %>% 
  rename(x = term, y = name) %>% 
  ggplot(mapping = aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdOrYl") +
  geom_text(aes(label = round(value, 3)))
