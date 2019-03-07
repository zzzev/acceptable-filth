library(tidyverse)
library(rvest)
library(showtext)

font_add_google("Lato", "lato")
theme_update(text = element_text(family = "lato"))
showtext.auto()

raw <- read_html("https://www.fda.gov/food/guidanceregulation/ucm056174.htm")
rows <- html_nodes(raw, ".table tbody > tr")
parsed_rows <- list()
for (row in rows) {
  tds <- html_nodes(row, "td") %>% html_text
  t <- tibble(product = html_node(row, "th") %>% html_text)
  if (length(tds) == 1) {
    t <- bind_cols(t, tibble(metadata = tds[[1]]))
  } else if (length(tds) == 2) {
    t <- bind_cols(t, tibble(defect = tds[[1]], action_level = tds[[2]]))
  }
  if (ncol(t) > 1) parsed_rows <- append(parsed_rows, list(t))
}

data <- parsed_rows %>%
  map_dfr(~.x) %>%
  fill(product) %>%
  fill(metadata, .direction = "up") %>%
  filter(!is.na(defect))

raw_y_format <- scales::number_format(scale = 0.001, suffix = "K")
y_format <- function(amounts) map_chr(amounts, function(amount) {
  if (is.na(amount)) return("NA")
  if (amount == 0) return("0")
  raw_y_format(amount)
})

simplify_product <- function(products) map_chr(products, function(product) {
  if (str_detect(product,"Chocolate")) return("Chocolate")
  if (str_detect(product, "Noodle")) return("Noodles")
  if (str_detect(product, "Capsicum")) return("Capsicum, Ground")
  if (str_detect(product, "Paprika")) return("Paprika, Ground")
  if (str_detect(product, "Processed")) return("Thyme, Unground")
  product
})

product_blacklist <- c(
  "Cocoa Powder Press Cake",
  "Popcorn"
)

insect_data <- data %>%
  filter(str_detect(defect, "Insect filth"),
         str_detect(action_level, "insect fragments"),
         !(product %in% product_blacklist)) %>%
  mutate(product = simplify_product(product)) %>%
  mutate(num_fragments = str_extract(action_level,
                                     "[0-9]+ (or more )?insect fragments") %>%
                           parse_number,
         sample_size = str_extract(action_level,
                                   "[0-9]+ gram") %>% parse_number,
         allowed_per_100g = num_fragments * 100 / sample_size) %>%
  arrange(desc(allowed_per_100g))
insect_data %>%
  select(-c(defect, action_level, metadata)) %>%
  mutate(product = reorder(product, allowed_per_100g)) %>%
  ggplot(aes(x = product, y = allowed_per_100g, fill = product)) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) +
    scale_y_continuous(labels = y_format) +
    labs(x = NULL,
         y = "Max Insect Fragments per 100g",
         title = "Allowed Insect Fragments in Food",
         subtitle = "Products over these limits are considered 'adulterated' by the USDA",
         caption = "Data: USDA    -    Visualization by @zzzev") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = -20)),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "#dddddd"),
          panel.grid.minor.y = element_line(color = "#dddddd"),
          plot.title = element_text(hjust = 0.5, margin = margin(t = 2)),
          plot.subtitle = element_text(hjust = 0.5, margin = margin(b = -30, t = 4)),
          plot.caption = element_text(hjust = 0.5))

ggsave("insect_fragments.png", width = 8, height = 8, dpi = 720 / 8)
ggsave("insect_fragments.pdf", width = 8, height = 8)

rodent_data <- data %>%
  filter(str_detect(defect, "Rodent filth"),
         str_detect(action_level, "rodent hairs"),
         !(product %in% product_blacklist)) %>%
  mutate(product = simplify_product(product),
         num_hairs = str_extract(action_level,
                                     "[0-9]+ (or more )?rodent hairs") %>%
           parse_number,
         sample_size = str_extract(action_level,
                                   "[0-9]+ gram") %>% parse_number,
         allowed_per_100g = num_hairs * 100 / sample_size) %>%
  arrange(desc(allowed_per_100g))

rodent_data %>%
  select(-c(defect, action_level, metadata)) %>%
  mutate(product = reorder(product, allowed_per_100g)) %>%
  ggplot(aes(x = product, y = allowed_per_100g, fill = product)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "A", begin = 0.3, end = 0.8) +
  scale_y_continuous(breaks = 0:4 * 20, minor_breaks = 0:4 * 20 - 10) +
  labs(x = NULL,
       y = "Max Rodent Hairs per 100g",
       title = "Allowed Rodent Hairs in Food",
       subtitle = "Products over these limits are considered 'adulterated' by the USDA",
       caption = "Data: USDA    -    Visualization by @zzzev") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = -20)),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#dddddd"),
        panel.grid.minor.y = element_line(color = "#dddddd"),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 2)),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = -30, t = 4)),
        plot.caption = element_text(hjust = 0.5))

ggsave("rodent_hairs.png", width = 8, height = 8, dpi = 720 / 8)
ggsave("rodent_hairs.pdf", width = 8, height = 8)
