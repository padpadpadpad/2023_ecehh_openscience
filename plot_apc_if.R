# load in apcs and IF data and see if there is a relationship

# load in packages
librarian::shelf(tidyverse, readxl, showtext, scales)

showtext_auto()
showtext::showtext_opts(dpi = 300)
sysfonts::font_families()

font_add_google("Outfit")

apc <- read.csv('APCs.csv')

# load in data
apc <- read.csv('APCs.csv') %>%
  filter(!str_detect(apc_.lower, 'per page')) %>%
  mutate(apc = parse_number(apc_.lower)) %>%
  group_by(issn_print, parent_publisher, source_title, oa_status) %>%
  summarise(ave_apc= mean(apc, na.rm = TRUE),
            ave_n = mean(n_doi, na.rm = TRUE), .groups = 'drop') %>%
  filter(ave_n > 10)


# load in impact factor
d_if <- read_excel('2019-2023JCRImpactFactor.xlsx', col_names = TRUE) %>%
  rename(issn_print = issn) %>%
  select(-eissn) %>%
  pivot_longer(cols = 2:6, names_to = 'pub_year', values_to = 'impact_factor') %>%
  mutate(pub_year = parse_number(pub_year),
         impact_factor = parse_number(impact_factor)) %>%
  group_by(issn_print) %>%
  summarise(ave_impact_factor = mean(impact_factor, na.rm = TRUE), .groups = 'drop')

d <- left_join(apc, d_if)

ggplot(d, aes(ave_apc, log(ave_impact_factor))) +
  geom_point(shape = 21, fill = BrewerUoE::uoe_colours()[4], size = 2) +
  stat_smooth(method = 'lm', se = FALSE, col = 'black') +
  theme_bw() +
  labs(x = 'Average APC (2015-2018; £)', y = 'log10 Average Impact Factor\n(2019-2023)',
       title = 'High Impact Factor journals charge more for OA') +
  theme_bw(base_size = 16, base_family = 'Outfit')

# save plot
ggsave("plots/apc_vs_if.png", width = 10, height = 5)

ggplot(d, aes(ave_apc, log(ave_n))) +
  geom_point()

ggplot(d, aes(log10(ave_n), log(ave_impact_factor))) +
  geom_point()
