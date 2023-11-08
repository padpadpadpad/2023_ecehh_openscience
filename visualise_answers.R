# lets make some graphs

# load in tidyverse and readxl using librarian
librarian::shelf(tidyverse, readxl, scales, padpadpadpad/BrewerUoE, showtext)

# check what fonts are available
showtext_auto()
showtext::showtext_opts(dpi = 300)
sysfonts::font_families()

# add Exeter font
# need to make sure cairo_pdf is enabled as the ggsave device for this to work: 
# see here: https://stackoverflow.com/questions/50767445/how-to-fix-failed-to-load-cairo-dll-in-r
# and here: https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/
font_add_google("Outfit")

# check fonts again
sysfonts::font_families()

#-----------------------------#
# custom ggplot2 functions ####
#-----------------------------#

# function to change text colour to white or black based on background
contrast <- function(colour) {
  out   <- rep("black", length(colour))
  light <- farver::get_channel(colour, "l", space = "hcl")
  out[light < 60] <- "white"
  out
}

autocontrast <- aes(colour = after_scale(contrast(fill)))

# change text size of geom_label to pts
pts <- function(x){
  as.numeric(grid::convertX(grid::unit(x, "points"), "mm"))
}

# custom label_wrap function
label_wrap_mod <- function (x, width){
  unlist(lapply(strwrap(x, width = width, simplify = FALSE), 
                paste0, collapse = "\n"))
}

# load in the data ECEHH
d <- read_excel("ECEHH 2023 Open Research Questions(1-11).xlsx")

# make dataframe for what people would include in a grant application ####
d_grant <- select(d, starts_with("When writing")) %>%
  mutate(id = 1:n()) %>%
  separate_rows(1, sep = ";") %>%
  rename(measure = 1) %>%
  mutate(measure = trimws(measure)) %>%
  filter(measure != "NA") %>%
  filter(measure != '')

# number of respondents
n_respond <- nrow(d)

d_grant <- group_by(d_grant, measure) %>%
  tally() %>%
  ungroup() %>%
  mutate(prop = n / n_respond) %>%
  filter(prop >0.1) %>%
  arrange(prop) %>%
  mutate(order = 1:n())

cols <- BrewerUoE::uoe_colours()[1:nrow(d_grant)]

# make plot
ggplot(d_grant, aes(forcats::fct_reorder(measure, -n), prop, fill = measure)) +
  geom_col(col = 'grey', show.legend = FALSE) + 
  geom_text(aes(label = round(prop, 2), y = prop - 0.05, !!!autocontrast),  size = MicrobioUoE::pts(11), family = 'Outfit') +
  theme_bw(base_size = 18, base_family = 'Outfit') +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank()) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_fill_manual(values = unname(cols)) +
  labs(title = 'What "metrics" would you include in a grant application?',
       x = '',
       y = 'Proportion of total respondents', 
       caption = 'Data is comprised of 11 respondents\nNote: measures <10% are not plotted') +
  ylim(c(0,1))

# save plot
ggsave("plots/metric_info.png", width = 10, height = 5)

# make dataframe for what people choose when they are publishing
d_publish <- select(d, starts_with("When you are choosing")) %>%
  mutate(id = 1:n()) %>%
  separate_rows(1, sep = ";") %>%
  rename(measure = 1) %>%
  mutate(measure = trimws(measure)) %>%
  filter(measure != "NA") %>%
  filter(measure != '') %>%
  group_by(measure) %>%
  tally() %>%
  ungroup() %>%
  mutate(prop = n / n_respond) %>%
  filter(prop >0.1) %>%
  arrange(prop) %>%
  mutate(order = 1:n())

cols <- BrewerUoE::uoe_colours()[1:nrow(d_publish)]

# make plot
ggplot(d_publish, aes(forcats::fct_reorder(measure, -n), prop, fill = measure)) +
  geom_col(col = 'grey', show.legend = FALSE) + 
  geom_text(aes(label = round(prop, 2), y = prop - 0.05, !!!autocontrast),  size = MicrobioUoE::pts(11), family = 'Outfit') +
  theme_bw(base_size = 18, base_family = 'Outfit') +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank()) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_fill_manual(values = unname(cols)) +
  labs(title = 'When you are choosing where to publish, what do you consider?',
       x = '',
       y = 'Proportion of total respondents', 
       caption = 'Data is comprised of 11 respondents\nNote: measures <10% are not plotted') +
  ylim(c(0,1))

# save plot
ggsave("plots/publishing_info.png", width = 10, height = 5)

# % of people preprinting their papers
select(d, starts_with("Do you preprint")) %>%
  group_by_all() %>%
  tally() %>%
  mutate(prop = n / sum(n))

# % of people actively making data accessible
select(d, starts_with("Do you actively make your data")) %>%
  group_by_all() %>%
  tally() %>%
  mutate(prop = n / sum(n))

# how do we make data accessible
d_datasharing <- select(d, starts_with("How do you share your data")) %>%
  mutate(id = 1:n()) %>%
  separate_rows(1, sep = ";") %>%
  rename(measure = 1) %>%
  mutate(measure = trimws(measure)) %>%
  filter(measure != "NA") %>%
  filter(measure != '') %>%
  group_by(measure) %>%
  tally() %>%
  ungroup() %>%
  mutate(prop = n / n_respond) %>%
  filter(prop >0.1) %>%
  arrange(prop) %>%
  mutate(order = 1:n())

cols <- BrewerUoE::uoe_colours()[1:nrow(d_datasharing)]

# make plot
ggplot(d_datasharing, aes(forcats::fct_reorder(measure, -n), prop, fill = measure)) +
  geom_col(col = 'grey', show.legend = FALSE) + 
  geom_text(aes(label = round(prop, 2), y = prop - 0.05, !!!autocontrast),  size = MicrobioUoE::pts(11), family = 'Outfit') +
  theme_bw(base_size = 16, base_family = 'Outfit') +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank()) +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_fill_manual(values = unname(cols)) +
  labs(title = 'How do you share your data most regularly',
       x = '',
       y = 'Proportion of total respondents', 
       caption = 'Data is comprised of 11 respondents\nNote: measures <10% are not plotted') +
  ylim(c(0,1))

# save plot
ggsave("plots/datasharing.png", width = 10, height = 5)

# % of people who share code
select(d, starts_with("Have you ever published the code")) %>%
  group_by_all() %>%
  tally() %>%
  mutate(prop = n / sum(n))
 
# have you ever pre-registered a study
select(d, starts_with("Have you ever pre-re")) %>%
  group_by_all() %>%
  tally() %>%
  mutate(prop = n / sum(n))

# are you engaged in open and reproducible science practices?
select(d, starts_with("Would you say you are engaged")) %>%
  group_by_all() %>%
  tally() %>%
  mutate(prop = n / sum(n))
