####**Manuscript Information**####

#Coral Research in Taiwan: a systematic review to inform priorities in the Anthropocene 

#Yuting Vicky Lin1¶, Daphne Z. Hoh2, Satoh Takanori3, Crystal J. McRae4¶* and Aziz J. Mulla5,6¶

#1 Sesoko Marine Research Station, Tropical Biosphere Research Center, University of the Ryukyus, Okinawa, Japan
#2 Taiwan Biodiversity Information Facility, Biodiversity Research Centre, Academia Sinica, Taipei, Taiwan
#3 Institute of Nature and Environmental Technology, Kanazawa University, Ishikawa, Japan
#4 Department of Marine Biotechnology and Resources, National Sun Yat-sen University, Kaohsiung, Taiwan.
#5 Institute of Oceanography, National Taiwan University, Taipei 10617, Taiwan
#6 Université Côte d'Azur, CNRS, UMR 7035 ECOSEAS, Nice, France

#¶ Contributed equally
#* Corresponding author: Crystal J. McRae 

#Contact Information: 
#Address: No. 70, Lianhai Rd, Gushan District, Kaohsiung City, 804, Taiwan
#Phone: (886) 07-5252000 (extension 5031)
#Email: crystal.j.mcrae@mail.nsysu.edu.tw

############################################################################
###<Figure 1 Taiwan Map & Research Topics>####

#clear workspace
rm(list=ls())

####1) Libraries####

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

####2) Data####

####Organise dataframes####

review_data <- read_csv("review_data.csv")
coords <- read_csv("coordinates.csv")

####or attach dataframes if uploading directly####

attach(review_data)
attach(coordinates)
coords <- coordinates


review_data <- review_data %>% 
  mutate(
    sub_location  = as.character(sub_location),
    mult_location = as.character(mult_location),
    location      = as.character(location)   
  )

coords <- coords %>% 
  mutate(
    sub_location = as.character(sub_location),
    latitude     = as.numeric(latitude),
    longitude    = as.numeric(longitude)
  )

locations_long <- review_data %>%
  mutate(paper_id = row_number()) %>%
  select(paper_id, sub_location, mult_location) %>%
  pivot_longer(
    cols      = c(sub_location, mult_location),
    names_to  = "source",
    values_to = "raw_location"
  ) %>%
  filter(!is.na(raw_location)) %>%
  separate_rows(raw_location, sep = ";") %>%
  mutate(
    sub_location = str_trim(raw_location)
  ) %>%
  filter(sub_location != "", sub_location != "NA")

unique_locs <- locations_long %>%
  distinct(sub_location)

####Join with Coordinates####

loc_map <- unique_locs %>%
  left_join(coords, by = "sub_location") %>%
  filter(!is.na(latitude), !is.na(longitude))

####Create region column####

loc_map <- loc_map %>%
  mutate(
    is_offshore = review_data$location[match(sub_location, review_data$sub_location)] == "offshore island",
    
    region = case_when(
      is_offshore                           ~ "Offshore",
      latitude >= 24                        ~ "North",
      latitude <= 23                        ~ "South",
      longitude >= 121 & longitude <= 122.5 ~ "East",
      longitude <= 120.5                    ~ "West",
      TRUE                                  ~ "Other"
    ),
    region = factor(region, levels = c("North","South","East","West","Offshore","Other"))
  )

####3) Plots####

####Basemap####

world <- ne_countries(scale = "medium", returnclass = "sf")

x_min <- 112
x_max <- 126
y_min <- 9
y_max <- 28

####Locations Taiwan####

ggplot() +
  geom_sf(data = world, linewidth = 0.3) +
  coord_sf(
    xlim = c(x_min, x_max),
    ylim = c(y_min, y_max),
    expand = FALSE
  ) +
  geom_point(
    data = loc_map,
    aes(x = longitude, y = latitude, colour = region),
    size  = 0.8,
    alpha = 0.8
  ) +
  scale_colour_manual(
    name = "Region",
    values = c(
      "North"    = "black",
      "South"    = "black",
      "East"     = "black",
      "West"     = "black",
      "Offshore" = "black",
      "Other"    = "black"
    )
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "NA"
  )

####Horizontal Barchart####

####Organise####

coords_clean <- coords %>%
  select(sub_location, location) %>%
  distinct() %>%
  rename(region = location)

data_long <- review_data %>%
  mutate(
    sub_location  = ifelse(sub_location  == "", NA, sub_location),
    mult_location = ifelse(mult_location == "", NA, mult_location)
  ) %>%
  pivot_longer(
    cols      = c(sub_location, mult_location),
    names_to  = "loc_type",
    values_to = "site_raw"
  ) %>%
  filter(!is.na(site_raw)) %>%
  separate_rows(site_raw, sep = ";\\s*") %>%
  rename(sub_location_name = site_raw)

data_with_region <- data_long %>%
  left_join(coords_clean, by = c("sub_location_name" = "sub_location")) %>%
  filter(!is.na(region))

####Colours####

category_cols <- c(
  "natural_products" = "#5481b0",
  "holobiont"        = "#914b61",
  "dynamics"         = "#db3024",
  "reproduction"     = "#ec5855",
  "taxonomy"         = "#f2a6ad",
  "climate_change"   = "#f1c393",
  "other_stressor"   = "#ebc952",
  "reef_organisms"   = "#d2a34b",
  "other"            = "#ab59a2",
  "policy"           = "#8539d0",
  "disease"          = "#5b628f",
  "mpa"              = "#508b61",
  "restoration"      = "#84a490",
  "remote_sensing"   = "#bfbfbf"
)

####Records per region####

paper_counts <- data_with_region %>%
  group_by(region, category) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    region   = factor(region, levels = c("north", "south", "east", "west", "offshore_island")),
    category = factor(category, levels = names(category_cols))
  ) %>%
  complete(region, category, fill = list(n = 0)) %>%
  mutate(n = ifelse(n == 0, NA, n))   # hide zeros

####Plot####

ggplot(paper_counts, aes(x = n, y = region, fill = category)) +
  geom_col(color = "black", linewidth = 0.2, width = 0.65, na.rm = TRUE) +
  scale_fill_manual(values = category_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Number of records",
    y = "Region",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "",
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


############################################################################
###<Figure 2 Timeline & NP Application>####

#clear workspace
rm(list=ls())

####1) Libraries####

library(tidyverse)
library(forcats)
library(patchwork)

####2) Data####

review_data <- read_csv("review_data.csv")

summary(review_data)
head(review_data)

review_data$year <- as.factor(review_data$year)
review_data$category <- as.factor(review_data$category)
review_data$type <- as.factor(review_data$type)
review_data$language <- as.factor(review_data$language)
review_data$focus <- as.factor(review_data$focus)
review_data$Cfoc_1 <- as.factor(review_data$Cfoc_1)
review_data$Cfoc_2 <- as.factor(review_data$Cfoc_2)
review_data$Cfoc_3 <- as.factor(review_data$Cfoc_3)
review_data$Cmult_foc <- as.factor(review_data$Cmult_foc)
review_data$Ofoc_1 <- as.factor(review_data$Ofoc_1)
review_data$Ofoc_2 <- as.factor(review_data$Ofoc_2)
review_data$Ofoc_3 <- as.factor(review_data$Ofoc_3)
review_data$Omult_foc <- as.factor(review_data$Omult_foc)
review_data$location <- as.factor(review_data$location)
review_data$sub_location <- as.factor(review_data$sub_location)
review_data$mult_location <- as.factor(review_data$mult_location)
review_data$international <- as.factor(review_data$international)
review_data$life_history_stage <- as.factor(review_data$life_history_stage)
review_data$study_duration <- as.factor(review_data$study_duration)
review_data$experiment_type <- as.factor(review_data$experiment_type)
review_data$nat_product_benefit <- as.factor(review_data$nat_product_benefit)
review_data$recommendation <- as.factor(review_data$recommendation)
review_data$late_exclusion <- as.factor(review_data$late_exclusion)
review_data$late_exclusion_reason <- as.factor(review_data$late_exclusion_reason)

summary(review_data)
summary(review_data$year)
summary(review_data$category, n = Inf)
summary(review_data$type, n = Inf)
summary(review_data$language)
summary(review_data$focus, n = Inf)
summary(review_data$location, n = Inf)
summary(review_data$international, n = Inf)
summary(review_data$life_history_stage, n = Inf)
summary(review_data$study_duration, n = Inf)
summary(review_data$experiment_type, n = Inf)
summary(review_data$nat_product_benefit, n = Inf)
summary(review_data$recommendation, n = Inf)
summary(review_data$late_exclusion, n = Inf)
summary(review_data$sub_location, n = Inf)
head(review_data)


####3) Plots####

####*Year Plot####

#by year
review_data_year <-
  review_data%>%
  group_by(year, category)%>%
  summarise(n=n())

year_plot <-
  ggplot(review_data_year, aes(x=year, y=n, fill = category)) + 
  geom_bar(stat = "identity")+
  labs(y = "Number of Studies", x="Year")+
  theme_classic()+
  scale_y_continuous(limits=c(0, 70), breaks=c(10, 20, 30, 40, 50, 60, 70))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(text=element_text(size=16,  family="sans"))

year_plot


#compute totals and create ordered factor: most abundant first
levels_order <- review_data_year %>%
  group_by(category) %>%
  summarise(total = sum(n, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  pull(category)

#check levels order
print(levels_order)

#apply ordering to the data (most abundant becomes the first factor level)
review_data_year <- review_data_year %>%
  mutate(category = factor(category, levels = levels_order))

#to ensure line in the correct place at 2000
x_2000 <- which(levels(review_data_year$year) == "2000") - 0.5

#plot the data

year_plot <- 
  ggplot(review_data_year, aes(x = year, y = n, fill = category)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = x_2000, linetype = "dashed", linewidth = 0.4, colour = "grey40", alpha = 0.6) +
  labs(y = "Number of Papers", x = "Year", title = "", fill = "Research Topic") +
  guides(fill = guide_legend(reverse = FALSE)) +
  theme_classic() +
  scale_fill_discrete(labels = function(x) gsub("\\bMpa\\b", "MPA", tools::toTitleCase(gsub("_", " ", x)))) +
  scale_y_continuous(limits = c(0, 70), breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        text = element_text(size = 16, family = "sans"),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))


year_plot


#create a 14-color gradient 
my_colours <- colorRampPalette(c("steelblue", "red", "pink", "gold2", "purple","seagreen", "gray" ))(14)

#check the colours
my_colours

#visually check colours
barplot(rep(1,14), col=my_colours, border=NA)

#apply colours to plot
year_plot_colour <- 
  year_plot +
  scale_fill_manual(
    values = my_colours,
    labels = function(x) gsub("\\bMpa\\b", "MPA", tools::toTitleCase(gsub("_", " ", x))))

year_plot_colour

ggsave("Timeline_Plot.png", plot = year_plot_colour, width = 10, height = 5, dpi = 300)
ggsave("Timeline_Plot.tiff", plot = year_plot_colour, width = 10, height = 5, dpi = 300)
ggsave("Timeline_Plot.pdf", plot = year_plot_colour, width = 10, height = 5, dpi = 300)


####*Category Plot####

#by category
review_data_cat <-
  review_data%>%
  group_by(category)%>%
  summarise(n=n())

review_data_cat <- 
  review_data_cat %>%
  mutate(percentage = n/sum(n)*100)

#clean and reorder categories as a factor
review_data_cat <- review_data_cat %>%
  mutate(    category_clean = str_to_title(str_replace_all(category, "_", " ")),
    category_clean = str_replace_all(category_clean, "\\bMpa\\b", "MPA"),
    category_clean = fct_reorder(category_clean, percentage, .desc = FALSE))

# Plot
cat_plot <-
  ggplot(review_data_cat, aes(
    x = category_clean,  # use the cleaned factor
    y = percentage,
    color = category_clean
  )) +
  geom_segment(aes(xend = category_clean, y = 0, yend = percentage), color = "black") +
  geom_point(size = 5, alpha = 1) +
  theme_classic() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    text = element_text(size = 16, family = "sans"),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))+
  labs(y = "Percentage of Studies", x = "Research Topic", title ="") +
  scale_color_manual(values = rev(my_colours))

cat_plot


ggsave("Category_Plot.tiff", plot = cat_plot, width = 10, height = 5, dpi = 300)
ggsave("Category_Plot.png", plot = cat_plot, width = 10, height = 5, dpi = 300)
ggsave("Category_Plot.pdf", plot = cat_plot, width = 10, height = 5, dpi = 300)


####*NP Application Plot####

review_data_NP <-
  review_data%>%
  filter(category =="natural_products")


review_data_NP_noNA <- 
  review_data_NP %>%
  filter(!is.na(nat_product_benefit))

#split the benefits into separate rows by ";"
review_data_NP_expanded <- 
  review_data_NP_noNA %>%
  separate_rows(nat_product_benefit, sep = ";") %>%
  mutate(nat_product_benefit = trimws(nat_product_benefit))

review_data_NP_expanded

review_data_NP_expanded$nat_product_benefit <- as.factor(review_data_NP_expanded$nat_product_benefit)
summary(review_data_NP_expanded$nat_product_benefit, n = Inf)

benefit_counts <- 
  review_data_NP_expanded %>%
  count(nat_product_benefit, sort = TRUE)

#clean labels
benefit_counts <- 
  benefit_counts %>%
  mutate(nat_product_benefit_clean = str_to_title(str_replace_all(nat_product_benefit, "_", " ")))

NP_benefit <-
  ggplot(benefit_counts, aes(x = reorder(nat_product_benefit_clean, n), y = n)) +
  geom_col(fill = "black") +
  coord_flip() +
  labs(
    x = "Natural Product Application",
    y = "Number of Mentions",
    title = ""
  ) +
  theme_classic(base_size = 14)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      text = element_text(size = 16, family = "sans"),
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"))

NP_benefit

ggsave("NP_application.tiff", plot = NP_benefit, width = 10, height = 5, dpi = 300)
ggsave("NP_application.png", plot = NP_benefit, width = 10, height = 5, dpi = 300)
ggsave("NP_application.pdf", plot = NP_benefit, width = 10, height = 5, dpi = 300)


### species analysis
# import and compile data
species_data <- read_csv("coralsp_list.csv")
species_data <- data.frame(species_data$updated_names,species_data$coral_type)
colnames(species_data) <- c("updated_names", "coral_type")
head(species_data)
species_data <- rbind(species_data[species_data$coral_type == "hard_coral",],
                      species_data[species_data$coral_type == "octocoral",])

# number of hard and soft corals species studied
species1 <- paste(species_data$updated_names,species_data$coral_type)
species1_uni <- unique(species1)
split_mat <- as.data.frame(do.call(rbind, strsplit(species1_uni, " "))[,c(1:2)] )
colnames(split_mat) <- c("species", "coral_type")
sum(split_mat$coral_type == "hard_coral")
sum(split_mat$coral_type == "octocoral")
unique(split_mat$coral_type)

# times of hard and soft corals species mentioned
species_freq <- as.data.frame(sort(table(species_data$updated_names),dec = T)) 
sum(species_freq$Freq)
cate <- species_data$coral_type
table(cate)

# Top 10 most frequently mentioned coral taxa
as.data.frame(sort(table(species1),dec = T))[c(1:10),]

############################################################################
###<Figure 3 Research Topic by Coral Type>####

####1) Libraries####

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

###Colour palette
type_cols <- c(
  "scleractinian" = "#405791",
  "octocoral"     = "#7F4A8A",
  "mix"           = "#7D253E"
)

###Clean data
df <- review_data %>%
  mutate(
    type = ifelse(type %in% c("mix", "octocoral", "scleractinian"),
                  type, NA_character_)
  ) %>%
  filter(!is.na(type), !is.na(category))

category_levels <- sort(unique(df$category))

make_plot <- function(type_name, x_max, show_y = TRUE) {
  
  df_type <- df %>%
    filter(type == type_name) %>%
    count(category, name = "n") %>%
    complete(category = category_levels, fill = list(n = 0)) %>%
    mutate(
      category = factor(category, levels = category_levels),
      point_col = ifelse(n == 0, "grey75", type_cols[type_name])
    )
  
  p <- ggplot(df_type, aes(x = n, y = category)) +
    geom_segment(
      aes(x = 0, xend = n, yend = category),
      color = type_cols[type_name],
      linewidth = 0.8
    ) +
    geom_point(
      aes(color = point_col),
      size = 3,
      show.legend = FALSE
    ) +
    scale_color_identity() +
    coord_cartesian(xlim = c(0, x_max)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "Number of papers",
      y = "",
      title = type_name
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major.y = element_blank()
    )
  
  if (!show_y) {
    p <- p +
      theme(
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  
  p
}

####Fig. 3 Coral Types bgy Research Topic####

###plots
p_scler <- make_plot("scleractinian", 125, show_y = TRUE)
p_octo  <- make_plot("octocoral", 600, show_y = FALSE)
p_mix   <- make_plot("mix", 30, show_y = FALSE)

## Combine
p_scler | p_octo | p_mix

###Single Plots

library(dplyr)
library(tidyr)
library(ggplot2)

type_cols <- c(
  "scleractinian" = "#405791",
  "octocoral"     = "#7F4A8A",
  "mix"           = "#7D253E"
)

df <- review_data %>%
  mutate(
    type = ifelse(type %in% c("mix", "octocoral", "scleractinian"),
                  type, NA_character_)
  ) %>%
  filter(!is.na(type), !is.na(category))

category_levels <- sort(unique(df$category))

make_plot <- function(type_name, x_max) {
  
  df_type <- df %>%
    filter(type == type_name) %>%
    count(category, name = "n") %>%
    complete(category = category_levels, fill = list(n = 0)) %>%
    mutate(
      category = factor(category, levels = category_levels),
      point_col = ifelse(n == 0, "grey75", type_cols[type_name])
    )
  
  ggplot(df_type, aes(x = n, y = category)) +
    geom_segment(
      aes(x = 0, xend = n, yend = category),
      color = type_cols[type_name],
      linewidth = 0.8
    ) +
    geom_point(
      aes(color = point_col),
      size = 3,
      show.legend = FALSE
    ) +
    scale_color_identity() +
    coord_cartesian(xlim = c(0, x_max)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "Number of papers",
      y = "Category",
      title = type_name
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major.y = element_blank()
    )
}

make_plot("scleractinian", 600)
make_plot("octocoral", 600)
make_plot("mix", 600)

###Table###
table_type_category <- review_data %>%
  filter(type %in% c("mix", "octocoral", "scleractinian"),
         !is.na(category)) %>%
  count(category, type, name = "n") %>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
  arrange(category)

table_type_category


############################################################################
###<Figure 4 Chord diagram>####

library(dplyr)
library(circlize)


dat <- review_data

if (!"life_history_stage" %in% names(dat) && "life_history_sta" %in% names(dat)) {
  dat <- dat %>% rename(life_history_stage = life_history_sta)
}


dat <- dat %>%
  mutate(
    life_history_stage = ifelse(life_history_stage == "undefined",
                                "undefined_life", life_history_stage),
    study_duration     = ifelse(study_duration == "undefined",
                                "undefined_duration", study_duration)
  )

###Fixed ordering
type_levels_fixed <- c("scleractinian", "octocoral", "mix")
exp_levels_fixed  <- c("interview", "computational", "mixed", "field", "laboratory")
life_levels_fixed <- c("undefined_life", "fossil", "planktonic",
                       "juvenile", "multiple", "adult")
dur_levels_fixed  <- c("undefined_duration", "short", "mid", "long")

#Apply factors
dat <- dat %>%
  mutate(
    type               = factor(type,               levels = type_levels_fixed),
    experiment_type    = factor(experiment_type,    levels = exp_levels_fixed),
    life_history_stage = factor(life_history_stage, levels = life_levels_fixed),
    study_duration     = factor(study_duration,     levels = dur_levels_fixed)
  )

###Organise long_format

all_links <- bind_rows(
  dat %>% select(type, target = experiment_type),
  dat %>% select(type, target = life_history_stage),
  dat %>% select(type, target = study_duration)
) %>%
  filter(!is.na(type), !is.na(target)) %>%
  count(type, target, name = "value")   


papers_per_type <- dat %>%
  filter(!is.na(type)) %>%
  count(type, name = "n_papers")


target_counts <- all_links %>%
  group_by(target) %>%
  summarise(n_links = sum(value), .groups = "drop")


sector_counts <- c(
  setNames(papers_per_type$n_papers, as.character(papers_per_type$type)),
  setNames(target_counts$n_links, as.character(target_counts$target))
)


links_scaled <- all_links %>%
  left_join(papers_per_type, by = "type") %>%
  group_by(type) %>%
  mutate(
    row_total     = sum(value),
    scale_factor  = n_papers / row_total,
    value_scaled  = value * scale_factor
  ) %>%
  ungroup()

###Matrix for plotting

mat <- xtabs(value_scaled ~ type + target, data = links_scaled)

###Colour Palette 

grid_col <- c(
  #Types
  scleractinian = "#5d72a2",
  octocoral     = "#94669c",
  mix           = "#92475c",
  
  #Experiment types
  interview     = "yellow3",
  computational = "yellow3",
  mixed         = "yellow3",
  field         = "yellow3",
  laboratory    = "yellow3",
  
  #Life-history stages
  undefined_life = "orange3",
  fossil         = "orange3",
  planktonic     = "orange3",
  juvenile       = "orange3",
  multiple       = "orange3",
  adult          = "orange3",
  
  #Study duration
  undefined_duration = "red3",
  short              = "red3",
  mid                = "red3",
  long               = "red3"
)

###Ordering Sectors 

right_order  <- c(exp_levels_fixed, life_levels_fixed, dur_levels_fixed)
sector_order <- c(type_levels_fixed, right_order)

gap_left  <- rep(2, length(type_levels_fixed) - 1)
gap_right <- rep(2, length(right_order) - 1)
gap_after <- c(gap_left, 18, gap_right, 18)

###Plot Unified Chord 

circos.clear()
circos.par(
  start.degree = 180,
  gap.after    = gap_after,
  track.margin = c(0.02, 0.02)
)

max_val <- max(mat)
link_lwd_mat <- 2 * sqrt(mat / max_val)

chordDiagram(
  x           = mat,
  order       = sector_order,
  grid.col    = grid_col,
  transparency = 0.35,
  link.sort   = TRUE,
  link.largest.ontop = TRUE,
  annotationTrack    = c("grid"),
  preAllocateTracks  = list(track.height = 0.11),
  link.lwd     = link_lwd_mat
)

title("", cex.main = 1.4, font.main = 2)

circos.trackPlotRegion(
  track.index = get.current.track.index(),
  bg.border = NA,
  panel.fun = function(x, y) {
    name  <- get.cell.meta.data("sector.index")
    xlim  <- get.cell.meta.data("xlim")
    ylim  <- get.cell.meta.data("ylim")
    
    # Look up count for this sector
    this_n <- sector_counts[name]
    label_txt <- if (!is.na(this_n)) {
      paste0(name, " (", this_n, ")")
    } else {
      name
    }
    
    circos.text(
      x = mean(xlim),
      y = ylim[1] + diff(ylim) * 0.9,   # inside the track
      labels = label_txt,
      cex = 0.6,
      facing = "clockwise",
      niceFacing = TRUE
    )
  }
)

circos.clear()












