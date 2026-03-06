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



###<Timeline & NP Application>####

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


