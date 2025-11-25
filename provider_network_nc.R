############################################################
# NC Medicare Provider Co-Practice Network (ZIP 27710)
# Author: Kamalakanta Gahan,Department of Population Health Sciences,Duke University
# Purpose:
#   1. Build provider co-practice network for NC using
#      CMS "Medicare Physician & Other Practitioners – by Provider" data
#   2. Identify ZIP-code provider hubs in NC
#   3. Visualize the provider network for ZIP 27710 (Duke Health hub)
#   4. Export summary tables and figures (no raw data)
############################################################

## 0. Setup ----

# Load packages
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(igraph)
library(ggplot2)

# Set working directory to folder containing the CSV
# (edit this path if needed)
setwd("~/Documents/R DATA ANALYSIS/Medicaredata")

# Input file name (do NOT upload this file to GitHub)
input_file <- "Medicare_Physician_Other_Practitioners_by_Provider_2023 2.csv"

# Create an outputs folder if you prefer to keep things tidy
if (!dir.exists("outputs")) dir.create("outputs")

## 1. Import data and keep NC providers ----

df <- read_csv(input_file,
               show_col_types = FALSE)

# Keep only columns needed for this network analysis
df_nc <- df %>%
  filter(Rndrng_Prvdr_State_Abrvtn == "NC") %>%
  select(Rndrng_NPI,
         Rndrng_Prvdr_Zip5,
         Tot_Benes)

# Clean ZIPs: keep valid 5-digit, non-"00000"
df_nc_clean <- df_nc %>%
  mutate(
    Rndrng_Prvdr_Zip5 = as.character(Rndrng_Prvdr_Zip5),
    Rndrng_Prvdr_Zip5 = ifelse(
      nchar(Rndrng_Prvdr_Zip5) == 5 & Rndrng_Prvdr_Zip5 != "00000",
      Rndrng_Prvdr_Zip5,
      NA
    )
  ) %>%
  filter(!is.na(Rndrng_Prvdr_Zip5))

## 2. ZIP-level summary: provider hubs in NC ----

zip_hubs <- df_nc_clean %>%
  group_by(Rndrng_Prvdr_Zip5) %>%
  summarise(
    n_providers = n(),
    total_benes = sum(Tot_Benes, na.rm = TRUE),
    mean_benes  = mean(Tot_Benes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_providers))

# Save top hubs and flag Duke ZIP 27710
zip_compare <- zip_hubs %>%
  mutate(is_27710 = ifelse(Rndrng_Prvdr_Zip5 == "27710", "Yes", "No")) %>%
  slice(1:10) %>%
  arrange(desc(is_27710), desc(n_providers))

write.csv(zip_compare,
          file = "outputs/zip_network_hubs_nc.csv",
          row.names = FALSE)

## 3. Build NC-wide co-practice network (by ZIP) ----
# Nodes: individual providers (NPI)
# Edges: providers in the same ZIP; edge weight = similarity in Tot_Benes

# Keep only ZIPs with >= 2 providers
zip_multi_nc <- df_nc_clean %>%
  group_by(Rndrng_Prvdr_Zip5) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 2)

df_nc_multi <- df_nc_clean %>%
  semi_join(zip_multi_nc, by = "Rndrng_Prvdr_Zip5") %>%
  mutate(Rndrng_NPI = as.character(Rndrng_NPI))

# For each ZIP, create all provider pairs and their beneficiary counts
edges_nc <- df_nc_multi %>%
  group_by(Rndrng_Prvdr_Zip5) %>%
  summarise(
    NPIs  = list(Rndrng_NPI),
    Benes = list(Tot_Benes),
    .groups = "drop"
  ) %>%
  mutate(
    pairs = map2(NPIs, Benes, ~{
      if (length(.x) < 2) return(NULL)
      
      combs <- t(combn(seq_along(.x), 2))
      data.frame(
        provider1       = .x[combs[, 1]],
        provider2       = .x[combs[, 2]],
        provider1_benes = .y[combs[, 1]],
        provider2_benes = .y[combs[, 2]]
      )
    })
  ) %>%
  unnest(pairs)

# Edge weight: inverse of beneficiary volume difference
edges_nc <- edges_nc %>%
  mutate(weight = 1 / (1 + abs(provider1_benes - provider2_benes)))

# Build igraph object for all NC providers
g_nc <- graph_from_data_frame(
  d = edges_nc %>% select(provider1, provider2, weight),
  directed = FALSE
)

## 4. Node-level centrality and ZIP-level hubs ----

degree_nc   <- degree(g_nc, mode = "all")
strength_nc <- strength(g_nc, mode = "all", weights = E(g_nc)$weight)

centrality_nc <- data.frame(
  provider = names(degree_nc),
  degree   = as.numeric(degree_nc),
  strength = as.numeric(strength_nc),
  stringsAsFactors = FALSE
)

# Attach ZIP and Tot_Benes back to nodes
centrality_nc <- centrality_nc %>%
  left_join(df_nc_multi %>%
              mutate(Rndrng_NPI = as.character(Rndrng_NPI)),
            by = c("provider" = "Rndrng_NPI"))

# ZIP-level summary using centrality
zip_hubs_centrality <- centrality_nc %>%
  group_by(Rndrng_Prvdr_Zip5) %>%
  summarise(
    n_providers  = n(),
    mean_degree  = mean(degree,   na.rm = TRUE),
    mean_strength = mean(strength, na.rm = TRUE),
    total_benes  = sum(Tot_Benes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_providers))

# (Optional) overwrite earlier hub file with centrality-enhanced table
write.csv(zip_hubs_centrality,
          file = "outputs/zip_network_hubs_nc_full.csv",
          row.names = FALSE)

## 5. Focus on ZIP 27710 (Duke Health hub) ----

zip_choice <- "27710"

df_zip <- df_nc_clean %>%
  filter(Rndrng_Prvdr_Zip5 == zip_choice) %>%
  mutate(Rndrng_NPI = as.character(Rndrng_NPI))

# All provider pairs in 27710
pairs_zip <- t(combn(df_zip$Rndrng_NPI, 2))
pairs_zip <- as.data.frame(pairs_zip, stringsAsFactors = FALSE) %>%
  rename(provider1 = V1, provider2 = V2) %>%
  left_join(df_zip %>% select(Rndrng_NPI, Tot_Benes),
            by = c("provider1" = "Rndrng_NPI")) %>%
  rename(provider1_benes = Tot_Benes) %>%
  left_join(df_zip %>% select(Rndrng_NPI, Tot_Benes),
            by = c("provider2" = "Rndrng_NPI")) %>%
  rename(provider2_benes = Tot_Benes) %>%
  mutate(weight = 1 / (1 + abs(provider1_benes - provider2_benes)))

# Build ZIP-specific graph
g_zip <- graph_from_data_frame(
  d = pairs_zip %>% select(provider1, provider2, weight),
  directed = FALSE
)

## 6. Centrality and visualization for ZIP 27710 ----

# Centrality
deg_zip <- degree(g_zip, mode = "all")
str_zip <- strength(g_zip, mode = "all", weights = E(g_zip)$weight)

nodes_zip <- data.frame(
  provider = names(deg_zip),
  degree   = as.numeric(deg_zip),
  strength = as.numeric(str_zip),
  stringsAsFactors = FALSE
) %>%
  mutate(
    rank_strength      = rank(strength, ties.method = "average"),
    strength_group_num = ntile(rank_strength, 3),
    strength_group     = factor(
      strength_group_num,
      levels = c(1, 2, 3),
      labels = c("Low", "Medium", "High")
    )
  )

# Attach to graph
V(g_zip)$degree         <- nodes_zip$degree[match(V(g_zip)$name, nodes_zip$provider)]
V(g_zip)$strength       <- nodes_zip$strength[match(V(g_zip)$name, nodes_zip$provider)]
V(g_zip)$strength_group <- nodes_zip$strength_group[match(V(g_zip)$name, nodes_zip$provider)]

# Rescale strength -> node size
str_vals <- V(g_zip)$strength
str_min  <- min(str_vals, na.rm = TRUE)
str_max  <- max(str_vals, na.rm = TRUE)
vertex_size <- 3 + 7 * (str_vals - str_min) / (str_max - str_min + 1e-6)

# Color map
col_map <- c("Low" = "lightblue", "Medium" = "steelblue", "High" = "darkblue")
vertex_color <- col_map[as.character(V(g_zip)$strength_group)]

# Layout
set.seed(27710)
layout_zip <- layout_with_fr(g_zip)

# Network plot
png("outputs/2.network_zip_27710_duke_hub.png",
    width = 1600, height = 1600, res = 220)

plot(
  g_zip,
  layout       = layout_zip,
  vertex.size  = vertex_size,
  vertex.color = vertex_color,
  vertex.label = NA,
  edge.width   = 0.2,
  edge.color   = adjustcolor("grey70", alpha.f = 0.3),
  main         = "Provider Co-Practice Network, ZIP 27710 (Duke Health Hub)"
)

legend(
  "topleft",
  legend = c("High strength", "Medium", "Low"),
  col    = c("darkblue", "steelblue", "lightblue"),
  pch    = 16,
  pt.cex = 1.5,
  bty    = "n",
  title  = "Co-practice strength"
)

dev.off()

## 7. Strength distribution histogram (ZIP 27710) ----

strength_df <- data.frame(strength = as.numeric(str_zip))

p_strength <- ggplot(strength_df, aes(x = strength)) +
  geom_histogram(binwidth = 1,
                 fill = "steelblue",
                 color = "white",
                 alpha = 0.9) +
  labs(
    title = "Strength Distribution — Provider Co-Practice Network (ZIP 27710)",
    x     = "Strength (sum of co-practice weights)",
    y     = "Frequency"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = "outputs/5.strength_distribution_zip_27710.png",
  plot     = p_strength,
  width    = 8,
  height   = 6,
  dpi      = 300
)

## 8. Degree distribution (ZIP 27710) — optional ----

degree_df <- data.frame(degree = as.numeric(deg_zip))

p_degree <- ggplot(degree_df, aes(x = degree)) +
  geom_histogram(binwidth = 5,
                 fill = "steelblue",
                 color = "white",
                 alpha = 0.9) +
  labs(
    title = "Degree Distribution — Provider Co-Practice Network (ZIP 27710)",
    x     = "Degree (number of provider co-practice ties)",
    y     = "Frequency"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = "outputs/4.degree_distribution_zip_27710.png",
  plot     = p_degree,
  width    = 8,
  height   = 6,
  dpi      = 300
)

############################################################
# End of script
############################################################
