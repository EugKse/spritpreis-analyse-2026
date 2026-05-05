# Spritpreis-Check 2026 // Eugeny Ksenzov
library(tidyverse)
library(httr2)

# --- Config & API Setup ---
API_KEY <- Sys.getenv("TANKERKOENIG_KEY")

# Fokus auf die Top 5 Metropolen
cities <- tribble(
  ~stadt,      ~lat,   ~lng,
  "Berlin",    52.52,  13.40,
  "Hamburg",   53.55,  9.99,
  "Muenchen",  48.14,  11.58,
  "Koeln",     50.94,  6.96,
  "Frankfurt", 50.11,  8.68
)

# --- Logic ---

get_prices <- function(stadt, lat, lng) {
  message("Fetching: ", stadt)
  
  req <- request("https://creativecommons.tankerkoenig.de/json/list.php") %>%
    req_url_query(
      lat = lat, lng = lng, rad = 5, 
      sort = "dist", type = "all", apikey = API_KEY
    )
  
  resp <- req %>% req_perform() %>% resp_body_json()
  
  # Data cleaning
  # Wichtig: 'stadt' Argument muss mit Spalte in 'cities' matchen
  resp$stations %>% 
    enframe(name = NULL) %>% 
    unnest_wider(value) %>%
    mutate(
      city = stadt, 
      timestamp = Sys.time()
    )
}

# --- Execution ---

if (API_KEY == "") stop("API Key fehlt in .Renviron!")

# Abfrage über alle Zeilen der cities-Tabelle
prices_raw <- pmap_dfr(cities, get_prices)

# Summary Stats für den Vergleich
df_stats <- prices_raw %>%
  group_by(city) %>%
  summarise(
    n = n(),
    avg_diesel = mean(diesel, na.rm = T),
    avg_e5 = mean(e5, na.rm = T)
  ) %>%
  arrange(avg_diesel)

# --- Plotting ---

plot_gas <- ggplot(df_stats, aes(x = reorder(city, avg_diesel), y = avg_diesel, fill = avg_diesel)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = sprintf("%.2f €", avg_diesel)), vjust = -0.5, fontface = "bold") +
  scale_fill_gradient(low = "#55efc4", high = "#ff7675") +
  coord_cartesian(ylim = c(1.80, 2.20)) +
  labs(
    title = "Spritpreis-Check: Wo kommt der Rabatt an?",
    subtitle = paste("Stand:", format(Sys.Date(), "%d.%m.%Y")),
    x = "", y = "Euro/Liter",
    caption = "Data: Tankerkönig API | Analysis: E. Ksenzov"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

# Anzeigen & Speichern
print(plot_gas)
ggsave("gas_price_check.png", plot_gas, width = 12, height = 8)

