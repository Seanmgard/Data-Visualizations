
# Load Packages -----------------------------------------------------------

pacman::p_load(dplyr, ggplot2, readxl, readr, tidyr, ggbeeswarm, ggtext, showtext, ggimage)

# Specify Font ------------------------------------------------------------

#Specify font
font_add_google("Lato", "lato")
showtext_auto()

# Load Data ---------------------------------------------------------------

Beatles_Album_List <- c("Please Please Me (Remastered)", "With The Beatles (Remastered)", "A Hard Day's Night (Remastered)",
                        "Beatles For Sale (Remastered)", "Help! (Remastered)", "Rubber Soul (Remastered)", "Revolver (Remastered)",
                        "Sgt. Pepper's Lonely Hearts Club Band (Remastered)", "Magical Mystery Tour (Remastered)", "The Beatles (Remastered)",
                        "Yellow Submarine (Remastered)", "Abbey Road (Remastered)", "Let It Be (Remastered)")

Rename_Map <- c("Please Please Me (Remastered)" = "Please Please Me", "With The Beatles (Remastered)" = "With The Beatles",
                "A Hard Day's Night (Remastered)" = "Hard Day's Night", "Beatles For Sale (Remastered)" = "Beatles For Sale",
                "Help! (Remastered)" = "Help!", "Rubber Soul (Remastered)" = "Rubber Soul", "Revolver (Remastered)" = "Revolver",
                "Sgt. Pepper's Lonely Hearts Club Band (Remastered)" = "Sgt. Peppers", "Magical Mystery Tour (Remastered)" = "Magical Mystery Tour",
                "The Beatles (Remastered)" = "White Album", "Yellow Submarine (Remastered)" = "Yellow Submarine",
                "Abbey Road (Remastered)" = "Abbey Road", "Let It Be (Remastered)" = "Let It Be")

Beatles_Raw <- read_csv("Projects/Beatles Emotional Score/beatles_spotify.csv")

Beatles <- Beatles_Raw |> 
  select(name, album, release_date, energy, URL)

Beatles_Albums <- Beatles |> 
  filter(album %in% Beatles_Album_List) |> 
  mutate(release_date = as.Date(release_date, format = "%m/%d/%Y")) |> 
  arrange(release_date)

Beatles_Albums$album <- Rename_Map[Beatles_Albums$album]

Beatles_Albums <- Beatles_Albums |> 
  filter(!album %in% c("Magical Mystery Tour", "Yellow Submarine"))

average_energy <- mean(Beatles_Albums$energy, na.rm = TRUE)

# Visualize Data ----------------------------------------------------------

#Title and Subtitle Text
Title <- "Using Spotify's API To Track The Energy of the Beatles"
Subtitle <- "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. \nTypically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, \nwhile a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range,\n perceived loudness, timbre, onset rate, and general entropy."
Caption <- "Data from Spotify. Chart by Sean Gardner (seanmgard.com)"

min_x <- min(Beatles_Albums$energy)

ggplot(Beatles_Albums, aes(x=energy, y = factor(album, level=c("Let It Be", "Abbey Road", "Yellow Submarine", "White Album",
                                                         "Magical Mystery Tour", "Sgt. Peppers", "Revolver", "Rubber Soul",
                                                         "Help!", "Beatles For Sale", "Hard Day's Night",
                                                         "With The Beatles", "Please Please Me")))) + 
  geom_point(aes(color = album), size = 3, alpha = 0.55) +
  geom_image(data = Beatles_Albums, mapping = aes(x=-0.01, y = album, image=URL), size = 0.06) +
  labs(x = "Energy Score", title = Title, subtitle = Subtitle, caption = Caption) +
  geom_vline(xintercept = average_energy, linetype = "dashed", color = "white") +
  annotate("text", x = average_energy + 0.28, y = length(levels(factor(Beatles_Albums$album))) /2, label = "Average Energy", color = "white") +
  geom_curve(aes(x = average_energy + 0.2, y = length(levels(factor(Beatles_Albums$album))) /2, xend = average_energy, yend = length(levels(factor(Beatles_Albums$album)))/2), color = "white", curvature = -0.3, size = 0.3, arrow = arrow(type = "closed", length = unit(0.15, "inches"))) +
  theme_void() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position = 'none',
    axis.title = element_blank(),
    axis.text = element_text(size = 12, colour = "white", family = "lato", hjust = 0),
    axis.text.y = element_blank(),
    text = element_text(size = 12, lineheight = 0.3, colour = "white", family = "lato"),
    plot.background = element_rect(fill = '#1E212B'),
    panel.background = element_blank(), 
    panel.grid=element_blank(),
    plot.title = element_text(color="white", face="bold", family = "lato", size=28, margin=margin(t=10)),
    plot.subtitle = element_markdown(color= "white", size=13, family = "lato", margin=margin(t = 5, b = 20)),
    plot.caption = element_markdown(colour = "grey", hjust = 0, family = "lato", margin = margin(t = 20)),
    plot.margin = margin(b = 50, t = 20, r = 100, l = 100))

## Need to change dimensions of all images to 1000x1000 to get them to fit properly.
