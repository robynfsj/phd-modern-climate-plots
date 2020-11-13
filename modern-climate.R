# Set up ------------------------------------------------------------------

library(ggplot2)
library(forcats)
library(gridExtra)




# Read and check data -----------------------------------------------------

ioannina <- read.csv("data/ioannina.csv")
ohrid <- read.csv("data/ohrid.csv")
ioannina
ohrid

# Convert month column from character to factor data type and order according 
# to appearance (plot will come out alphabetically if not performed).
ioannina$month <- fct_inorder(as.factor(ioannina$month))
ohrid$month <- fct_inorder(as.factor(ohrid$month))




# Plot Ioannina -----------------------------------------------------------

ioannina_plot <- ggplot(data = ioannina, aes(x = month)) + 
  geom_col(aes(y = precipitation),
           width = 0.5,
           fill = "lightblue3") +
  geom_line(aes(y = temperature * 7),
            size = 1.5,
            colour = "indianred",
            group = 1) +
  geom_point(aes(y = temperature * 7),
             shape = 21,
             size = 2.5,
             colour = "indianred",
             fill = "white",
             group = 1) +
  scale_y_continuous(name = "Precipitation (mm)",
                     expand = c(0,0), # force y-axis to cross x-axis at 0
                     limits = c(0, 180),
                     breaks = seq(0, 180, 20),
                     sec.axis = sec_axis(~ . / 7, name = "Temperature (°C)")) +
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", 
                              "N", "D")) +
  xlab("Month") +
  ggtitle("Lake Ioannina") +
  theme_classic(base_size = 14) +
  theme(
    # left y-axis
    axis.text.y.left = element_text(colour = "lightblue3"),
    axis.title.y.left = element_text(colour = "lightblue3"),
    axis.ticks.y.left = element_line(colour = "lightblue3"),
    axis.line.y.left = element_line(colour = "lightblue3"),
    # right y-axis
    axis.text.y.right = element_text(colour = "indianred"),
    axis.title.y.right = element_text(colour = "indianred3", vjust = 1),
    axis.ticks.y.right = element_line(colour = "indianred"),
    axis.line.y.right = element_line(colour = "indianred")
  )

ioannina_plot




# Plot Ohrid --------------------------------------------------------------

ohrid_plot <- ggplot(data = ohrid, aes(x = month)) + 
  geom_col(aes(y = precipitation),
           width = 0.5,
           fill = "lightblue3") +
  geom_line(aes(y = temperature * 7),
            size = 1.5,
            colour = "indianred",
            group = 1) +
  geom_point(aes(y = temperature * 7),
             shape = 21,
             size = 2.5,
             colour = "indianred",
             fill = "white",
             group = 1) +
  scale_y_continuous(name = "Precipitation (mm)",
                     expand = c(0,0), # force y-axis to cross x-axis at 0
                     limits = c(0, 180),
                     breaks = seq(0, 180, 20),
                     sec.axis = sec_axis(~ . / 7, name = "Temperature (°C)")) +
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", 
                              "N", "D")) +
  xlab("Month") +
  ggtitle("Lake Ohrid") +
  theme_classic(base_size = 14) +
  theme(
    # left y-axis
    axis.text.y.left = element_text(colour = "lightblue3"),
    axis.title.y.left = element_text(colour = "lightblue3"),
    axis.ticks.y.left = element_line(colour = "lightblue3"),
    axis.line.y.left = element_line(colour = "lightblue3"),
    # right y-axis
    axis.text.y.right = element_text(colour = "indianred"),
    axis.title.y.right = element_text(colour = "indianred3", vjust = 1),
    axis.ticks.y.right = element_line(colour = "indianred"),
    axis.line.y.right = element_line(colour = "indianred")
  )

ohrid_plot




# Plots together ----------------------------------------------------------

grid.arrange(ioannina_plot, ohrid_plot, nrow = 2)
# NOTE: adjust base_size and breaks to make the y-axis more suitable for the 
# smaller plot size.



