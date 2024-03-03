install.packages("ggplot2")
install.packages("gridExtra")
install.packages("grid")
install.packages("gtable")
install.packages("scales")

library(ggplot2)
library(gridExtra)
library(scales) 


punnett_df <- expand.grid(Parent1 = c("A", "a"), Parent2 = c("A", "a"))
punnett_df$Offspring = with(punnett_df, paste0(Parent1, Parent2))

punnett_plot <- ggplot(punnett_df, aes(x = Parent1, y = Parent2)) +
  geom_tile(aes(fill = Offspring), colour = "black", size = 0.5) +
  geom_text(aes(label = Offspring), size = 6) +
  scale_fill_manual(values = c("AA" = "lightgreen", "Aa" = "lightblue", "aA" = "lightblue", "aa" = "salmon")) +
  labs(title = "Punnett Square for Allele Crosses", subtitle = "Parental Genotypes and Offspring Outcomes") +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 10))

allele_frequencies <- c("AA" = 0.25, "Aa" = 0.5, "aa" = 0.25)
prout_df <- data.frame(Genotype = names(allele_frequencies), Frequency = allele_frequencies)
prout_df$PercentLabel <- paste(prout_df$Genotype, " - ", scales::percent(prout_df$Frequency))

prout_plot <- ggplot(prout_df, aes(x = "", y = Frequency, fill = PercentLabel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c(
    paste("AA", " - ", scales::percent(0.25)) = "lightgreen",
    paste("Aa", " - ", scales::percent(0.5)) = "lightblue",
    paste("aa", " - ", scales::percent(0.25)) = "salmon"
  )) +
  labs(title = "Population Allele Frequencies", subtitle = "Distribution based on Punnett Square outcomes") +
  theme_void() +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 10),
        legend.title = element_blank(), legend.position = "right")


combined_plot <- grid.arrange(punnett_plot, prout_plot, ncol = 2, widths = c(1.5, 1))


grid.text("Genetic Crosses →", x = 0.5, y = 0.95, gp = gpar(col = "blue", fontsize = 10, fontface = "bold"))
grid.text("→ Allele Frequencies", x = 0.5, y = 0.05, gp = gpar(col = "blue", fontsize = 10, fontface = "bold"))


print(combined_plot)

