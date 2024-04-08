
source("rmd/scripts/baseEnergia.R")

library(ggtext)

baseGraph <- base %>% 
  filter(year == 2019 &
           hdicode != "" & 
           !is.na(hdicode) & 
           !is.na(pib_percapita) & 
           !is.na(per_capita_electricity)) %>% 
  mutate(colombia = ifelse(iso_code == "COL", 2, 1)) %>%
  select(hdicode, per_capita_electricity, pib_percapita, iso_code, population, colombia)

baseGraph$hdicode <- factor(baseGraph$hdicode, levels = c("Low", "Medium",
                                                          "High", "Very High"),
                            labels = c("Bajo", "Medio", "Alto", "Muy Alto"))


Reg1 <- lm(formula = log(per_capita_electricity) ~ log(pib_percapita), 
           data = baseGraph, 
           weights = sqrt(population))

summ_reg1 <- summary(Reg1)

grafico <- baseGraph %>%  
  ggplot(mapping = aes(x = log(pib_percapita), y = log(per_capita_electricity))) +
  geom_point(mapping = aes(color = hdicode, size = population, alpha = 0.5), show.legend = F) +
  scale_size(range = c(1,24)) +
  scale_color_manual(values = c("#900C3F",
                                "#365486",
                                "#E3651D",
                                "#597E52")) +
  geom_smooth(method = "lm", se = F, linewidth = 1, color = "#641E16", alpha = 0.5) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Consumo de electricidad según PIB per cápita - 2019",
       subtitle = "Índice de Desarrollo Humano (IDH): <span style='color:#900C3F;'>**Bajo**</span>, <span style='color:#365486;'>**Medio**, </span><span style='color:#E3651D;'>**Alto**</span> y <span style='color:#597E52;'>**Muy Alto**</span>",
       y = "Logaritmo de Consumo de electricidad per cápita",
       x = "Logaritmo de PIB per cápita",
       caption = "Fuente: elaboración propia en base a Our World in Data y Banco Mundial<br>**#30DayChartChallenge #Day8**<br>@GEstebanGomez") +
  annotate(geom = "text",
           x = c(6, 6),
           y = c(10, 9.5),
           label = c(TeX("$R^2 = 0.77$"),
                     TeX("$\\hat{\\beta} = 1.038$")),
           size = c(4,4)) +
  theme_bw() +
  theme(
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_markdown(hjust = 0.5, size = 12),
        plot.caption = element_markdown(size=10, hjust=0.0, color="black", face = "italic"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none"
        )


ggsave(filename = "../rmd/resultados/graficos/30DayChartChallenge/8Day_circular.png",
       dpi = 300,
       width = 9.23,
       height = 5.81,
       )
