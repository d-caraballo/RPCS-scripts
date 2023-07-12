library(dyplr)
library(ggplot2)
library(scales)
library("ggh4x")

# calculate means and standard errors by rank and sex
plotdata <- RPCS %>%
  group_by(Locality, Sex, Clade)

plotdata$Locality <- factor(plotdata$Locality, c("San Alonso", "Loreto", "Contreras Cue", "Estancia La Tacuarita", "Saladas Sur",
                                                 "Saladas", "Santa Rosa",  "San Roque", "Estancia San Luis", "Pago Alegre", 
                                                 "Paraje Angostura", "Mbarigui", "Chavarria","Colonia 3 de abril",
                                                 "Rincon de Ambrosio","Goya"))

# plot the means and standard errors by sex
ggplot(plotdata, aes(x = factor(Locality,
                                labels = c("San \nAlonso", "Loreto", "Contreras \nCue", "Estancia \nLa \nTacuarita", "Saladas \nSur",
                                           "Saladas", "Santa \nRosa",  "San \nRoque", "Estancia \nSan Luis", "Pago \nAlegre", 
                                           "Paraje \nAngostura", "Mbarigui", "Chavarria","Colonia 3 \nde abril",
                                           "Rincon \nde \nAmbrosio","Goya")),
                     y = RPCS_copy_number_x1000, 
                     group=Sex, 
                     color=Sex)) +
  geom_point(size = 5, 
             alpha = .6) +
  geom_smooth(size = 0.9, level = 0.5, se=FALSE, formula=y ~ x
              ) +
  labs(x = "",
       y = "RPCS copy number (x10^3)",
       title = "",
       subtitle = "") +
  theme_minimal()


#smooth
#se: Display confidence interval around smooth? (TRUE by default, see level to control.)
#level:	Level of confidence interval to use (0.95 by default).

