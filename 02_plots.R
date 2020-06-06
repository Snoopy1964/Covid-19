###################################################
#
# Plots:
#
#
#
#
#
###################################################

#-----------------------------------------
# Anzahl Erkrankte pro 100.000 Einwohner
#-----------------------------------------
coi <- c("BE", "BW", "CA", "DK", "FI", "FR","DE", "GL", "IS", "IN", "IL", "IT", "JP", "LU", "MX", "NL", "NO", "PL", "PT", "RU", "ES", "SE", "CH", "TR", "GB", "US")

# df.latest <- df %>% group_by(charcode)  %>% dplyr::filter(day == today())
df.latest <- df %>% dplyr::filter(day == today())
df.latest <- df %>% dplyr::filter(day == as.Date("2020-06-01"))

gg.incidence.latest <- df.latest                                      %>% 
  mutate(Rsum = cases/population*100000)                        %>% 
  select(c(charcode, country.iso, Rsum, cases, population))     %>% 
  # ungroup(charcode)                                                   %>%
  top_n(20, Rsum)                                                     %>%
  ggplot(aes(reorder(country.iso, Rsum), Rsum))

gg.incidence.latest                       +
  geom_bar(stat="identity")               +
  coord_flip()                            +
  geom_text(aes( y     = Rsum-25, 
                 label = round(Rsum,0)), 
            color = "white")                +
  labs( x = "Land", 
        y = "Anzahl Erkrankte pro 100.000 Einwohner")

#-----------------------------------------------------------
# Anzahl Aktiver Erkrankungen pro 100.000 Einwohner pro Tag
#   - gefiltert nach Tag
#-----------------------------------------------------------

coi <- c("DE")

df.active <- df %>% dplyr::filter(charcode %in% coi) 

gg <- df.active %>% ggplot(aes(x=day))
gg + 
  geom_area(aes(y = cases,           fill = "recovered")) + 
  geom_area(aes(y = active + deaths, fill = "active"))    +
  geom_area(aes(y = deaths,          fill = "death"))     +
  scale_fill_manual(name="", 
                    values = c("recovered"="#00ba38", "active"="#f8766d", "death"="dark grey"))  # line color
  
  

