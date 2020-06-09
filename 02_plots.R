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
coi <- c("FR","DE", "RU", "US")

# df.latest <- df %>% group_by(charcode)  %>% dplyr::filter(day == today())
df.latest <- df %>% dplyr::filter(day == today()-1)
df.latest <- df %>% dplyr::filter(day == as.Date("2020-06-08"))

gg.incidence.latest <- df.latest                                      %>% 
  mutate(Rsum = cases/population*100000)                        %>% 
  select(c(charcode, country.iso, Rsum, cases, population))     %>% 
  # ungroup(charcode)                                                   %>%
  top_n(20, Rsum)                                                     %>%
  ggplot(aes(reorder(country.iso, Rsum), Rsum))

gg.incidence.latest                       +
  geom_bar(stat="identity", fill = "#f8766d")               +
  coord_flip()                            +
  geom_text_repel(aes( y     = Rsum, 
                 label = round(Rsum,0)), 
                 hjust = "bottom",
                 direction = "x",
                 color = "black")                +
  labs( x = "Land", 
        y = "Anzahl Erkrankte pro 100.000 Einwohner")


gg.cases.latest <- df.latest                                      %>% 
  select(c(charcode, country.iso, cases, population))     %>% 
  top_n(20, cases)                                                     %>%
  ggplot(aes(reorder(country.iso, cases), cases))

gg.cases.latest                       +
  geom_bar(stat="identity", fill = "#f8766d")               +
  coord_flip()                            +
  geom_text_repel(aes( y     = cases, 
                 label = format(cases, big.mark = ".", decimal.mark = ",")), 
            hjust = "top",
            direction = "x",
            color = "black")                +
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
  
  
#-----------------------------------------------------------
# CI pro Tag pro 100.000 Einwohner
#-----------------------------------------------------------
gg <- df %>% 
  dplyr::filter(charcode %in% c("DE", "US", "BR") & day >= "2020-03-01") %>% 
  group_by(charcode)                                                     %>%
  arrange(charcode,day)                                                  %>% 
  mutate(
    CI   = cases.day/population*100000,
    CI.7 = rollmean(CI,7, align = "right", fill = NA)
  )                                                                      %>%
  select(c(1:4, "CI", "CI.7"))                                           %>%
  ggplot(aes(x=day))

gg + 
  geom_smooth(aes(y=CI, color = charcode)) + 
  geom_point(aes(y=CI.7, color = charcode))



#-----------------------------------------------------------
# CI pro Tag pro 100.000 Einwohner (World)
#-----------------------------------------------------------
gg <- df %>% 
  group_by(day)                           %>%
  dplyr::filter(day >= "2020-02-01")      %>%
  summarize(
    cases.day     = sum(cases.day),
    active.day    = sum(active.day),
    recovered.day = sum(recovered.day),
    deaths.day    = sum(deaths.day)
  )                                       %>%
  ggplot(aes(x=day))

gg + 
  geom_bar(aes(y=cases.day,                 fill = "total"),     stat="identity") +
  geom_bar(aes(y=-deaths.day-recovered.day, fill = "recovered"), stat="identity") +
  geom_bar(aes(y=-deaths.day,               fill = "death"),     stat="identity") +
  scale_fill_manual(name="", 
                    values = c("recovered"="#00ba38", "total"="#f8766d", "death"="dark grey"))  # line color

  



