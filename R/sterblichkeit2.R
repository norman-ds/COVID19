setwd('R')

## read downloaded data 
## download : https://www.bfs.admin.ch/bfs/de/home/statistiken/gesundheit/gesundheitszustand/sterblichkeit-todesursachen.html
## So können im Mortalitätsmonitoring die «beobachteten» Zahlen mit den «erwarteten» Zahlen verglichen werden. Das BFS publiziert diese Zahlen seit dem 11. Mai 2015 wöchentlich. Das Monitoring umfasst alle Personen mit Wohnsitz in der Schweiz, die in der Schweiz verstorben sind.
#fn0 <- 'ts-d-14.03.04.03-wr.csv' # (<10KB) wöchentlich erfasste Todesfälle 2020
#fn0n <- 'ts-d-14.03.04.05-wr.csv' # (<20KB) Wöchentliche Todesfälle nach Grossregion, 65-Jährige und ältere, 2020
## download : https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/geburten-todesfaelle/todesfaelle.html
#fn1 <- 'cc-d-01.04.02.01.03.xlsx' # (<100KB) provisorisch Todesfällenach 2020 nach Monate, Kanton/Städte, Gender, Nation, Alter
#fn2 <- 'cc-d-01.04.02.01.32.xlsx' # (300KB) ständige Wohnbevölkerung Schweiz, Todesfälle 2015-20, nach Woche, Alter
#fn3 <- 'ts-q-01.04.02.01.31.csv' # (10MB) Todesfälle nach Fünf-Jahres-Altersgruppe, Geschlecht, Woche und Grossregion
fn4 <- 'ts-q-01.04.02.01.30.csv' # (20MB) Todesfälle nach Fünf-Jahres-Altersgruppe, Geschlecht, Woche und Kanton
datapath <- file.path('..','Data')

library(dplyr)
dfdeath <- read.csv2(file.path(datapath,fn4), stringsAsFactors=F) %>%
  filter(AGE != '_T', SEX=='T') %>%
  mutate(year=gsub('^(.{4}).*','\\1',TIME_PERIOD),
         cw=gsub('.*(.{2})$','\\1',TIME_PERIOD)) %>%
  mutate(cw = as.integer(cw)) %>%
  mutate(AC0=gsub('(.+[ET])([0-9]?)([049])$','\\2\\3',AGE)) %>%
  mutate(AC1=as.integer(AC0)) %>%
  #mutate(age=if_else(AC1<=64,'0-64','65+')) %>%
  mutate(age=as.character(cut(AC1, breaks = c(0,64,80,100)))) %>%
  group_by(geo=GEO, year, cw, age) %>%
  summarise(value=sum(Obs_value)) %>%
  group_by(geo, year, age) %>%
  mutate(value_cum = cumsum(value)) %>%
  ungroup() %>%
  mutate(ggeo=gsub('^(CH0[1-7]).$','\\1',geo)) %>%
  inner_join(
    read.csv2(file.path(datapath,'grossregionCH.csv'), stringsAsFactors=F)
  )

year_last <- '2020'
cw_last <- max(dfdeath[dfdeath$year==year_last,]$cw)


library(ggplot2)
# Todesfälle pro Kalenderwoche 
dfdeath %>%
  filter(geo=='CH') %>%
  ggplot(aes(x=cw, y=value, color=year)) +
  geom_line() +
  scale_x_continuous(limits = c(1, 52)) +
  facet_grid(age ~.)

# Todesfälle kummuliert 
dfdeath %>%
  filter(geo=='CH') %>%
  ggplot(aes(x=cw, y=value_cum, color=year)) +
  geom_line() +
  scale_x_continuous(limits = c(1, 52)) +
  facet_grid(age ~.)

# Todesfälle 2020 pro Alter, Kalenderwoche und Kanton 
dfdeath %>%
  filter(year=='2020', geo != 'CH') %>%
  filter(grepl('CH07',geo)) %>%
  ggplot(aes(x=cw, y=value, color=kanton)) +
  geom_line() +
  scale_x_continuous(limits = c(1, 15)) +
  facet_grid(age ~., scales = 'free')


# Gemeldete Todesfälle der Jahre 2020-2020 (Age 65+)
# pro Kanton, Kalenderwoche und kumuliert

facetlab <- c("pro KW", "kumuliert")
names(facetlab) <- c("value", "value_cum")

dfdeath %>%
  mutate(highlight=year!=year_last) %>%
  filter(geo != 'CH') %>%
  filter(grepl('CH07',geo)) %>%
  #filter(age == '65+') %>%
  filter(cw %in% 1:20) %>%
  tidyr::pivot_longer(cols = starts_with('value'), names_to = 'key', values_to = 'value') %>%
  ggplot(aes(x=cw, y=value, group=year, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a3", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.2)) +
  theme_minimal() +
  ggtitle("Gemeldete Todesfälle der Jahre 2000-2020 (Age 65+)") +
  ylab(NULL) +
  xlab('Kalenderwoche') +
  scale_x_continuous() +
  facet_wrap(. ~ key + age, nrow=2, scales = 'free', labeller = labeller(key = facetlab)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )





# latest data

bardat <- dfdeath %>% 
  filter(cw <= cw_last) %>%
  group_by(year, kanton, geo, age) %>%
  summarise(N=sum(value), 
            N6= sum(value[cw<=(cw_last-6)]), 
            N4= sum(value[cw<=(cw_last-4)])) %>%
  ungroup() %>%
  mutate(highlight=year!=year_last)  %>%
  mutate(x = as.integer(year))

bardat2 <- dfdeath %>% 
  filter(cw <= cw_last) %>%
  group_by(year, kanton, geo, age) %>%
  summarise(N=sum(value), 
            N12= sum(value[cw<=12]), 
            N9= sum(value[cw<=9]), 
            N6= sum(value[cw<=6]), 
            N3= sum(value[cw<=3])) %>%
  ungroup() %>%
  mutate(highlight=year!=year_last)  %>%
  mutate(x = as.integer(year))


bartxt <- sprintf('Im Jahr 2020 erfasste Todesfälle %s\n sind leicht über Mittlerem Erwartungswert %s', 
                  format(last(bardat$N),big.mark = "'"), format(last(bardat$Expect),big.mark = "'"))
bartxt <- 'Hallo Velo'

bardat2 %>%
  filter(grepl('CH011',geo)) %>%
  #filter(age == '65+') %>%
  ggplot(aes(x=as.integer(year), y=N, fill=highlight)) +
  geom_bar(stat = "identity", width = 1) +
  geom_segment( aes(x=x, xend=x, y=N3, yend=N6), color='white') +
  geom_segment( aes(x=x, xend=x, y=N9, yend=N12), color='white') +
  geom_smooth(method = lm, se = T, fill='lightblue') +
  #geom_point(aes(y=N6), color='white', shape='-', size=7) +
  #geom_point(aes(y=N4), color='white', shape='-', size=7) +
  scale_fill_manual(values = c("#69b3a3", "lightgrey")) +
  #scale_color_manual(values = c("#19b3a3", "lightgrey")) +
  theme_minimal() +
  ggtitle(sprintf("Todesfälle bis Kalenderwoche %s", cw_last)) +
  ylab(NULL) +
  xlab('Jahr') +
  #scale_x_continuous() +
  geom_label(x=2010, y=350, label='Todesfälle bis Woche 9 und 11', size=4, color='white', fill='lightgrey') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  facet_wrap(age ~ .)

# ggsave('dc2020.png', width = 8, height = 6)









filter(grepl("^20[1-2][0-9]$", KJ)) %>%
  filter(Alter=='65+') %>%
  select(KJ,KW=Kalenderwoche, Expect=Erwartung, N=Anzahl_Todesfalle) %>%
  mutate_at(c('Expect', 'N'), as.integer)
dat20 <-read.csv2('../Data/ts-d-14.03.04.03-wr.csv', stringsAsFactors=F) %>%
  filter(Alter=='65+') %>%
  mutate(KJ='2020') %>%
  select(KJ,KW=Woche, Expect=Erwartung, N=hochrechnung) %>%
  mutate_at(c('Expect', 'N'), as.integer)

# build a dataframe
dat <- bind_rows(dat20,dat19) %>%
  group_by(KJ) %>%
  mutate(N_cum = cumsum(N), Expect_cum = cumsum(Expect)) %>%
  ungroup() %>%
  mutate(highlight=KJ!='2020')

# latest record
datlatest <-dat20 %>% 
  filter(!is.na(N)) %>%
  filter(KW==max(KW))
  
rm(dat19, dat20)

# use ggplot for all plots
library(ggplot2)
library(dplyr)

#  Mittlere Erwartungswerte der Toten der Jahre 2010-2020 (Age 65+)

curvetxt <- sprintf('Mittlerer Erwartungswert %s \nfür Schweizer über 65 Jahre \nin der Woche 6 and 7 im 2020.', max(dat$Expect))

dat %>%
  ggplot(aes(x=KW, y=Expect, group=KJ, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a3", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.2)) +
  theme_minimal() +
  ggtitle("Mittlere Erwartungswerte der Toten der Jahre 2010-2020 (Age 65+)") +
  ylab(NULL) +
  xlab('Kalenderwoche') +
  scale_x_continuous(limits = c(1, 52)) +
  geom_label( x=15, y=1200, label=curvetxt, size=4, color="#69b3a3") +
  geom_label( x=40, y=950, label='Mittlerer Erwartungswert der Toten\nsteigt jährlich.', 
              size=4, color="lightgrey") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) 


# Gemeldete Todesfälle der Jahre 2010-2020 (Age 65+)
# pro Kalenderwoche und kumuliert

facetlab <- c("pro KW", "kumuliert")
names(facetlab) <- c("N", "N_cum")

dat %>%
  select(KJ, KW, N, N_cum, highlight) %>%
  filter(KW<22) %>%
  tidyr::pivot_longer(cols = starts_with('N'), names_to = 'key', values_to = 'value') %>%
  ggplot(aes(x=KW, y=value, group=KJ, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a3", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.2)) +
  theme_minimal() +
  ggtitle("Gemeldete Todesfälle der Jahre 2010-2020 (Age 65+)") +
  ylab(NULL) +
  xlab('Kalenderwoche') +
  scale_x_continuous(limits = c(1, 20)) +
  facet_wrap(. ~ key, scales = 'free_y', labeller = labeller(key = facetlab)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )


#  Mittlere Erwartungswerte der Toten der Jahre 2010-2020 (Age 65+)
# pro Kalenderwoche und kumuliert

facetlab <- c("pro KW", "kumuliert")
names(facetlab) <- c("Expect", "Expect_cum")

dat %>%
  select(KJ, KW, Expect, Expect_cum, highlight) %>%
  tidyr::pivot_longer(cols = starts_with('E'), names_to = 'key', values_to = 'value') %>%
  ggplot(aes(x=KW, y=value, group=KJ, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a3", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.2)) +
  theme_minimal() +
  ggtitle("Mittlere Erwartungswerte der Toten der Jahre 2010-2020 (Age 65+)") +
  ylab(NULL) +
  xlab('Kalenderwoche') +
  scale_x_continuous(limits = c(1, 52)) +
  facet_wrap(. ~ key, scales = 'free_y', labeller = labeller(key = facetlab)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )


#  Mittlere Erwartungswerte und gemeldete Totesfälle der Jahre 2010-2020 (Age 65+)
# pro Kalenderwoche und kumuliert

facet1lab <- c("erwartet", "gemeldet")
facet2lab <- c("pro KW", "kumuliert")
names(facet1lab) <- c("erwartet", "gemeldet")
names(facet2lab) <- c("pro KW", "kumuliert")

dat %>%
  tidyr::pivot_longer(cols = starts_with(c('N','E')), names_to = 'key', values_to = 'value') %>%
  mutate(key1 = if_else(grepl('^N',key), 'gemeldet', 'erwartet'),
         key2 = if_else(grepl('cum$',key), 'kumuliert', 'pro KW')) %>%
  #filter(grepl('Expect',key) | KW<21) %>%
  ggplot(aes(x=KW, y=value, group=KJ, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a3", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.2)) +
  theme_minimal() +
  ggtitle("Todesfälle der Jahre 2010-2020 (Age 65+)",
          "Mittlere Erwartungswerte und laufend erfasste Todesfälle") +
  ylab(NULL) +
  xlab('Kalenderwoche') +
  scale_x_continuous(limits = c(1, 52)) +
 # facet_wrap(key2 ~ key1, scales = 'free') +
  facet_grid(vars(key2), vars(key1), scales = 'free', 
             labeller = labeller(key1=facet1lab, key2=facet2lab)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )

# ggsave('dc2020.png', width = 8, height = 6)

# latest data

bardat <- dat %>% 
  filter(KW <= datlatest$KW) %>%
  group_by(KJ) %>%
  summarise(N=sum(N), Expect=sum(Expect), highlight=any(highlight)) %>%
  ungroup() %>%
  mutate(diff = N-Expect)

bartxt <- sprintf('Im Jahr 2020 erfasste Todesfälle %s\n sind leicht über Mittlerem Erwartungswert %s', 
                  format(last(bardat$N),big.mark = "'"), format(last(bardat$Expect),big.mark = "'"))

bardat %>%
  ggplot(aes(x=KJ, y=N, fill=highlight)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#69b3a3", "lightgrey")) +
  geom_point(aes(x=KJ, y=Expect)) +
  theme_minimal() +
  ggtitle("Todesfälle bis Kalenderwoche 15 (Age 65+)",
          "Mittlere Erwartungswerte und laufend erfasste Todesfälle") +
  ylab(NULL) +
  xlab('Kalenderwoche') +
  geom_label(x='2017', y=12000, label=bartxt, size=4, color="#69b3a3", fill='white') +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )

# ggsave('dc2020.png', width = 8, height = 6)
