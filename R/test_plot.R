setwd('R')

# read downloaded data 
# download : https://www.bfs.admin.ch/bfs/de/home/statistiken/gesundheit/gesundheitszustand/sterblichkeit-todesursachen.html

library(dplyr)
dat19 <-read.csv2('../Data/ts-d-14.03.04.03-wr_ZR.csv', stringsAsFactors=F) %>%
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
  #mutate_at(c('KJ','KW'), as.character) %>%
  group_by(KJ) %>%
  mutate(N_cum = cumsum(N), Expect_cum = cumsum(Expect)) %>%
  ungroup() %>%
  mutate(highlight=KJ!='2020')

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

ggsave('dc2020.png', width = 8, height = 6)
