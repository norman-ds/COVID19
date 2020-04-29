setwd('R')

# read downloaded data 
# download : https://www.bfs.admin.ch/bfs/de/home/statistiken/gesundheit/gesundheitszustand/sterblichkeit-todesursachen.html
# So können im Mortalitätsmonitoring die «beobachteten» Zahlen mit den «erwarteten» Zahlen verglichen werden. Das BFS publiziert diese Zahlen seit dem 11. Mai 2015 wöchentlich. Das Monitoring umfasst alle Personen mit Wohnsitz in der Schweiz, die in der Schweiz verstorben sind.
fn0 <- 'ts-d-14.03.04.03-wr.csv' # (<10KB) Wöchentlich erfasste Todesfälle 2020
fn0n <- 'ts-d-14.03.04.05-wr.csv' # (<20KB) Wöchentliche Todesfälle nach Grossregion, 65-Jährige und ältere, 2020
# download : https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/geburten-todesfaelle/todesfaelle.html
fn1 <- 'cc-d-01.04.02.01.03.xlsx' # (<100KB) provisorisch Todesfällenach 2020 nach Monate, Kanton/Städte, Gender, Nation, Alter
fn2 <- 'cc-d-01.04.02.01.32.xlsx' # (300KB) ständige Wohnbevölkerung Schweiz, Todesfälle 2015-20, nach Woche, Alter
fn3 <- 'ts-q-01.04.02.01.31.csv' # (10MB) Todesfälle nach Fünf-Jahres-Altersgruppe, Geschlecht, Woche und Grossregion
fn4 <- 'ts-q-01.04.02.01.30.csv' # (20MB) Todesfälle nach Fünf-Jahres-Altersgruppe, Geschlecht, Woche und Kanton
datapath <- file.path('..','Data')

library(dplyr)

###############################################
# N=5411 Schweizer mit Wohnsitz Schweiz
read.csv2(file.path(datapath,fn0n), stringsAsFactors=F) %>%
  filter(Woche<5) %>%
  add_tally(as.double(hochrechnung))

df0 <-read.csv2(file.path(datapath,fn0), stringsAsFactors=F) %>%
  mutate(value=as.integer(hochrechnung), 
         year=gsub('.*(.{4})$','\\1',Endend),
         cw=sprintf('%02d',Woche)) %>%
  filter(!is.na(value)) %>%
  select(year,cw, age=Alter, value) %>%
  mutate(file = fn0)

  
###############################################
# N=5437
read.csv2(file.path(datapath,fn4), stringsAsFactors=F) %>%
  filter(GEO=='CH', grepl('2020-W0[1-4]',TIME_PERIOD), AGE=='_T', SEX=='T') %>%
  add_tally(Obs_value)


df4 <- read.csv2(file.path(datapath,fn4), stringsAsFactors=F) %>%
  filter(GEO=='CH', grepl('2020-W[01]',TIME_PERIOD), SEX=='T') %>%
  filter(AGE != '_T') %>%
  mutate(year=gsub('^(.{4}).*','\\1',TIME_PERIOD),
         cw=gsub('.*(.{2})$','\\1',TIME_PERIOD)) %>%
  mutate(AC0=gsub('(.+[ET])([0-9]?)([049])$','\\2\\3',AGE)) %>%
  mutate(AC1=as.integer(AC0)) %>%
  mutate(age=if_else(AC1<=64,'0-64','65+')) %>%
  group_by(year, cw, age) %>%
  summarise(value=sum(Obs_value)) %>%
  ungroup() %>%
  mutate(file = fn4)



###############################################
# N=5437
read.csv2(file.path(datapath,fn3), stringsAsFactors=F) %>%
  filter(GEO=='CH', grepl('2020-W0[1-4]',TIME_PERIOD), AGE=='_T', SEX=='T') %>%
  add_tally(OBS_VALUE)

df3 <- read.csv2(file.path(datapath,fn3), stringsAsFactors=F) %>%
  filter(GEO=='CH', grepl('2020-W[01]',TIME_PERIOD), SEX=='T') %>%
  filter(AGE != '_T') %>%
  mutate(year=gsub('^(.{4}).*','\\1',TIME_PERIOD),
         cw=gsub('.*(.{2})$','\\1',TIME_PERIOD)) %>%
  mutate(AC0=gsub('(.+[ET])([0-9]?)([049])$','\\2\\3',AGE)) %>%
  mutate(AC1=as.integer(AC0)) %>%
  mutate(age=if_else(AC1<=64,'0-64','65+')) %>%
  group_by(year, cw, age) %>%
  summarise(value=sum(OBS_VALUE)) %>%
  ungroup() %>%
  mutate(file = fn3)



library(readxl)
###############################################
# N=5497 Schweizer + 515 Ausländer
fpath1 <- file.path(datapath,fn1)
excel_sheets(fpath1)
read_excel(fpath1, sheet = 'Januar 2020p', skip = 4) %>%
  filter(Kantone=='Schweiz')
  
df1 <- excel_sheets(fpath1) %>%
  rlang::set_names() %>%
  purrr::map(read_excel, path = fpath1, skip=4) %>%
  bind_rows(.id='id') %>%
  filter(Kantone=='Schweiz') %>%
  select(c(1,8:10)) %>%
  tidyr::pivot_longer(-id) %>%
  mutate(year=gsub('.*(2[012][0-9]{2}).*','\\1',id),
         month=gsub('^(.+)\\s.*','\\1',id),
         age=if_else(grepl('^65',name),'65+','0-64')) %>%
  group_by(year, month, age) %>%
  summarise(value=sum(value)) %>%
  ungroup() %>%
  mutate(file = fn1)


###############################################
# N=5437 ständiger Wohnsitz in der Schweiz
fpath2 <- file.path(datapath,fn2)
f2namesY <- 2020:2015 
f2namesA <- c('Total', 'Y0T19', 'Y20T39', 'Y40T64', 'Y65T79', 'Y_GE80')
f2colnames <- c('Woche',unlist(lapply(f2namesA, paste, f2namesY, sep='YV')))
f2namesK <- excel_sheets(fpath2)
read_excel(fpath2, sheet = 'CH', range = cell_rows(8:12), col_names = f2colnames) %>%
  filter(Woche<5) %>%
  # select(Woche, Y65T79YV2020, Y_GE80YV2020) %>%
  # mutate(Y_GE65YV2020 = Y65T79YV2020 + Y_GE80YV2020) %>%
  # add_tally(Y_GE65YV2020)
  select(Woche,TotalYV2020, TotalYV2019, TotalYV2018) %>%
  add_tally(TotalYV2020)


df2 <- read_excel(fpath2, sheet = 'CH', range = cell_rows(8:60), col_names = f2colnames) %>%
  tidyr::pivot_longer(-Woche) %>%
  filter(!grepl('^Total',name), grepl('2020',name), !is.na(value)) %>%
  mutate(year=gsub('.*(20[0-9]{2})$','\\1',name),
         cw=sprintf('%02d',Woche),
         age=gsub('^.*[TE]([0-9]{2})YV.*$','\\1',name)) %>%
  mutate(age=if_else(age<'79','0-64','65+')) %>%
  group_by(year, cw, age) %>%
    summarise(value=sum(value)) %>%
  ungroup() %>%
  mutate(file = fn2)


###############################################
library(ggplot2)
bind_rows(df0,df2,df3,df4) %>%
  mutate(cw = as.integer(cw)) %>%
  ggplot(aes(x=cw, y=value, color=file)) +
  geom_line() +
  scale_x_continuous(limits = c(1, 15)) +
  facet_grid(rows='age')
  
bind_rows(df0,df2,df3,df4) %>%
  filter(cw=='15')

# the file 'ts-d-14.03.04.04.03-wr.csv' has slightly larger numbers in the last two weeks
# for groupe age 65+: 1364 rsp 1494 (+10%)
# and for age 0-64: 148 rsp 185 (+25%)

