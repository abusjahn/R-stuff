# Analyse von Pizza-Bestellungen
# Datensatz: d.pizza aus dem Paket DescTools
# unabhängige Variablen: weekday, wine_ordered
# abhängige Variablen: count,delivery_min, temperature, price, wine_ordered,
#                     Wochenende
# 
# Der Kunde möchte allgemein von Ihnen wissen, welche Unterschiede es gibt, dazu
# hätte er gern Abbildungen und Tabellen.
# 
# Das Formulieren von Hypothesen und die Auswahl statistischer Verfahren ist Ihnen
# überlassen.
# Erzeugen Sie bitte einen Bericht, der idealerweise zu Folgeaufträgen führt!

pacman::p_load(tidyverse,DescTools,ggbeeswarm,ggsignif, wrappedtools)

rawdata <- d.pizza %>% 
  as_tibble() %>% 
  mutate(wine_ordered=factor(wine_ordered,levels=c(1,0),labels=c('yes','no')),
         weekend=ifelse(test = weekday<6,yes = 'no',no = 'yes') %>% factor(),
         weekday=factor(weekday,levels=1:7,labels=day.name))

# Grafiken zu wine_ordered #####
ggplot(rawdata,aes(wine_ordered))+
  geom_bar()
ggplot(rawdata,aes(wine_ordered,delivery_min))+
  geom_boxplot(aes(fill=wine_ordered))+
  geom_beeswarm(aes(color=wine_ordered),alpha=.5,cex = .8)
#loop
quantvars_names <- c('delivery_min', 'temperature', 'price')
results=tibble(variable=quantvars_names,
               wine='',`no wine`='',p='')
for(var_i in seq_along(quantvars_names)){
  results[var_i,c('wine','no wine')] <- 
    by(rawdata[[quantvars_names[var_i]]],
       rawdata$wine_ordered,median_quart) %>% as.list()
  p <-  wilcox.test(
    as.formula(paste0(quantvars_names[var_i],'~wine_ordered')),
    data=rawdata)$p.value %>% formatP()
  results[var_i,'p'] <- p

  plottmp <- 
    rawdata %>% 
    filter(!is.na(wine_ordered)) %>%
    ggplot(aes_string('wine_ordered',quantvars_names[var_i]))+
    geom_boxplot(outlier.alpha = 0)+
    geom_beeswarm(alpha=.25,cex = .8)+
    geom_signif(comparisons = list(c(1,2)),
                annotations =  p)+
    scale_y_continuous(expand = expansion(mult = c(.05,.1)))
  print(plottmp)
}
rawdata %>% 
  filter(!is.na(wine_ordered),!is.na(weekend)) %>% 
  ggplot(aes(wine_ordered, fill=weekend))+
  geom_bar(position='fill')+
  scale_y_continuous(labels = scales::percent)
rawdata %>% 
  filter(!is.na(wine_ordered),!is.na(weekday)) %>% 
  ggplot(aes(wine_ordered, fill=weekday))+
  geom_bar(position='fill')+
  scale_y_continuous(labels = scales::percent)
rawdata %>% 
  filter(!is.na(wine_ordered),!is.na(weekday)) %>% 
  ggplot(aes(fill=wine_ordered, x=weekday))+
  geom_bar(position='fill')+
  scale_y_continuous(labels = scales::percent)

#Grafiken zu weekday####
for(var_i in seq_along(quantvars_names)){
  plottmp <- 
    rawdata %>% 
    filter(!is.na(weekday)) %>% 
    ggplot(aes_string('weekday',quantvars_names[var_i]))+#,
                     # color='wine_ordered'))+
    geom_boxplot(outlier.alpha = 0)+
    geom_beeswarm(alpha=.5,cex = .8,
                  dodge.width = .75)
  print(plottmp)
}
rawdata %>% 
  # filter(!is.na(weekday)) %>% 
  ggplot(aes(delivery_min,temperature))+
  geom_point(aes(color=weekend), alpha=.7)+
  geom_smooth(color='darkblue')+
  geom_smooth(method='lm', color='darkgreen')
  