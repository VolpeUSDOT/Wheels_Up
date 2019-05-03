# Figures for May 7 C-70 Meeting

# Set up ----
library(tidyverse)
library(gridExtra) # for grid.arrange of multiple ggplots

codeloc = ifelse(grep('Flynn', normalizePath('~/')),
                 "~/git/Wheels_Up",
                 "Erika_put_your_path_here/Wheels_Up")
sharedloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Data"
figloc = "//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance/Figures"


avail_data = dir(sharedloc)
avail_data = avail_data[grep('^ASQP.\\d{4}.RData$', avail_data)] # Match with regular expresion to only prepared .RData files

if(length(avail_data) == 0) {
  source(file.path(codeloc, 'datacleaning', 'Data_prep.R'))
} else {
  for(x in avail_data){
    load(file.path(sharedloc, x))
  }
  load(file.path(sharedloc, 'Cross-year_Summary.RData'))
}


# DCA - LAX, 
# DCA - BOS
# Histograms of airtime, overlay by different years.
# Or facet carrier,
# maybe violin charts 

# Example airport airtime figs ----

# Function to make frequency plots of air time by pairs of origin and destination, for a given year. 
# origin = "DCA"; dest = "BOS"; year = 18
# Options: three kinds of plots of frequency, select historgram, kernel density, or 'frequency polygon' plot. Function defaults to histogram and only produces one type.

make_od_hist <- function(origin, dest, year,
                         plottype = c('hist_plot', 'density_plot', 'freqpoly_plot')){
  
  # create od_xx from d_xx, where xx is year
  assign(paste0("od_", year),
         get(paste0("d_", year)) %>%
               filter(ORIGIN == origin & DEST == dest) 
         )
  
  od_xx <- get(paste0("od_", year))

  # opposite direction, create do_xx from d_xx, where xx is year
  assign(paste0("do_", year),
         get(paste0("d_", year)) %>%
           filter(ORIGIN == dest & DEST == origin) 
  )
  
  do_xx <- get(paste0("do_", year))
  
  suppressMessages( oddo <- full_join(od_xx, do_xx) )

  cbPalette = scales::alpha(c('navyblue', 'firebrick'), 0.8)

  hist_plot <- ggplot(oddo, aes(x = AIR_TIME, fill = ORIGIN)) + 
    geom_histogram() +
    scale_fill_manual(values = cbPalette,
                      name = "Origin airport",
                      breaks = c(origin, dest),
                      labels = c(paste0(origin, ' (', nrow(od_xx), ')'), 
                                 paste0(dest,   ' (', nrow(do_xx), ')'))) +
      theme_bw() +
    xlab("Air time (minutes)") + ylab("Frequency of flights") +
    ggtitle(paste("Distribution of air time for \n", origin, '<>', dest, "flights in", paste0(20, year)))
  
  density_plot <- ggplot(oddo, aes(x = AIR_TIME, fill = ORIGIN)) + 
    geom_density() +
    scale_fill_manual(values = cbPalette,
                      name = "Origin airport",
                      breaks = c(origin, dest),
                      labels = c(paste0(origin, ' (', nrow(od_xx), ')'), 
                                 paste0(dest,   ' (', nrow(do_xx), ')'))) +
    theme_bw() +
    xlab("Air time (minutes)") + ylab("Relative frequency of flights") +
    ggtitle(paste("Distribution of air time for \n", origin, '<>', dest, "flights in", paste0(20, year)))

  freqpoly_plot <- ggplot(oddo, aes(x = AIR_TIME, color = ORIGIN)) +
    geom_freqpoly(size = 1, binwidth = 2, show.legend = T) +
    scale_color_manual(values = cbPalette,
                       name = "Origin airport",
                       breaks = c(origin, dest),
                       labels = c(paste0(origin, ' (', nrow(od_xx), ')'), 
                                  paste0(dest,   ' (', nrow(do_xx), ')'))) +
    theme_bw() +
    xlab("Air time (minutes)") + ylab("Frequency of flights") +
    ggtitle(paste("Distribution of air time for \n", origin, '<>', dest, "flights in", paste0(20, year)))
  
  return(get(plottype))
}

# Make plots ----
# Legend not showing for freqpoly, skip it...

make_od_hist('DCA', 'BOS', 18, 'hist_plot')
make_od_hist('DCA', 'BOS', 18, 'density_plot')
# make_od_hist('DCA', 'BOS', 18, 'freqpoly_plot')

p1 <- make_od_hist('LAX', 'DCA', 18, 'density_plot')
p2 <- make_od_hist('LAX', 'BOS', 18, 'density_plot')
p3 <- make_od_hist('DCA', 'BOS', 18, 'density_plot')

gridarr1 <- grid.arrange(p1, p2)
ggsave(file = file.path(figloc, 'LAX_DCA_BOS_Flight_Times_1.jpg'),
       plot = gridarr1)
gridarr2 <- grid.arrange(p1, p2, p3)
ggsave(file = file.path(figloc, 'LAX_DCA_BOS_Flight_Times_2.jpg'),
       plot = gridarr2)

p1 <- make_od_hist('LAX', 'BWI', 18, 'density_plot')
p2 <- make_od_hist('LAX', 'BOS', 18, 'density_plot')
p3 <- make_od_hist('BOS', 'BWI', 18, 'density_plot')

gridarr1 <- grid.arrange(p1, p2)
ggsave(file = file.path(figloc, 'LAX_BWI_BOS_Flight_Times_1.jpg'),
       plot = gridarr1)
gridarr2 <- grid.arrange(p1, p2, p3)
ggsave(file = file.path(figloc, 'LAX_BWI_BOS_Flight_Times_2.jpg'),
       plot = gridarr2)

# repeat, now historgrams

p1 <- make_od_hist('LAX', 'DCA', 18)
p2 <- make_od_hist('LAX', 'BOS', 18)
p3 <- make_od_hist('DCA', 'BOS', 18)

gridarr1 <- grid.arrange(p1, p2)
ggsave(file = file.path(figloc, 'LAX_DCA_BOS_Flight_Times_1_hist.jpg'),
       plot = gridarr1)
gridarr2 <- grid.arrange(p1, p2, p3)
ggsave(file = file.path(figloc, 'LAX_DCA_BOS_Flight_Times_2_hist.jpg'),
       plot = gridarr2)

p1 <- make_od_hist('LAX', 'BWI', 18)
p2 <- make_od_hist('LAX', 'BOS', 18)
p3 <- make_od_hist('BOS', 'BWI', 18)

gridarr1 <- grid.arrange(p1, p2)
ggsave(file = file.path(figloc, 'LAX_BWI_BOS_Flight_Times_1_hist.jpg'),
       plot = gridarr1)
gridarr2 <- grid.arrange(p1, p2, p3)
ggsave(file = file.path(figloc, 'LAX_BWI_BOS_Flight_Times_2_hist.jpg'),
       plot = gridarr2)

# Make comparision for a few years, too
p1 <- make_od_hist('LAX', 'DCA', 15)
p2 <- make_od_hist('LAX', 'DCA', 16)
p3 <- make_od_hist('LAX', 'DCA', 17)
p4 <- make_od_hist('LAX', 'DCA', 18)

gridarr1 <- grid.arrange(p1, p2, p3, p4, 
                         nrow = 2)
ggsave(file = file.path(figloc, 'LAX_DCA_2015-2018_hist.jpg'),
       plot = gridarr1)

gridarr1 <- grid.arrange(p1, p4, 
                         nrow = 2)
ggsave(file = file.path(figloc, 'LAX_DCA_2015-2018_2_hist.jpg'),
       plot = gridarr1)

# Tables ---
# Make some summary tables to confirm these values

d_18 %>% 
  filter(ORIGIN == "DCA" & DEST == "BOS") %>% 
  summarize(max_AT = max(AIR_TIME, na.rm=T),
            mean_AT = mean(AIR_TIME, na.rm=T),
            min_AT = min(AIR_TIME, na.rm=T))
#  120    63.4     43

# BOS -> DCA typically longer by 10 minutes than reverse
d_18 %>% 
  filter(ORIGIN == "BOS" & DEST == "DCA") %>% 
  summarize(max_AT = max(AIR_TIME, na.rm=T),
            mean_AT = mean(AIR_TIME, na.rm=T),
            min_AT = min(AIR_TIME, na.rm=T))
# 144    73.6     51

# LAX -> BOS can be as short as 242 min = 4.03 hrs
d_18 %>% 
  filter(ORIGIN == "LAX" & DEST == "BOS") %>% 
  summarize(max_AT = max(AIR_TIME, na.rm=T),
            mean_AT = mean(AIR_TIME, na.rm=T),
            min_AT = min(AIR_TIME, na.rm=T))
#  max_AT mean_AT min_AT
# <dbl>   <dbl>  <dbl>
#  388    298.    242

# Yes, BOS -> LAX can be as long as 425 min = 7.08 hours
d_18 %>% 
  filter(ORIGIN == "BOS" & DEST == "LAX") %>% 
  summarize(max_AT = max(AIR_TIME, na.rm=T),
            mean_AT = mean(AIR_TIME, na.rm=T),
            min_AT = min(AIR_TIME, na.rm=T))
#  max_AT mean_AT min_AT
# <dbl>   <dbl>  <dbl>
#  425    348.    302
