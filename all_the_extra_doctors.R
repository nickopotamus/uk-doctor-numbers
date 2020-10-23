# Basic data handling
library(tidyverse)
library(readxl)
library(lubridate)

#Make plots pretty
library(viridis)
library(RColorBrewer)

# For multiple plots
library(gridExtra)
library(cowplot)

# Get workforce data from 
# https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/july-2020
# and tidy up
url <- "https://files.digital.nhs.uk/78/B3D48D/NHS%20Workforce%20Statistics%2C%20July%202020%20Doctors%20by%20Grade%20and%20Specialty.xlsx"
download.file(url, destfile = "raw_stats.xlsx") 
raw_data <- read_excel("raw_stats.xlsx", sheet="Data")
processed <- raw_data %>% 
  mutate(date = ymd(Date), grade = factor(Grade), spec_group = factor(`Specialty Group`), spec = factor(Specialty)) %>% 
  select (date, grade, spec_group, spec, FTE) %>% 
  mutate(grade = fct_collapse(fct_inorder(grade), 
    Consultant = 'Consultant', 
    SAS = c("Staff Grade", "Specialty Doctor", 'Associate Specialist', "Other and Local HCHS Doctor Grades"),
    Registrar = "Specialty Registrar",
    Core = 'Core Training',
    FY2 = 'Foundation Doctor Year 2',
    FY1 = 'Foundation Doctor Year 1',
    Practitioner = 'Hospital Practitioner / Clinical Assistant'))


## Basic summary table
all_docs <- processed %>% select(date, grade, FTE) %>% group_by(date, grade) %>% summarise(FTE = sum(FTE))

ggplot(all_docs, aes(x=date, y=FTE, fill=grade)) +
  geom_area() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Number of UK doctors since 2010",
       subtitle = "Visualisation by @Nickopotamus. Data: digital.nhs.uk.") +
  xlab("Date of report") +
  ylab("Full time equivalents") +
  theme_minimal() +
  scale_fill_viridis(discrete = T) +
  theme(legend.title = element_blank()) 

## Since 2019
# https://www.gov.uk/government/news/over-13700-more-nurses-working-in-the-nhs
# claims "Over the last year (July 2019 to July 2020)... the number of doctors 
# has gone up by 7,810, from 112,797 to 120,607"
all_docs_since_2019 <- filter(all_docs, date > "2019-06-01")

ggplot(all_docs_since_2019, aes(x=date, y=FTE, group=grade, color=grade)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  labs(title = "Number of UK doctors by grade since July 2019",
       subtitle = "Visualisation by @Nickopotamus. Data: digital.nhs.uk.") +
  xlab("Date of report") +
  ylab("Full time equivalents") +
  theme_minimal() +
  scale_color_viridis(discrete = T) +
  theme(legend.title = element_blank()) 

## By speciality
by_spec <- processed %>% select(date, spec_group, FTE) %>% group_by(date, spec_group) %>% summarise(FTE = sum(FTE))

ggplot(by_spec, aes(x=date, y=FTE, fill=spec_group)) +
  geom_area() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Number of UK doctors by speciality since 2010",
       subtitle = "Visualisation by @Nickopotamus. Data: digital.nhs.uk.") +
  xlab("Date of report") +
  ylab("Full time equivalents") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Paired")

ggplot(by_spec, aes(x=date, y=FTE, group=spec_group, color=spec_group)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Number of UK doctors by grade since 2010",
       subtitle = "Visualisation by @Nickopotamus. Data: digital.nhs.uk.") +
  xlab("Date of report") +
  ylab("Full time equivalents") +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  theme(legend.title = element_blank()) 

## Plot for specific specialities only
anaes <- filter(processed, spec_group == "Anaesthetics") %>% 
  select(date, grade, FTE) %>% group_by(date, grade) %>% summarise(FTE = sum(FTE))
genmed <- filter(processed, spec_group == "General medicine group") %>% 
  select(date, grade, FTE) %>% group_by(date, grade) %>% summarise(FTE = sum(FTE))
gensurg <- filter(processed, spec_group == "Surgical group") %>% 
  select(date, grade, FTE) %>% group_by(date, grade) %>% summarise(FTE = sum(FTE))
em <- filter(processed, spec_group == "Emergency Medicine") %>% 
  select(date, grade, FTE) %>% group_by(date, grade) %>% summarise(FTE = sum(FTE))

plot_anaes <- ggplot(anaes, aes(x=date, y=FTE, group=grade, color=grade)) +
  geom_line() +
  labs(title = "Anaesthetics") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  xlab("Date") + ylab("FTE doctors") +
  theme_minimal() + scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme(legend.title = element_blank()) 

plot_genmed <- ggplot(genmed, aes(x=date, y=FTE, group=grade, color=grade)) +
  geom_line() +
  labs(title = "General medicine") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  xlab("Date") + ylab("FTE doctors") +
  theme_minimal() + scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme(legend.position="none")

plot_gensurg <- ggplot(gensurg, aes(x=date, y=FTE, group=grade, color=grade)) +
  geom_line() +
  labs(title = "Surgery") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  xlab("Date") + ylab("FTE doctors") +
  theme_minimal() + scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme(legend.position="none")

plot_em <- ggplot(em, aes(x=date, y=FTE, group=grade, color=grade)) +
  geom_line() +
  labs(title = "Emergency medicine") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  xlab("Date") + ylab("FTE doctors") +
  theme_minimal() + scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme(legend.position="none")

legend <- get_legend(plot_anaes)

plot_anaes <- plot_anaes + theme(legend.position="none")

grid.arrange(plot_genmed, plot_gensurg, plot_anaes, plot_em, legend, 
             ncol=3, widths=c(2.3, 2.3, 0.8), 
             layout_matrix = cbind(c(1,2), c(3,4), 5))
