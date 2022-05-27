library(ggplot2)
library(ggpubr)
library("stringr") 
library(tidyr)

setwd('ALTER....')


##
## Figure 1
##

#Read the data
raw_result_data1 <- read.csv(file = 'data_figure_1_n.csv', sep=';', check.names = FALSE)

# Manipulate the data
raw_result_data1$Date <- factor(raw_result_data1$Date, levels = unique(raw_result_data1$Date))
raw_result_data1.long <- pivot_longer(raw_result_data1, cols=2:8, names_to = "Article", values_to = "Fine")

raw_result_data1.long = raw_result_data1.long %>% group_by(Date) %>% mutate(r.fine = Fine/sum(Fine))
raw_result_data1.long = raw_result_data1.long %>% group_by(Date, Article) %>% mutate(s.fine = sum(Fine))

# Plot the data
gdpr.fines.over.time.plot <-ggplot(data=raw_result_data1.long, aes(x=Date, y=r.fine*100, fill=Article)) +
  geom_bar(position="stack", stat="identity", colour="black", alpha = 0.6) + 
  geom_line(data=raw_result_data1.long, aes(x=Date, y=s.fine, group=Article), size=1.5) +
  geom_line(data=raw_result_data1.long, aes(x=Date, y=s.fine, group=Article, color = Article), size=1)+
  scale_y_continuous( name = 'Relative Amount of Penalties [%]', sec.axis = sec_axis(~. *.95, name = "Number of Penalties"))+
  theme_pubr() +
  xlab("") +
  labs_pubr(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.text=element_text(size=12, face = "bold"))
# gdpr.fines.over.time.plot 
ggsave("number-amount-gdpr-fines-over-time.pdf", plot = gdpr.fines.over.time.plot , dpi = 600, device="pdf")



##
## Figure 3
##

#Read the data
raw_result_data3 <- read.csv(file = 'gdpr_enforcement_tracker_export_full_list.csv', sep=',', check.names = FALSE, encoding="UTF-8")

# Manipulate the data
raw_result_data3_print=head(raw_result_data3, 20)

raw_result_data3_print$Frequency <- factor(raw_result_data3_print$Frequency, 
                                           levels = raw_result_data3_print$Frequency[order(raw_result_data3_print$Frequency)])

# Plot the data
word.frequency.plot = ggplot(data=raw_result_data3_print, aes(x=reorder(Word, Frequency) , y=Frequency, label=Frequency)) +
  geom_bar(stat="identity") + 
  geom_text(size = 5, position = position_stack(vjust = 1.043)) +
  xlab("Word Stem") + 
  coord_flip() +
  theme(text=element_text(family="Serif")) +
  theme_pubr() +
  labs_pubr(base_size = 16)
word.frequency.plot

ggsave("word_frequency_plot.pdf", plot = word.frequency.plot, dpi = 600, device="pdf")


##
## Figure 4
##

# Read the data
raw_result_data4 <- read.csv(file = 'access_analysis_data.csv', sep=';')

# Manipulate the data
raw_result_data4$Category <- factor(raw_result_data4$Category, levels = raw_result_data4$Category)

# Plot the data
access.analysis.plot<-ggplot(data=raw_result_data4, aes(x=Category , y=Occurrence, fill=Type, label=Occurrence)) +
  geom_bar(stat="identity", colour="black")+ 
  geom_text(size = 5, position = position_stack(vjust = 0.5))+
  coord_flip() +
  theme(text=element_text(family="Serif")) +
  theme_pubr() +
  labs_pubr(base_size = 16) +
  scale_fill_brewer(type = "seq", palette = "Greys") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 45), expand=c(0,0))+
  theme(legend.text=element_text(size=14, lineheight = .8, face = "bold"), 
          legend.position = c(0.5,0.1), 
          legend.direction="vertical",
          plot.margin=unit(c(0,0,0,0),"mm"),)
# access.analysis.plot

ggsave("access-analysis.pdf", plot = access.analysis.plot, dpi = 600, device="pdf",
       width = 23,
       height = 20,
       units = c("cm"))