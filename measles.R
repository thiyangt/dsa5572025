library(ggplot2)
library(magrittr)
library(tidyverse)
# Read the data 
measles = readr::read_csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/refs/heads/master/measles_data_1928-2011.csv")

# Show the first few rows of the data
print(head(measles))

cols <- c(
  colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421"))(20),
  colorRampPalette(c("#e29421", "#f05336", "#ce472e"))(80)
)

ggplot(measles, aes(x = Year, y = State, fill = value)) + 
  geom_tile(colour = "white", linewidth = 0.5, 
            width = .9, height = .9) + 
  theme_minimal() +
  scale_fill_gradientn(colours = cols,
                       limits = c(0, 4000),
                       breaks = seq(0, 4000, by = 1000),
                       labels = c("0k", "1k", "2k", "3k", "4k"),
                       na.value = rgb(246/255, 246/255, 246/255),
                       guide = guide_colourbar(ticks = TRUE, 
                                               nbin = 50,
                                               barheight = .5, 
                                               label = TRUE, 
                                               barwidth = 10,
                                               title = "Cases per million",
                                               title.position = "top",
                                               title.hjust = 0.5)) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(1930, 2010, by = 10),
                     limits = c(1928, 2012)) +
  geom_vline(xintercept = 1963, color = "black", size = 0.5)
###########
ggplot(measles, aes(x=Year, y=State, fill=value))+
  geom_tile()
#####
ggplot(measles, aes(x=Year, y=State, fill=value))+
geom_tile(colour="white", size=0.25)+
  #remove x and y axis labels
  labs(x="", y="")+
  #remove extra space
  scale_y_discrete(expand=c(0, 0))+
  #define new breaks on x-axis
  scale_x_discrete(expand=c(0, 0),
                   breaks=c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000"))+
  #set a base size for all fonts
  theme_grey(base_size=8)+
  #theme options
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank()
  )

####
m4 <- measles %>%
  # convert state to factor and reverse order of levels
  mutate(state=factor(State, levels=rev(sort(unique(State))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(value, breaks=c(-1, 0, 1, 10, 100, 500, 1000, max(value, na.rm=T)),
                         labels=c("0", "0-1", "1-10", "10-100", "100-500", "500-1000", ">1000"))) %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor), levels=rev(levels(countfactor))))
####
# assign text colour
textcol <- "grey40"

# further modified ggplot
p <- ggplot(m4, aes(x=Year, y=State, fill=countfactor))+
  geom_tile(colour="white", size=0.2)+
  guides(fill=guide_legend(title="Cases per\n100,000 people"))+
  labs(x="", y="", title="Incidence of Measles in the US")+
  scale_y_discrete(expand=c(0, 0))+
  scale_x_discrete(expand=c(0, 0), breaks=c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000"))+
  scale_fill_manual(values=c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#ddf1da"), na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=10)+
  theme(legend.position="right", legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0, "cm")),
        legend.text=element_text(colour=textcol, size=7, face="bold"),
        legend.key.height=grid::unit(0.8, "cm"),
        legend.key.width=grid::unit(0.2, "cm"),
        axis.text.x=element_text(size=10, colour=textcol),
        axis.text.y=element_text(vjust=0.2, colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title=element_text(colour=textcol, hjust=0, size=14, face="bold")
  )
p
