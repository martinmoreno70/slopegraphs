##code adapted from https://ibecav.github.io/slopegraph/

library(readxl)
library(magrittr)
library(tidyr)
library(ggrepel)
library(dplyr)
library(plotly)

library(extrafont)
library(extrafontdb)
loadfonts(device = "win")

wkdir <- "E:/"
setwd(wkdir)

#maths
file<-"data.xlsx"
sheetname<-"data"
rangename<-"a1:d22"
titlename<-"Teachers: Math Pre and Post Tests Results"

mydata<- read_excel(paste0(wkdir,file), sheet = sheetname, range = rangename)
names(mydata)[1:2]<-c("item","topic")
mydata$topic <- factor(mydata$topic)
mydata<- mydata %>% mutate(diff = `Post-Test` - `Pre-Test` ) %>%        
  mutate(sep=ifelse(diff>25,"Growth","Same"))

#reshape data long to wide
data <- gather(mydata, test, value , c("Pre-Test", "Post-Test"), factor_key=TRUE)


######################################################################


theme_slope <- function(){
  theme_bw() +
    theme(
      text = element_text(family = "Roboto Condensed Light"), 
      title = element_text(colour = "gray25", face = "bold", family = "Roboto", size = 14),
      plot.title       = element_text(size=12, face = "bold", hjust = 0.5, family="Roboto", color="black") ,
      plot.subtitle    = element_text(size=11, face = "bold", hjust = 0.5, family = "Roboto", color = "gray40"),
      plot.caption     = element_text(size=10, face = "plain", family = "Roboto Condensed", color = "gray40" ),
      strip.background = element_rect(fill = "gray95", color = "gray95"),
      panel.background = element_blank(),
      panel.grid=element_blank(),
      axis.ticks=element_blank(),
      axis.text=element_blank(),
      axis.title.y     = element_blank() ,
      axis.title.x     = element_blank() ,
      axis.text.x.top  = element_text(size=10) ,
      axis.text.y      = element_blank() ,
      panel.border=element_blank(),
      panel.grid.major.y = element_blank() ,
      panel.grid.minor.y = element_blank() ,
      panel.grid.major.x = element_blank() ,
      legend.position = "none"
    )
}



titlename<-"Maths Pre and Post Tests Results by Items"

graph<- ggplot(data = data, aes(x = test, y = value, group = item, color=sep )) +
  geom_line(aes( alpha = 1), size = 1) +
  scale_color_manual(values = c("black", "cadetblue")) +
  geom_text_repel(data = data %>% filter( sep=="Same" & test=="Pre-Test") , 
                  aes(label = paste0("[", item, "] ", topic, " | ", diff, "pp"), family="Roboto"), 
                  hjust = "right", 
                  colour = "cadetblue" ,
                  segment.colour = "cadetblue" , 
                  size = 2.5, 
                  nudge_x = -.05, 
                  direction = "y") +
  geom_text_repel(data = data %>% filter( sep=="Growth" & test=="Post-Test") , 
                  aes(label = paste0("[", item, "] ", topic, " | ", diff, "pp"), family="Roboto"), 
                  hjust = "left", 
                  colour = "black" ,
                  segment.colour = "black" , 
                  size = 2.5, 
                  nudge_x = .05, 
                  direction = "y") +
  geom_label(aes(label = value, family="Roboto" ), 
             size = 2.5, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +                  
  scale_x_discrete(position = "top") +
  labs( title = titlename, subtitle = "", 
        caption = "Note: Colors of elements indicate size of change between pre and post tests \n in percentage points (pp): grey<=25pp, black>25pp \n Mart?n Moreno" )  +
  theme_slope()
graph

png(filename="slopegraph.png", 
    type="cairo",
    units="in", 
    width=9.5, 
    height=7, 
    pointsize=12, 
    res=400)
graph
dev.off()
