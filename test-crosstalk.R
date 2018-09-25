library(crosstalk)
library(plotly)
library(DT)

d <- data.frame(x=1:10,y=1:10,f=gl(2,5,labels = letters[1:2]))
sd <- SharedData$new(d)

# options(persistent = TRUE)

p <- ggplot(sd, aes(x, y)) +
  geom_text(aes(label=f)) +
  theme_void() 

bscols(
  ggplotly(p) %>%
    highlight(color = "red",on = "plotly_click"),
  datatable(sd, style="bootstrap", class="compact", width="100%",
            options=list(deferRender=FALSE, dom='t',
                           paging = FALSE,
                           ordering=FALSE,
                           info=FALSE,
                           scrollX=FALSE, 
                           scrollY=FALSE,
                           searching=TRUE
                         
                         
                         
                         
                         )))
