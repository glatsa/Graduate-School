# load Library
# if GIF is not being create Follow the below instructions
# https://stackoverflow.com/questions/58991580/gganimate-returns-png-files-but-no-animated-object

# to create the plots 
library(ggplot2)
library(gganimate)

# to create the GIF
library(gifski)
library(png)

# to create the Video
library(av)

#Baisc Animation
############################################################

p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()
plot(p)

anim <- p + transition_states(Species,
                    transition_length = 2,
                    state_length = 1)
animate(anim, renderer = gifski_renderer(loop = T))
# Example taken from https://gganimate.com/articles/gganimate.html

#############################################################

theme_set(theme_bw())
library(gapminder)
head(gapminder)

#############################################################

# Create Plot
p <- ggplot(gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
View(gapminder)

# Create Animation
r <- p + transition_time(year)

# animate(r, renderer = gifski_renderer(loop = T))# Continous Loop
animate(r, renderer = gifski_renderer(loop = F)) # No Loop

# Example from: https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/

##################################################################

getwd()
#setwd()
# saving Animation
anim_save("title.GIF", animation = last_animation(), 'Directory')

##################################################################

# several Various Types of Animation
## Adding a Changing Title
anim <- r + labs(title = "Year: {frame_time}")
animate(anim, renderer = gifski_renderer(loop = F))

## Easing
anim <- r + ease_aes('cubic-in-out')
animate(anim, renderer = gifski_renderer(loop = F))

## Not Smooth/Slow
anim <- p +  transition_states(year,  4, 1)
animate(anim,duration = 20, renderer = gifski_renderer(loop = F))

## Enter & Exit
anim <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point(aes(colour = Species), size = 2) + 
  transition_states(Species, 2, 1)
anim <- anim + enter_fade() + exit_shrink()
animate(anim,duration = 20, renderer = gifski_renderer(loop = F))

## Video

getwd()
#setwd()
vid <- animate(anim, renderer = av_renderer('Title.mp4'), 
        width = 1280, height = 720, res = 104, fps = 25)

# if you need additional help go to the below pages
# More Documentation https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=2ahUKEwjikumY8_3nAhVBLKwKHdQzCDgQFjAAegQIAhAB&url=https%3A%2F%2Fcran.r-project.org%2Fweb%2Fpackages%2Fgganimate%2Fgganimate.pdf&usg=AOvVaw3Bxql771jAZ2CVWOMvHVYG


