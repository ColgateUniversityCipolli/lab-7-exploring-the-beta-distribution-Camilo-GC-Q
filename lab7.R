library(tidyverse)

# Task 1: Describe the Population Distribution

distribution = function(alpha, beta){
  q1.fig.dat = tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta))
      
  ggplot(data= q1.fig.dat)+ # specify data
    geom_line(aes(x=x, y=beta.pdf, color= paste("Beta Dist(", alpha, ", ", beta, ")")) + # plot beta dist
    geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) + # plot guassian dist
    geom_hline(yintercept=0)+ # plot x axis
    theme_bw()+ # change theme
    xlab("x")+ # label x axis
    ylab("Density")+ # label y axis
    scale_color_manual("", values = c("black", "grey"))+ # change colors
    theme(legend.position = "bottom") # move legend to bottom
}

         
         
         
         
