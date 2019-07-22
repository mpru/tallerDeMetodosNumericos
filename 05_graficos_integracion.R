library(ggplot2)
library(dplyr)
library(polynom)

# colores lindos 
# col_list<-c("#FF222C", "#1DFBFF", "#FDFF24", "#2CFF18", "#FF38F4", "#C3C4C9", "#000000")

#----------------------------
# Curva y puntos
#----------------------------

f <- function(x) 2 + exp((1-x)/4) * sin(3 * x)
d <- data_frame(x = seq(0, by = .5, length.out = 13), y = f(x))
recta2pts <- function(x0, y0, x1, y1) {
    m = (y1 - y0) / (x1 - x0)
    h = y0 - x0 * m
    function(x) h + m * x
}

g <- ggplot(d, aes(x = x, y = y)) + 
    stat_function(fun = f, aes(color = "f(x) real"), lwd = .8) + 
    geom_point(aes(color = "Puntos\n tabulados"), size = 1.5) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4)) +
    scale_x_continuous(expand = c(0, 0), limits = c(-.3, 6.5), breaks = seq(0, 6, 0.5)) +
    scale_color_discrete("") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
ggsave("Plots/Unidad4_2_g.pdf", g, width = 4.5, height = 2.5)

#----------------------------
# Trapecial
#----------------------------

g1 <- g +
    geom_polygon(data = data_frame(x = c(d$x[1], d$x[1], d$x[2], d$x[2], d$x[1]),
                                   y = c(0, d$y[1], d$y[2], 0, 0)), 
                 fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8)+
    annotate("text", x = 0.25, y = .5, label = "A[1]", parse = T)
ggsave("Plots/Unidad4_2_g1.pdf", g1, width = 4.5, height = 2.5)

g2 <- g1 + 
    stat_function(fun = recta2pts(d$x[1], d$y[1], d$x[2], d$y[2]),
                  aes(color = "Recta de\n interpolación"))  # recta que pasa por x0 y x1
ggsave("Plots/Unidad4_2_g2.pdf", g2, width = 4.5, height = 2.5)   


g3 <- g +
    geom_polygon(data = data_frame(x = c(d$x[2], d$x[2], d$x[3], d$x[3], d$x[2]),
                                   y = c(0, d$y[2], d$y[3], 0, 0)), 
                 fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8)+
    annotate("text", x = 0.75, y = .5, label = "A[2]", parse = T) + 
    stat_function(fun = recta2pts(d$x[2], d$y[2], d$x[3], d$y[3]),
                  aes(color = "Recta de\n interpolación")) 
ggsave("Plots/Unidad4_2_g3.pdf", g3, width = 4.5, height = 2.5)  

g4 <- g
for (i in 2:nrow(d)) {
    g4 <- g4 + 
        geom_polygon(data = data_frame(x = c(d$x[i-1], d$x[i-1], d$x[i], d$x[i], d$x[i-1]),
                                       y = c(0, d$y[i-1], d$y[i], 0, 0)), 
                     fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .5)+
        annotate("text", x = 0.25 + (i-2)*0.5, y = .5, label = paste0("A[", i-1, "]"), parse = T)
}
g4
ggsave("Plots/Unidad4_2_g4.pdf", g4, width = 4.5, height = 2.5)  


#----------------------------
# Simpson 1/3
#----------------------------

# polinomio que pasa por 3 ptos (lagrange, equivalente a newton)
poli <- as.function(poly.calc(d$x[1:3], d$y[1:3]))


g5 <- g +
    geom_area(stat = "function", 
              fun = as.function(poly.calc(d$x[1:3], d$y[1:3])), 
              xlim = d$x[c(1, 3)], 
              fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8) +
    annotate("text", x = 0.5, y = .5, label = "A[1]", parse = T) +
    stat_function(fun = as.function(poly.calc(d$x[1:3], d$y[1:3])),
                  aes(color = "Polinomio de\n interpolación"))  
ggsave("Plots/Unidad4_2_g5.pdf", g5, width = 4.5, height = 2.5)   

g6 <- g +
    geom_area(stat = "function", 
              fun = as.function(poly.calc(d$x[3:5], d$y[3:5])), 
              xlim = d$x[c(3, 5)], 
              fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8) +
    annotate("text", x = 1.5, y = .5, label = "A[2]", parse = T) +
    stat_function(fun = as.function(poly.calc(d$x[3:5], d$y[3:5])),
                  aes(color = "Polinomio de\n interpolación"))  
ggsave("Plots/Unidad4_2_g6.pdf", g6, width = 4.5, height = 2.5)   

g7 <- g
for (i in seq(2, nrow(d)-1, 2)) {
    g7 <- g7 +
        geom_area(stat = "function", 
                  fun = as.function(poly.calc(d$x[(i-1):(i+1)], d$y[(i-1):(i+1)])), 
                  xlim = d$x[c(i-1, i+1)], 
                  fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8) +
        annotate("text", x = (i-1)/2, y = .5, label = paste0("A[", i/2, "]"), parse = T) 
}
ggsave("Plots/Unidad4_2_g7.pdf", g7, width = 4.5, height = 2.5)

#----------------------------
# Simpson 3/8
#----------------------------

g8 <- g +
    geom_area(stat = "function", 
              fun = as.function(poly.calc(d$x[1:4], d$y[1:4])), 
              xlim = d$x[c(1, 4)], 
              fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8) +
    annotate("text", x = 0.75, y = .5, label = "A[1]", parse = T) +
    stat_function(fun = as.function(poly.calc(d$x[1:4], d$y[1:4])),
                  aes(color = "Polinomio de\n interpolación"))  
ggsave("Plots/Unidad4_2_g8.pdf", g8, width = 4.5, height = 2.5)   

g9 <- g +
    geom_area(stat = "function", 
              fun = as.function(poly.calc(d$x[4:7], d$y[4:7])), 
              xlim = d$x[c(4, 7)], 
              fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8) +
    annotate("text", x = 2.25, y = .5, label = "A[2]", parse = T) +
    stat_function(fun = as.function(poly.calc(d$x[4:7], d$y[4:7])),
                  aes(color = "Polinomio de\n interpolación"))  
ggsave("Plots/Unidad4_2_g9.pdf", g9, width = 4.5, height = 2.5)   

g10 <- g
pos <- numeric() # a mano xq no me doy cuenta
pos[seq(1, nrow(d)-2, 3)] <- c(0.75, 2.25, 3.75, 5.25)
etiq <- character()
etiq[seq(1, nrow(d)-2, 3)] <- c("A[1]", "A[2]", "A[3]", "A[4]")
for (i in seq(1, nrow(d)-2, 3)) {
    g10 <- g10 +
        geom_area(stat = "function", 
                  fun = as.function(poly.calc(d$x[i:(i+3)], d$y[i:(i+3)])), 
                  xlim = d$x[c(i, i+3)], 
                  fill = "#2CFF18", alpha = 0.3, color = "#2CFF18", lty = 2, lwd = .8) +
        annotate("text", x = pos[i], y = .5, label = etiq[i], parse = T)
}
ggsave("Plots/Unidad4_2_g10.pdf", g10, width = 4.5, height = 2.5)
