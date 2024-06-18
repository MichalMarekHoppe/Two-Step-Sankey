## Two-Step Sankey
## by Dr Michal Marek Hoppe, 2024 

library(ggplot2)

## load data ----
data <- 
  data.frame(lapply(read.csv("data.csv", row.names = 1), function (x) {
    factor(x,
           levels = unname(unlist(read.csv("categories.csv",
                                           row.names = NULL,
                                           header = FALSE))))}))
rownames(data) <-
  unlist(read.csv("data.csv", row.names = NULL)[1])

## print graphs function ----
print_graphs <- function (p, width, height, res, name, folder = "") {
  folder <- ifelse(folder == "",
                   "",
                   paste0(folder, "/"))
  
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  ggsave(paste0(folder, name, ".pdf"),
         plot = p,
         width = width,
         height = height,
         dpi = res,
         units = "px",
         bg = "white")
  png(filename = paste0(folder, name, ".png"),
      width = width,
      height = height,
      res = res,
      units = "px",
      bg = "white")
  options(warn = -1)
  print(p)
  invisible(dev.off())
  options(warn = 0)
}

two_step_sankey <- function(data,
                            sort = FALSE,
                            palette = c("#354542", "#6E7E64", "grey66",  "#966F6E", "#E6514E"),
                            x = 100,
                            y = 100,
                            x_gap = 50,
                            y_gap = 4,
                            trans = 0.4,
                            text_size = 2,
                            comparison_bar = TRUE,
                            comparison_bar_item_divider = FALSE,
                            cat_box_divider = FALSE,
                            divider_width = 0.25,
                            draw_connectors = TRUE,
                            connectors_colour = "white") {
  
  # functions
  colour_diluter <-
    function (colour, alpha) {
      colfunc <- 
        colorRampPalette(c("white", colour))
      return(colfunc(100)[round(alpha * 100)])
    }
  sigmoid_curve <- 
    function(x1, y1, x2, y2, n) {
      seq_x <- seq(x1, x2, length.out = n)
      seq_y <- y1 + (y2 - y1) * plogis(seq(from = -5, to = 5, length.out = n))
      points <- data.frame(x = seq_x, y = seq_y)
      return(points)
    }
  
  if (sort == TRUE) {
    data <-
      data[order(rownames(data)),]
  }
  
  # define order
  order <- rownames(data)
  names(order) <- 1:length(rownames(data))
  
  
  # define categories
  colfunc <- colorRampPalette(palette)
  colours <-
    colfunc(length(unname(unlist(read.csv("categories.csv",
                                          row.names = NULL,
                                          header = FALSE)[1]))))
  names(colours) <-
    unname(unlist(read.csv("categories.csv",
                           row.names = NULL,
                           header = FALSE)[1]))
  
  ## let's Sankey! ----
  sankey <- data
  sankey <- sankey[order(sankey[,1], sankey[,2]),]
  
  # add columns
  for (i in colnames(sankey)){
    sankey <- cbind(sankey[grep(i,
                                colnames(sankey),
                                invert = TRUE)],
                    sankey[grep(i,
                                colnames(sankey))],
                    x1_xxx = NA,
                    x2_xxx = NA,
                    y_xxx = NA)
    colnames(sankey)[grep("_xxx",
                          colnames(sankey))] <-
      gsub("_xxx", paste0("_", i), colnames(sankey)[grep("_xxx",
                                                         colnames(sankey))])
  }
  sankey[,grep("x1_", colnames(sankey))] <- x
  vars <- colnames(sankey)[grep("_", colnames(sankey), invert = TRUE)]
  height_unit <- 
    (y - y_gap * (length(unname(unlist(read.csv("categories.csv",
                                                row.names = NULL,
                                                header = FALSE)))) - 1)) /
    length(rownames(sankey)) * 0.99
  
  # get y
  for (i in vars){
    sankey <- sankey[order(sankey[,i]),]
    r <- 1
    for (r in seq_along(rownames(sankey))){
      if (r == 1) {
        sankey[r, paste0("y_", i)] <- 
          0 - height_unit/2
      } else {
        sankey[r, paste0("y_", i)] <-
          ifelse(sankey[r, i] == sankey[r-1, i],
                 sankey[r - 1, paste0("y_", i)] - height_unit,
                 sankey[r - 1, paste0("y_", i)] - height_unit - y_gap)
      }
    }
  }
  sankey <- sankey[order(sankey[,vars[1]]),]
  # get x
  x_width <- (x - x_gap)/length(vars)
  x_each_gap <- 
    ifelse(length(vars) > 2,
           x_gap/length(vars),
           x_gap)
  for (i in seq_along(vars)) {
    if (i != length(vars)) {
      sankey[,paste0("x1_",vars[i])] <- 
        (i-1)*x_width + (i-1)*x_each_gap 
      sankey[,paste0("x2_",vars[i])] <- 
        (i-1)*x_width + (i-1)*x_each_gap + x_width
    } else {
      sankey[,paste0("x1_",vars[i])] <- 
        (i-1)*x_width + (i-1)*x_each_gap 
      sankey[,paste0("x2_",vars[i])] <- x
    }
  }
  
  # draw the graph 
  if (comparison_bar == TRUE) {
    p <- ggplot() + 
      theme_void() +
      xlim(0, x + 1 + height_unit) +
      ylim(-y, 0)
  } else {
    p <- ggplot() + 
      theme_void() +
      xlim(0, x) +
      ylim(-y, 0)
  }
  
  # groupings 
  sankey <- sankey[order(sankey[,vars[1]]),]
  sankey <- cbind(group = NA,
                  sankey)
  for (r in seq_along(rownames(sankey))) {
    if (r == 1) {
      sankey[r, "group"] <- 1
    } else { 
      sankey[r, "group"] <-
        ifelse(sankey[r,vars[1]] == sankey[r-1,vars[1]] &
                 sankey[r,vars[2]] == sankey[r-1,vars[2]],
               max(sankey$group, na.rm = TRUE),
               max(sankey$group, na.rm = TRUE) + 1)
    }
  }
  line_density <- 500 # number of gradient lines
  line_size <- 0.25
  
  # draw connecting lines
  for (g in sample(1:max(sankey$group))) {
    # generate vertices
    polygon_data <- 
      data.frame(
        x = c(unique(sankey[sankey$group == g,
                            paste0("x2_", vars[1])]),
              rep(unique(sankey[sankey$group == g,
                                paste0("x1_", vars[2])]),
                  2),
              unique(sankey[sankey$group == g,
                            paste0("x2_", vars[1])])),
        y = c(max(sankey[sankey$group == g,
                         paste0("y_", vars[1])]) + height_unit/2,
              max(sankey[sankey$group == g,
                         paste0("y_", vars[2])]) + height_unit/2,
              min(sankey[sankey$group == g,
                         paste0("y_", vars[2])]) - height_unit/2,
              min(sankey[sankey$group == g,
                         paste0("y_", vars[1])]) - height_unit/2))
    
    colfunc <- 
      colorRampPalette(c(colour_diluter(colours[names(colours) == 
                                                  unique(sankey[sankey$group == g,
                                                                vars[1]])],
                                        trans),
                         colour_diluter(colours[names(colours) == 
                                                  unique(sankey[sankey$group == g,
                                                                vars[2]])],
                                        trans)))
    # sigmoid
    vertice_top <-
      sigmoid_curve(polygon_data[1,"x"], polygon_data[1,"y"],
                    polygon_data[2,"x"], polygon_data[2,"y"], 
                    line_density)
    vertice_bottom <-
      sigmoid_curve(polygon_data[4,"x"], polygon_data[4,"y"],
                    polygon_data[3,"x"], polygon_data[3,"y"], 
                    line_density)
    
    lines <- 
      data.frame(x1 = vertice_top$x,
                 y1 = vertice_top$y,
                 x2 = vertice_bottom$x,
                 y2 = vertice_bottom$y,
                 colour = colfunc(line_density))
    
    
    for (n in 1:nrow(lines)) {
      p[["layers"]][[length(p[["layers"]])+1]] <- 
        annotate("segment",
                 x = lines[n, "x1"],
                 xend = lines[n, "x2"],
                 y = lines[n, "y1"],
                 yend = lines[n, "y2"],
                 color = lines[n, "colour"],
                 size = line_size)
    }; rm(n)
    g <- g + 1
  }
  for (i in vars){
    r <- 1
    for (r in seq_along(rownames(sankey))) {
      # cat box
      if (cat_box_divider == TRUE) {
      p[["layers"]][[length(p[["layers"]])+1]] <- 
        geom_rect(aes(xmin = !!sankey[r, paste0("x1_", i)], 
                      xmax = !!sankey[r, paste0("x2_", i)], 
                      ymin = !!(sankey[r, paste0("y_", i)] - 
                                  height_unit/2), 
                      ymax = !!(sankey[r, paste0("y_", i)] + 
                                  height_unit/2)), 
                  fill = colour_diluter(colours[names(colours) == sankey[r, i]],
                                        trans),
                  color = 'white', size = divider_width) 
      } else if (cat_box_divider == FALSE) {
        p[["layers"]][[length(p[["layers"]])+1]] <- 
          geom_rect(aes(xmin = !!sankey[r, paste0("x1_", i)], 
                        xmax = !!sankey[r, paste0("x2_", i)], 
                        ymin = !!(sankey[r, paste0("y_", i)] - 
                                    height_unit/2), 
                        ymax = !!(sankey[r, paste0("y_", i)] + 
                                    height_unit/2)), 
                    fill = colour_diluter(colours[names(colours) == sankey[r, i]],
                                          trans)) 
      }
      
      # cat name
      p[["layers"]][[length(p[["layers"]])+1]] <- 
        geom_text(aes(x = !!mean(c(sankey[r, paste0("x1_", i)],
                                   sankey[r, paste0("x2_", i)])),
                      y = !!sankey[r, paste0("y_", i)]),
                  label = rownames(sankey)[r],
                  vjust = 0.5, hjust = 0.5,
                  color = colours[names(colours) == sankey[r, i]],
                  size = text_size)
      
      # line connector
      if (i ==  vars[1]) {
        col_line <- 
          colorRampPalette(c(colours[names(colours) == sankey[r, vars[1]]],
                             colours[names(colours) ==  sankey[r, vars[2]]]))
        line <- 
          sigmoid_curve(sankey[r,paste0("x2_", vars[1])]-1,
                        sankey[r,paste0("y_", vars[1])],
                        sankey[r,paste0("x1_", vars[2])]+1,
                        sankey[r,paste0("y_", vars[2])],
                        line_density)
        line$color <-
          col_line(line_density)
        if (r == 1) {
          connectors <- line
        } else {
          connectors <- rbind(connectors, line)
        }
      }
      
      if (i == vars[length(vars)]) {
        # comparison bar
        if (comparison_bar_item_divider == TRUE) {
        p[["layers"]][[length(p[["layers"]])+1]] <-
          geom_rect(aes(xmin = !!(x + 1),
                        xmax = !!(x + height_unit + 1),
                        ymin = !!(sankey[r, paste0("y_", i)] -
                                    height_unit/2),
                        ymax = !!(sankey[r, paste0("y_", i)] +
                                    height_unit/2)),
                    fill = colour_diluter(colours[names(colours) ==
                                                    sankey[r, vars[1]]],
                                          trans),
                    color = 'white', size = divider_width)
        } else if (comparison_bar_item_divider == FALSE) {
          p[["layers"]][[length(p[["layers"]])+1]] <-
            geom_rect(aes(xmin = !!(x + 1),
                          xmax = !!(x + height_unit + 1),
                          ymin = !!(sankey[r, paste0("y_", i)] -
                                      height_unit/2),
                          ymax = !!(sankey[r, paste0("y_", i)] +
                                      height_unit/2)),
                      fill = colour_diluter(colours[names(colours) ==
                                                      sankey[r, vars[1]]],
                                            trans))
        }
      }
      r <- r + 1
    }
    i <- vars[grep(i, vars) + 1]
  }
  # add connectors
  if (draw_connectors == TRUE) {
    p[["layers"]][[length(p[["layers"]])+1]] <-
      geom_point(data = connectors,
                 aes(x = x, y = y), color = connectors_colour, 
                 shape = 16, size = 0.02, alpha = 0.1,
                 show.legend = FALSE)
  }
  return(p)
}

## print ----
print_graphs(two_step_sankey(data,
                             sort = FALSE, # sort data 
                             palette = c("#354542", "#6E7E64", "grey66",  "#966F6E", "#E6514E"), # category colours
                             x = 100, # relative width
                             y = 100, # relative height
                             x_gap = 66, # relative gap width
                             y_gap = 4, # relative gap between categories
                             trans = 0.4, # box alpha
                             text_size = 1.5, # text size
                             comparison_bar = TRUE, # add comparison bar
                             comparison_bar_item_divider = TRUE, # item frame divider
                             cat_box_divider = FALSE, # item frame divider
                             divider_width = 0.25, # item divider width if TRUE
                             draw_connectors = TRUE, # draw connecting lines
                             connectors_colour = "white"), # connecting lines colour
             2500, # actual width
             2000, # actual height
             500, # resolution
             "two-step-sankey" # file name
)
