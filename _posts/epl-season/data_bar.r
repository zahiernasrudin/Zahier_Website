data_bars <- function(data,
                      fill_color = "#1e90ff",
                      fill_color_ref = NULL,
                      fill_opacity = 1,
                      fill_gradient = FALSE,
                      background = "transparent",
                      max_value = NULL,
                      min_value = NULL,
                      align_bars = "left",
                      bar_height = 19,
                      text_position = "outside-end",
                      text_color = "black",
                      brighten_text = TRUE,
                      brighten_text_color = "white",
                      bold_text = FALSE,
                      number_fmt = NULL,
                      icon = NULL,
                      icon_ref = NULL,
                      icon_size = 20,
                      icon_color = NULL,
                      icon_color_ref = NULL,
                      img = NULL,
                      img_ref = NULL,
                      img_height = 20,
                      img_width = 20) {
  
  cell <- function(value, index, name) {
    
    if (!is.numeric(value)) return(value)
    
    ### stop messages
    if (!is.logical(fill_gradient)) {
      
      stop("`fill_gradient` must be TRUE or FALSE")
    }
    
    if (!is.logical(bold_text)) {
      
      stop("`bold_text` must be TRUE or FALSE")
    }
    
    if (!is.logical(brighten_text)) {
      
      stop("`brighten_text` must be TRUE or FALSE")
    }
    
    if (!is.null(max_value) & !is.numeric(max_value)) {
      
      stop("`max_value` must be numeric")
    }
    
    if (!is.numeric(fill_opacity)) {
      
      stop("`fill_opacity` must be numeric")
    }
    
    if (fill_opacity < 0 | fill_opacity > 1) {
      
      stop("`fill_opacity` must be a value between 0 and 1")
    }
    
    if (!align_bars %in% c("left","right")) {
      
      stop("`align_bars` must be either left or right")
    }
    
    if (!text_position %in% c("inside-base","inside-end","outside-base","outside-end","center","none")) {
      
      stop("`text_position` must be either center, inside-base, inside-end, outside-base, outside-end, or none")
    }
    
    if (length(text_color) > 1) {
      
      stop("multiple colors detected in `text_color`. only one color can be used.")
    }
    
    if (length(brighten_text_color) > 1) {
      
      stop("multiple colors detected in `brighten_text_color` only one color can be used.")
    }
    
    if (length(background) > 1) {
      
      stop("multiple colors detected in `background`. only one color can be used.")
    }
    
    ### optional formatter from scales package
    if (is.null(number_fmt)) {
      
      text_label <- value
      
    } else text_label <- number_fmt(value)
    
    ### color palette function
    color_pal <- function(x) {
      
      if (!is.na(x))
        rgb(grDevices::colorRamp(c(fill_color))(x), maxColorValue = 255)
      else
        NULL
    }
    
    assign_color <- function(x) {
      
      if (!is.na(x)) {
        rgb_sum <- rowSums(grDevices::colorRamp(c(fill_color))(x))
        color <- ifelse(rgb_sum >= 375, text_color, brighten_text_color)
        color
      } else
        NULL
    }
    
    ### normalization for color palette
    normalized <-
      (value - min(data[[name]], na.rm = TRUE)) / (max(data[[name]], na.rm = TRUE) - min(data[[name]], na.rm = TRUE))
    
    ### conditional fill color and font color
    if (is.character(fill_color_ref)) {
      
      if (all(fill_color_ref %in% names(which(sapply(data, is.character))))) {
        
        if (is.character(fill_color_ref)) { fill_color_ref <- which(names(data) %in% fill_color_ref) }
        
        fill_color_pal <- data[[fill_color_ref]][index]
        fill_color_pal <- grDevices::adjustcolor(fill_color_pal, alpha.f = fill_opacity)
        
        rgb_sum <- rowSums(grDevices::colorRamp(c(fill_color_pal))(1))
        
        if (brighten_text == TRUE & text_position == "inside-end" | brighten_text == TRUE & text_position == "inside-base" | brighten_text == TRUE & text_position == "center") {
          
          font_color <- ifelse(rgb_sum >= 375, text_color, brighten_text_color)
          
        } else font_color <- text_color
        
      } else {
        
        stop("Attempted to select non-existing column or non-character column with fill_color_ref")
      }
      
    } else {
      
      fill_color_pal <- color_pal(normalized)
      fill_color_pal <- grDevices::adjustcolor(fill_color_pal, alpha.f = fill_opacity)
      
      if (brighten_text == TRUE & text_position == "inside-end" | brighten_text == TRUE & text_position == "inside-base" | brighten_text == TRUE & text_position == "center") {
        
        font_color <- assign_color(normalized)
        
      } else font_color <- text_color
    }
    
    ### fill_gradient fill_color
    if (fill_gradient == TRUE & length(fill_color) > 1) {
      
      fill_color <- grDevices::adjustcolor(fill_color, alpha.f = fill_opacity)
      
      gradient <- paste0("linear-gradient(to right,", paste(fill_color, collapse = ", "))
      
      fill_color_pal <- NULL
      
    } else if (fill_gradient == TRUE & length(fill_color) == 1) {
      
      stop("must provide at least two colors in `fill_color` when using `fill_gradient = TRUE`")
      
    } else gradient <- NULL
    
    ### icon_ref
    if (is.character(icon_ref)) {
      
      if (all(icon_ref %in% names(which(sapply(data, is.character))))) {
        
        if (is.character(icon_ref)) { icon_ref <- which(names(data) %in% icon_ref) }
        
        icon <- data[[icon_ref]][index]
        
        ### icon_color
        if (!is.null(icon_color)) {
          colors <- icon_color
        } else colors <- fill_color_pal
        
        ### icon_color_ref
        if (is.character(icon_color_ref)) {
          if (all(icon_color_ref %in% names(which(sapply(data, is.character))))) {
            
            if (is.character(icon_color_ref)) { icon_color_ref <- which(names(data) %in% icon_color_ref) }
            
            colors <- data[[icon_color_ref]][index]
            
          } else { stop("Attempted to select non-existing column or non-character column with icon_color_ref") }
        }
        
        icon_label <- htmltools::tagAppendAttributes(shiny::icon(icon),
                                                     style = paste0("font-size:", icon_size, "px", "; color:", colors))
        
      } else {
        
        stop("Attempted to select non-existing column or non-character column with icon_ref")
        
      }
      
    } else if (!is.null(icon)) {
      
      ### icon_color
      if (!is.null(icon_color)) {
        colors <- icon_color
      } else colors <- fill_color_pal
      
      ### icon_color_ref
      if (is.character(icon_color_ref)) {
        if (all(icon_color_ref %in% names(which(sapply(data, is.character))))) {
          if (is.character(icon_color_ref)) { icon_color_ref <- which(names(data) %in% icon_color_ref) }
          
          colors <- data[[icon_color_ref]][index]
          
        } else { stop("Attempted to select non-existing column or non-character column with icon_color_ref") }
      }
      
      icon_label <- htmltools::tagAppendAttributes(shiny::icon(icon),
                                                   style = paste0("font-size:", icon_size, "px", "; color:", colors))
      
    } else icon_label <- NULL
    
    ### img_ref
    if (is.character(img_ref)) {
      if (all(img_ref %in% names(which(sapply(data, is.character))))) {
        if (is.character(img_ref)) { img_ref <- which(names(data) %in% img_ref) }
        
        imgs <- data[[img_ref]][index]
        
        if (!is.character(imgs)) return(imgs)
        
        if (grepl("https|http|", imgs) == FALSE) {
          
          stop("must provide valid link to image")
        }
        
        img_label <- htmltools::img(src = imgs, height = img_height, width = img_width)
        
      } else { stop("Attempted to select non-existing column or non-character column with img_ref") }
      
    } else if (!is.null(img)) {
      
      if (!is.character(img)) return(img)
      
      if (grepl("https|http", img) == FALSE) {
        
        stop("must provide valid link to image")
      }
      
      img_label <- htmltools::img(src = img, height = img_height, width = img_width)
      
    } else img_label <- NULL
    
    ### height of data_bars
    height <- bar_height
    
    ### width of data_bars
    width <- if (is.numeric(value) & is.null(max_value) & is.null(min_value)) {
      
      paste0(abs(value) / max(abs(data[[name]]), na.rm = TRUE) * 100, "%")
      
      ### min_value provided
    } else if (is.numeric(value) & is.null(max_value) & !is.null(min_value)) {
      
      paste0((abs(value) - min_value) / (max(abs(data[[name]]), na.rm = TRUE) - min_value) * 100, "%")
      
      ### max_value provided
    } else if (is.numeric(value) & !is.null(max_value) & is.null(min_value)) {
      
      paste0((abs(value) / max_value) * 100, "%")
      
      ### min and max provided
    } else if (is.numeric(value) & !is.null(max_value) & !is.null(min_value)) {
      
      paste0((abs(value) - min_value) / (max_value - min_value) * 100, "%")
      
    } else if (!is.numeric(value)) {
      
      return(value)
    }
    
    if (bold_text == TRUE) {
      
      bold_text <- "bold"
      
    } else bold_text <- "normal"
    
    #### pos_neg charts
    neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
    pos_chart <- htmltools::div(style = list(flex = "1 1 0"))
    
    ### neg side
    if ((min(data[[name]], na.rm = TRUE) < 0)) {
      
      ### add opacity to fill
      fill_color <- grDevices::adjustcolor(fill_color, alpha.f = fill_opacity)
      
      if (value < 0 & text_position == "outside-end") {
        
        ### assign negative color if two fill_color are provided
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, width = width, height = height, transition = "width 1s", textAlign = "left", display = "flex", alignItems = "center"), icon_label, img_label)
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end", color = font_color, fontWeight = bold_text, background = background), text_label, bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        
      } else if (value < 0 & text_position == "inside-end") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                              icon_label, img_label,
                              htmltools::div(style = list(textAlign = "left", marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, color = font_color, fontWeight = bold_text, width = width, height = height, transition = "width 1s", textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label))
        chart <-  htmltools::div(style = list(flexGrow = 1, background = background), bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        
      } else if (value < 0 & text_position == "outside-base") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                              "", icon_label, img_label,
                              htmltools::div(style = list(textAlign = "left", marginRight = "7px", marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, width = width, height = height, transition = "width 1s", display = "flex", alignItems = "center")))
        chart <-  htmltools::div(style = list(flexGrow = 1, background = background), bar)
        bar_chart <- htmltools::div(style = list(display = "flex", alignItems = "center", color = font_color, fontWeight = bold_text), chart, text_label)
        neg_chart <- htmltools::tagAppendChild(neg_chart, bar_chart)
        
      } else if (value < 0 & text_position == "inside-base") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                              icon_label, img_label,
                              htmltools::div(style = list(marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, color = font_color, fontWeight = bold_text, width = width, height = height, transition = "width 1s", textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label))
        chart <-  htmltools::div(style = list(background = background), bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        
      } else if (value < 0 & text_position == "center") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                              icon_label, img_label,
                              htmltools::div(style = list(textAlign = "center", background = fill_color_pal, backgroundImage = gradient, color = font_color, fontWeight = bold_text, width = width, height = height, transition = "width 1s", textOverflow = "ellipsis", whiteSpace = "nowrap", marginLeft = "7px"), text_label))
        chart <-  htmltools::div(style = list(flexGrow = 1, background = background), bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        
      } else if (value < 0 & text_position == "none") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[1]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
                              htmltools::div(style = list(textAlign = "left", marginLeft = "7px", background = fill_color_pal, backgroundImage = gradient, color = font_color, fontWeight = bold_text, width = width, height = height, transition = "width 1s", display = "flex", alignItems = "center"), icon_label, img_label))
        chart <-  htmltools::div(style = list(flexGrow = 1, background = background), bar)
        neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
        
        ### pos side
      } else if (value >= 0 & text_position == "outside-end") {
        
        ### assign positive color if two fill_color are provided
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(marginRight = "7px", background = fill_color_pal, backgroundImage = gradient, width = width, height = height, transition = "width 1s", display = "flex", alignItems = "center", justifyContent = "flex-end"), icon_label, img_label)
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", background = background, color = font_color, fontWeight = bold_text), bar, text_label)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
        
      } else if (value >= 0 & text_position == "inside-end") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                              htmltools::div(style = list(marginRight = "7px", textAlign = "right", background = fill_color_pal, backgroundImage = gradient, color = font_color, fontWeight = bold_text, width = width, height = height, transition = "width 1s", textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label), icon_label, img_label)
        chart <- htmltools::div(style = list(background = background), bar)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
        
      } else if (value >= 0 & text_position == "outside-base") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                              htmltools::div(style = list(background = fill_color_pal, backgroundImage = gradient, width = width, height = height, transition = "width 1s", display = "flex", alignItems = "center", marginLeft = "7px", marginRight = "7px"), ""),
                              icon_label, img_label)
        chart <- htmltools::div(style = list(flexGrow = 1, background = background), bar)
        bar_chart <- htmltools::div(style = list(display = "flex", alignItems = "center", color = font_color, fontWeight = bold_text, display = "flex", alignItems = "center"), text_label, chart)
        pos_chart <- htmltools::tagAppendChild(pos_chart, bar_chart)
        
      } else if (value >= 0 & text_position == "inside-base") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                              htmltools::div(style = list(marginRight = "7px", textAlign = "left", background = fill_color_pal, backgroundImage = gradient, color = font_color, fontWeight = bold_text, width = width, height = height, transition = "width 1s", textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label),
                              icon_label, img_label)
        chart <- htmltools::div(style = list(background = background), bar)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
        
      } else if (value >= 0 & text_position == "center") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal
        
        if (is.null(brighten_text_color)) {
          font_color <- "black"
        } else font_color <- font_color
        
        bar <- htmltools::div(style = list(display = "flex", alignItems = "center"),
                              htmltools::div(style = list(textAlign = "center", marginRight = "7px", background = fill_color_pal, backgroundImage = gradient, color = font_color, fontWeight = bold_text, width = width, height = height, transition = "width 1s", textOverflow = "ellipsis", whiteSpace = "nowrap"), text_label), icon_label, img_label)
        chart <- htmltools::div(style = list(background = background), bar)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
        
      } else if (value >= 0 & text_position == "none") {
        
        if (length(fill_color) == 2) {
          fill_color_pal <- fill_color[[2]]
        } else fill_color_pal <- fill_color_pal
        
        bar <- htmltools::div(style = list(textAlign = "right", background = fill_color_pal, backgroundImage = gradient, width = width, height = height, transition = "width 1s", display = "flex", alignItems = "center", justifyContent = "flex-end"), icon_label, img_label)
        chart <- htmltools::div(style = list(display = "flex", alignItems = "center", background = background),
                                bar)
        pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
      }
      
      htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)
      
    } else {
      
      ### data_bars for positive values only
      bar_chart <-
        function(text_label,
                 icon_label,
                 img_label,
                 width = "100%",
                 height = "100%",
                 fill = NULL,
                 background = NULL,
                 color = NULL,
                 fontWeight = NULL) {
          
          if (align_bars == "right" & text_position == "outside-end") {
            
            chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end",
                color = font_color,
                fontWeight = bold_text),
                text_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  marginLeft = "7px",
                  display = "flex",
                  alignItems = "center"
                ), icon_label, img_label))
            
            back <-
              htmltools::div(style = list(
                flexGrow = 1,
                background = background
              ),
              chart)
            
            htmltools::div(back)
            
          } else if (align_bars == "right" & text_position == "inside-end") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                icon_label,
                img_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  fontWeight = bold_text,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  textOverflow = "ellipsis",
                  whiteSpace = "nowrap",
                  marginLeft = "7px"
                ), text_label))
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
            
          } else if (align_bars == "right" & text_position == "inside-base") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                icon_label,
                img_label,
                htmltools::div(style = list(
                  marginLeft = "7px",
                  textAlign = "right",
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  fontWeight = bold_text,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  textOverflow = "ellipsis",
                  whiteSpace = "nowrap"
                ), text_label))
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                marginLeft = "7px",
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
            
          } else if (align_bars == "right" & text_position == "outside-base") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                "",
                icon_label,
                img_label,
                htmltools::div(style = list(
                  textAlign = "left",
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  fontWeight = bold_text,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  marginLeft = "7px",
                  display = "flex",
                  alignItems = "center"
                )))
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                marginRight = "7px",
                background = background
              ),
              fill_chart)
            
            htmltools::div(style = list(display = "flex",
                                        alignItems = "center",
                                        color = font_color),
                           back_chart,
                           text_label)
            
          } else if (align_bars == "right" & text_position == "center") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                icon_label,
                img_label,
                htmltools::div(style = list(
                  textAlign = "center",
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  fontWeight = bold_text,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  textOverflow = "ellipsis",
                  whiteSpace = "nowrap",
                  marginLeft = "7px"
                ), text_label))
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                marginLeft = "7px",
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
            
          } else if (align_bars == "right" & text_position == "none") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                justifyContent = "flex-end"),
                htmltools::div(style = list(
                  marginLeft = "7px",
                  textAlign = "left",
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  display = "flex",
                  alignItems = "center"
                ), icon_label, img_label))
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                marginLeft = "7px",
                background = background
              ),
              fill_chart)
            
            htmltools::div(style = list(display = "flex", alignItems = "center"),
                           back_chart)
            
          } else if (align_bars == "left" & text_position == "outside-end") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center",
                color = font_color,
                fontWeight = bold_text),
                htmltools::div(style = list(
                  alignText = "right",
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  marginRight = "7px",
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end"
                ),
                icon_label,
                img_label),
                text_label)
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
            
          } else if (align_bars == "left" & text_position == "inside-end") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center"),
                htmltools::div(style = list(
                  textAlign = "right",
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  fontWeight = bold_text,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  textOverflow = "ellipsis",
                  whiteSpace = "nowrap",
                  marginRight = "7px"
                ), text_label),
                icon_label,
                img_label)
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
            
          } else if (align_bars == "left" & text_position == "inside-base") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center"),
                htmltools::div(style = list(
                  textAlign = "left",
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  fontWeight = bold_text,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  textOverflow = "ellipsis",
                  whiteSpace = "nowrap",
                  marginRight = "7px"
                ), text_label),
                icon_label,
                img_label)
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
            
          } else if (align_bars == "left" & text_position == "outside-base") {
            
            bar <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center"),
                htmltools::div(style = list(
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  display = "flex",
                  alignItems = "center",
                  marginRight = "7px"
                ), ""),
                icon_label,
                img_label)
            
            chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                marginLeft = "7px",
                background = background
              ),
              bar)
            
            htmltools::div(style = list(display = "flex",
                                        alignItems = "center",
                                        color = font_color,
                                        fontWeight = bold_text),
                           text_label,
                           chart)
            
          } else if (align_bars == "left" & text_position == "center") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center"),
                htmltools::div(style = list(
                  textAlign = "center",
                  background = fill,
                  backgroundImage = gradient,
                  color = font_color,
                  fontWeight = bold_text,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  textOverflow = "ellipsis",
                  whiteSpace = "nowrap",
                  marginRight = "7px"
                ), text_label),
                icon_label,
                img_label)
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
            
          } else if (align_bars == "left" & text_position == "none" | is.null(align_bars) & text_position == "none") {
            
            fill_chart <-
              htmltools::div(style = list(
                display = "flex",
                alignItems = "center"),
                htmltools::div(style = list(
                  textAlign = "right",
                  background = fill,
                  backgroundImage = gradient,
                  width = width,
                  height = height,
                  transition = "width 1s",
                  marginLeft = "7px",
                  display = "flex",
                  alignItems = "center",
                  justifyContent = "flex-end"
                ),
                icon_label,
                img_label))
            
            back_chart <-
              htmltools::div(style = list(
                flexGrow = 1,
                background = background
              ),
              fill_chart)
            
            htmltools::div(back_chart)
          }
        }
      
      ### adjust spacing so numbers align properly in outside-base
      if (text_position == "outside-base") {
        
        max_digits <- max(nchar(data[[name]]), na.rm = TRUE)+1
        
        text_label <- stringr::str_pad(text_label, max_digits)
        
      } else text_label <- text_label
      
      bar_chart(text_label,
                icon_label,
                img_label,
                height = height,
                width = width,
                fill = fill_color_pal,
                background = background,
                color = font_color,
                fontWeight = bold_text)
    }
  }
}