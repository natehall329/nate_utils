#' create a heatmap of fixed effects for a series of lmer model objects
#'
#' TODO document.
#'
#' @importFrom limma zscoreT
#' @importFrom viridis scale_fill_viridis
#'
#' @export


lmer_heatmaps <- function(lmer_list, box_width = .5, coef_str_replace = NULL, substitue_str = "", viridis_theme = TRUE, background_base = 0, ...) {

  mod_coeffs <- lapply(lmer_list, function(x) {
    sum <- summary(x)

    if(class(x) == "lmer"){
      sum <- data.frame(sum$coefficients) %>% rownames_to_column() %>% rename(`coeff` = `rowname`,
                                                                              `beta` = `Estimate`,
                                                                              `se` = `Std..Error`
                                                                              # `t` = `t.value`,
                                                                              # `p` = `Pr...t..`
      ) %>% dplyr::filter(coeff != "(Intercept)") %>%
        setNames(gsub("\\.value", "",names(.))) #%>% setNames(str_replace(names(.),"Pr\\.", "p"))
      colnames(sum)[grepl('Pr',colnames(sum))] <- 'p'
      sum
      # clean this up later. messy, but allows for t and z stats to get passed.
    } else if (class(x) == "lme"){
      sum <- data.frame(sum$tTable) %>% rownames_to_column() %>% rename(`coeff` = `rowname`,
                                                                        `beta` = `Value`,
                                                                        `se` = `Std.Error`
                                                                        # `t` = `t.value`,
                                                                        # `p` = `Pr...t..`
      ) %>% dplyr::filter(coeff != "(Intercept)") %>%
        setNames(gsub("\\.value", "",names(.))) #%>% setNames(str_replace(names(.),"Pr\\.", "p"))
      sum

    } else {
      print("Model class must be lmer or lme!")
      stop()
    }

  })


  nest_df <- Map(cbind, model_id = names(mod_coeffs), mod_coeffs) %>% do.call(rbind, .) #%>% dplyr::select(model_id, coeff, t,df, p)
  names(nest_df) <- tolower(names(nest_df))



  if("t" %in% names(nest_df)){
    nest_df$z <- mapply(limma::zscoreT, nest_df$t, nest_df$df) # remap t-scores to z scale if necessary
  }


  # if similar coefficients need to be replaced (e.g. different models measuring interactions with seperate self-report scales)
  for(i in coef_str_replace){ # ugly but works, figure out how to get rid of this later.
    split_coeffs <- str_split(nest_df$coeff, ":")
    nest_df$coeff <- do.call(rbind,lapply(split_coeffs, function(x) paste(gsub(paste0("\\<",i,"\\>"), "sr", x), collapse = ":")))
  }
  nest_df$coeff <- factor(nest_df$coeff, levels = rev(unique(nest_df$coeff))) # preserve ordering provided (top to bottom, as in summary output).


  # base matrix
  gg <- ggplot(data = nest_df) + geom_tile(aes(x=model_id, y=coeff, fill=z)) + scale_fill_viridis(name="z-statistic")

  # flag significant effects with tiles by significance level.
  # TODO abstract into a function.

  ### make frames for p < .05, p < .01, p < .005. These will get translated to the boxes on the matrix.
  nest_df$na.1 <- ifelse(nest_df$p < .1 & nest_df$p > .05, TRUE, FALSE)
  nest_df$na.05 <- ifelse(nest_df$p < .05 & nest_df$p > .01, TRUE, FALSE)
  nest_df$na.01 <- ifelse(nest_df$p <= .01 & nest_df$p >.005, TRUE, FALSE)
  nest_df$na.005 <- ifelse(nest_df$p <= .005, TRUE, FALSE)

  frames.1 <- nest_df %>% dplyr::filter(na.1)
  frames.05 <- nest_df %>% dplyr::filter(na.05)
  frames.01 <- nest_df %>% dplyr::filter(na.01)
  frames.005 <- nest_df %>% dplyr::filter(na.005)

  tmp <- ggplot_build(gg)$data[[1]][2,]  # get the first data point from geom_raster
  width <- tmp$xmax - tmp$xmin  # calculate the width of the rectangle
  height <- tmp$ymax - tmp$ymin  # calculate the height of the rectangle

  gg <- gg + geom_tile(data = frames.1, aes(x = model_id, y = coeff), width = width, height = height, color = "grey", fill = NA, size=box_width/2, linetype ="dotted", alpha = .8) +
    geom_tile(data = frames.05, aes(x = model_id, y = coeff), width = width, height = height, color = "black", fill = NA, size=box_width) +
    geom_tile(data = frames.01, aes(x = model_id, y = coeff), width = width, height = height, color = "orange", fill = NA, size=box_width) +
    geom_tile(data = frames.005, aes(x = model_id, y = coeff), width = width, height = height, color = "red", fill = NA, size=box_width)

  ### to help hide values close to 0, we can set the background to be at the 0 point on the viridis gradient

  if(viridis_theme){ # TODO abstract this into a self-contained theme for any plot using the viridis gradient in the future.

    ## get exact 0 point rendered on viridis scale.
    l  <- cowplot::get_legend(gg)

    viridis_value_mapping <- data.frame(raster_value = rev(l[[1]][[1]]$grobs[[2]]$raster),
                                        numeric_value = seq(range(nest_df$z)[1], range(nest_df$z)[2], length.out = 300)) %>% mutate(diffVal = min(abs(numeric_value - background_base)) == abs(numeric_value))

    viridis_background <- viridis_value_mapping %>% dplyr::filter(diffVal) %>% pull(raster_value) %>% as.character()
    viridis_grid <-  viridis_value_mapping[which(viridis_value_mapping$diffVal) - 15,"raster_value"] %>% as.character()


    theme_viridis_blend <- function (base_size = 11,
                                     base_family = ""
    ) {
      theme_dark() %+replace%
        theme(
          rect = element_rect(fill = viridis_grid),
          panel.background = element_rect(fill = viridis_background),
          panel.grid.major  = element_line(color = viridis_grid),

          # strip.background =
          #panel.border = element_rect(color = "lightgrey", fill = NA),
          #axis.line = element_line(color = "lightgrey"),
          #axis.ticks = element_line(color = "lightgrey"),
          axis.text.x = element_text(color = "white", angle = 45, vjust= .95, hjust = .95,face = "plain"),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white", angle = 90, face = "plain"),
          axis.text.y = element_text(color = "white", hjust = .95),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white")

        )
    }

    gg <- gg + theme_viridis_blend(gg)
  }

  gg <- gg + coord_equal()

  return(gg)
}


