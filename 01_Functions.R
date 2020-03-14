# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: 00_FUNCTIONS
#' PROJECTS: PEARSON
#' DESCRIPTIONS: PERSONALIZED FUNCTIONS
#' 
#' 
#' PROGRAMMER: LEONARDO PALOMERA
#' DATE: 3/4/2020
#' R VERSION: 3.5.0
#' INPUT FILES
#' OUTPUT FILES
#' SECTION

source("./00_Source.R")

#' # STAT LINE VISULIZATION --------------------------------------------------
StatMeanLine <- ggproto("StatMeanLine", Stat,
                        compute_group = function(data, scales) {
                          transform(data, yintercept=mean(y))
                        },
                        required_aes = c("x", "y")
)

stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatMeanLine, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



# VISULIZATION TOOLS ------------------------------------------------------
pretty_colors <- 
  #' GENERAL COLOR SCHEMES
  c("#778899", "#C90E17", "#001933", "#08519c", "#6495ED","#B0C4DE", 
    "#999999", "#000000", "#800000", "#B23232", "#691b14")





