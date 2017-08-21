library(RColorBrewer)
library(knitr)
library(kableExtra)
# Define some colors
cols <- colorRampPalette(brewer.pal(n = 9,
                                    name = 'BrBG'))(2)
tablify <- function(x, n = 5){
  DT::datatable(x,
                selection = 'none',
                escape = FALSE,
                options = list(#sDom  = '<"top">lrt<"bottom">ip',
                  pageLength = n, 
                  dom = 'tip'))
}
kablify <- function(x, size = 10, caption =''){
  kable(x, format = "html", caption = caption) %>% 
    kable_styling(#bootstrap_options = c("striped",
                                        # "hover",
                                        # "condensed"),
                  font_size = size,
                  bootstrap_options = "striped",
                  # position = 'float_left',
                  # latex_options = c("striped", "hold_position"),
                  full_width = F)
}
