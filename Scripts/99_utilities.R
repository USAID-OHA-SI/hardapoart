#plot when no data exists

dummy_plot <- function(q){
  tibble::tibble(x = .5, y = .5,
                 lab = "No data available.") %>% 
    ggplot2::ggplot(aes(x, y, label = lab)) +
    ggplot2::geom_text(family = "Source Sans Pro", fontface = "italic", size = 5) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = {q}) +
    glitr::si_style_nolines() +
    ggplot2::theme(axis.text = element_blank())
}


# Helper function for assertr::verify() to show more descriptive error messages
  err_text <- function(msg) stop(msg, call = FALSE)
  