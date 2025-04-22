theme_fontsize <- function (base_size = 6) 
{
    theme(text = element_text(size = base_size, color = "black"), 
        axis.title = element_text(size = base_size, color = "black"), 
        axis.text = element_text(size = base_size, color = "black"), 
        plot.title = element_text(size = base_size, color = "black"), 
        # strip.title= element_text(size = base_size, color= "black"),
        strip.text.x = element_text(size = base_size, color = "black"),  
        strip.text.y = element_text(size = base_size, color = "black"), 
        legend.text = element_text(size = base_size, color = "black"), 
        legend.title = element_text(size = base_size, color = "black"))
}
