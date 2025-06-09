generate_cover <- function(input, filtered_data) {
  switch(input,
         "Width-Balanced" = create_width_balanced_cover(
           min(filtered_data),
           max(filtered_data),
           input$num_patches,
           input$percent_overlap
         ))
}
