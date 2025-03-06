generation_image <- function() {
  x <- -500:500 / 100
  #writePNG(quantized_image, "data/kmean_data/test_image_data1_quantized.png")
  image_path <- "unit3/output/parabola.png"  # Update this path
  png(image_path)
  plot(x, x * x, type = "l")
  dev.off()
}
generation_pdf <- function() {
  x <- -500:500 / 100
  #writePNG(quantized_image, "data/kmean_data/test_image_data1_quantized.png")
  image_path <- "unit3/output/parabola.pdf"  # Update this path
  pdf(image_path)
  plot(x, x * x, type = "l")
  dev.off()
}
generation_postscript <- function() {
  x <- -500:500 / 100;
  #writePNG(quantized_image, "data/kmean_data/test_image_data1_quantized.png")
  image_path <- "unit3/output/parabola.ps"  # Update this path
  # Open a PostScript graphics device
  postscript(image_path, width=7, height=7, horizontal=FALSE, paper="special")
  plot(x, x * x, type = "l")
  dev.off()
}
generation_image()
generation_pdf()
generation_postscript()