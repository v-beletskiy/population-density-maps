options(rgl.useNULL = FALSE)

require(rgl)
require(webshot2)
require(sf)
require(tmap)
require(ggplot2)
require(mapview)
require(stars)
require(rayshader)
require(colorspace)
require(rayrender)

######## Configs for different chart options (uncomment the one you want to render) #########
#############################################################################################

# 1st - default - moderately dark orange, view from the south-west
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 52
# shadowcolor <- "#efebe0"
# camera_theta <- -68
# camera_phi <- 55
# camera_zoom <- 0.85
# # 1st light is main, from west.
# # 2nd light is used to highlight western part of city bars at low angle
# # 3d light is used to highlight western shadows
# # 4th light is located strictly at top to enlighten shadows
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 11, 50, 90)
# render_3d_lightdirection <- c(320, 320, 290, 120)
# render_3d_lightintensity <- c(270, 350, 25, 90)
# output_file_name <- "output/ukraine_population_density_1.png"

# 2nd - default but lighter
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 52
# shadowcolor <- "#efebe0"
# camera_theta <- -68
# camera_phi <- 55
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 11, 50, 90)
# render_3d_lightdirection <- c(320, 320, 290, 120)
# render_3d_lightintensity <- c(450, 310, 35, 105)
# output_file_name <- "output/ukraine_population_density_2.png"

# 3d - default but with less height city bars
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 85
# shadowcolor <- "#efebe0"
# camera_theta <- -68
# camera_phi <- 55
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 11, 50, 90)
# render_3d_lightdirection <- c(320, 320, 290, 120)
# render_3d_lightintensity <- c(270, 350, 25, 90)
# output_file_name <- "output/ukraine_population_density_3.png"

# 4th - default but lighter & with less height city bars
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 85
# shadowcolor <- "#efebe0"
# camera_theta <- -68
# camera_phi <- 55
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 11, 50, 90)
# render_3d_lightdirection <- c(320, 320, 290, 120)
# render_3d_lightintensity <- c(450, 310, 35, 105)
# output_file_name <- "output/ukraine_population_density_4.png"

# 5th - default but low south angle & lighter & with less height city bars
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 85
# shadowcolor <- "#efebe0"
# camera_theta <- -56
# camera_phi <- 33
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 11, 50, 90)
# render_3d_lightdirection <- c(270, 270, 290, 120)
# render_3d_lightintensity <- c(450, 320, 35, 105)
# output_file_name <- "output/ukraine_population_density_5.png"

# 6th - default but low south angle & lighter
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 52
# shadowcolor <- "#efebe0"
# camera_theta <- -56
# camera_phi <- 33
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 11, 50, 90)
# render_3d_lightdirection <- c(270, 270, 290, 120)
# render_3d_lightintensity <- c(450, 320, 35, 105)
# output_file_name <- "output/ukraine_population_density_6.png"

# 7th - default but low south angle & with less height city bars
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 85
# shadowcolor <- "#efebe0"
# camera_theta <- -56
# camera_phi <- 33
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 11, 50, 90)
# render_3d_lightdirection <- c(270, 270, 290, 120)
# render_3d_lightintensity <- c(270, 350, 25, 90)
# output_file_name <- "output/ukraine_population_density_7.png"

# 8th - default but angle from top & with less height city bars
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 85
# shadowcolor <- "#efebe0"
# camera_theta <- -56
# camera_phi <- 58
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 20, 50, 90)
# render_3d_lightdirection <- c(270, 270, 290, 120)
# render_3d_lightintensity <- c(270, 350, 25, 90)
# output_file_name <- "output/ukraine_population_density_8.png"

# 9th - default but angle from top & with less height city bars & lighter
color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
color_bias <- 3
zscale <- 85
shadowcolor <- "#efebe0"
camera_theta <- -56
camera_phi <- 58
camera_zoom <- 0.85
render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
render_3d_lightaltitude <- c(35, 20, 50, 90)
render_3d_lightdirection <- c(270, 270, 290, 120)
render_3d_lightintensity <- c(450, 320, 35, 105)
output_file_name <- "output/ukraine_population_density_9.png"

# 10th - default but low south-east angle & with less height city bars & lighter
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 52
# shadowcolor <- "#efebe0"
# camera_theta <- -38
# camera_phi <- 30
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 20, 50, 90)
# render_3d_lightdirection <- c(270, 270, 290, 120)
# render_3d_lightintensity <- c(450, 320, 35, 105)
# output_file_name <- "output/ukraine_population_density_10.png"

# 11th - default but low south-east angle & lighter
# color <- c("#f5e0af", "#f0cd98", "#f1b382", "#e88c75", "#e87d62")
# color_bias <- 3
# zscale <- 85
# shadowcolor <- "#efebe0"
# camera_theta <- -38
# camera_phi <- 30
# camera_zoom <- 0.85
# render_3d_lightcolor <- c("#ffde8f", "#ffffff", "#ffffff", "#fdf8ec")
# render_3d_lightaltitude <- c(35, 20, 50, 90)
# render_3d_lightdirection <- c(270, 270, 290, 120)
# render_3d_lightintensity <- c(450, 320, 35, 105)
# output_file_name <- "output/ukraine_population_density_11.png"

# TODO - render with background = '#070707' AND '#272625'

### Other color palette options:
# color <- c('#F4DE9C', '#E9C46A', '#F4A261', '#E76F51', '#E45F3E')

# color <- c('#F5E9BA', '#f7cf9b', '#F5C884', '#EB9E7D', '#e7895f')
# color <- c('#f8e5c2', '#f7cf9b', '#f5be84', '#e49675', '#e67c4c')
# shadowcolor = '#eee5cb',

# color <- c('#eddd8e', '#1cb101', '#01b1b1', '#0154B1')
# shadowcolor = #f2ebc7

# color <- c('#2A9D8F', '#E9C46A', '#F4A261', '#E76F51')

# color <- c('#FCF7ED', '#F6E8CB', '#F0D8A8', '#EAC886', '#E3B963', '#DDA940', '#9C711C', '#684B12')
# shadowcolor = '#FCF7ED',

# color <- c('#fef7ef', '#ffe5c6', '#ffcb8d', '#ffb153', '#fa8800', '#a75c02')
# shadowcolor = '#fef7ef'

#############################################################################################
#############################################################################################

# load population 400m H3 hexagon
# https://data.humdata.org/dataset/kontur-population-ukraine
country_hex_population_file <-
  st_read("data/kontur_population_UA_20220630.gpkg") %>%
  st_transform(3106)

# load population by administrative boundary
# https://data.humdata.org/dataset/kontur-boundaries-ukraine
country_boundaries_file <-
  st_read("data/kontur_boundaries_UA_20220407.gpkg") %>%
  st_transform(3106)

country_boundary <-
  country_boundaries_file %>%
  st_geometry() %>% # extracts geometries (shapes) from spatial data frame (boundary gpkg file)
  st_union() %>%
  st_sf() %>% # creates simple features object
  st_make_valid()

# boundary chart
# ggplot(country_hex_population_file) +
#   geom_sf(aes(fill = population),
#           color = "gray66",
#           linewidth = 0) +
#   geom_sf(
#     data = country_boundary,
#     fill = NA,
#     color = "black",
#     linetype = "dashed",
#     linewidth = 1
#   )

country_bounding_box <- st_bbox(country_boundary)

bottom_left <- st_point(c(country_bounding_box[["xmin"]], country_bounding_box[["ymin"]])) %>%
  st_sfc(crs = 3106)
bottom_right <- st_point(c(country_bounding_box[["xmax"]], country_bounding_box[["ymin"]])) %>%
  st_sfc(crs = 3106)
top_left <- st_point(c(country_bounding_box[["xmin"]], country_bounding_box[["ymax"]])) %>%
  st_sfc(crs = 3106)
top_right <- st_point(c(country_bounding_box[["xmax"]], country_bounding_box[["ymax"]])) %>%
  st_sfc(crs = 3106)

width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if (width > height) {
  width_ratio <- 1
  height_ratio <- height / width
} else {
  width_ratio <- width / height
  height_ratio <- 1
}

base_res_coeff <- 2 # 2
base_res <- 1000 * base_res_coeff

width_res <- floor(base_res * width_ratio)
height_res <- floor(base_res * height_ratio)

pop_raster <- st_rasterize(
  country_hex_population_file,
  nx = width_res %>% as.numeric(),
  ny = height_res %>% as.numeric()
)

pop_matrix <- matrix(
  pop_raster[["population"]],
  nrow = width_res,
  ncol = height_res
)

interpolated_colors <- colorRampPalette(color, bias = color_bias)(256)
# swatchplot(interpolated_colors)

rgl::close3d()

pop_matrix %>%
  # maps elevation to color for each point
  height_shade(texture = interpolated_colors) %>%
  plot_3d(
    heightmap = pop_matrix,
    zscale = zscale / base_res_coeff,
    solid = FALSE,
    shadowdepth = 0,
    shadowcolor = shadowcolor,
  )

render_camera(theta = camera_theta, phi = camera_phi, zoom = camera_zoom)

# interactive chart view
rgl::rglwidget()

outputfile <- glue::glue(output_file_name)

# render_snapshot(outputfile, fmt = "png", width = 1000, height = 1000)
# snapshot3d(outputfile, fmt = "png", width = 1000, height = 1000)

# render_highquality(
#   filename = outputfile,
#   interactive = FALSE,
#   lightcolor = render_3d_lightcolor,
#   lightaltitude = render_3d_lightaltitude,
#   lightdirection = render_3d_lightdirection,
#   lightintensity = render_3d_lightintensity,
#   width = 6000, # 6000
#   height = 6000, # 6000
#   samples = 300 # uncomment to increase quality while rendering full-size file
# )
