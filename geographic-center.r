install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "ggplot2"))
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("ropensci/rnaturalearthhires")
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


# La semilla es para reproducibilidad
seed <- 1743
set.seed(seed)

# Cargar datos geograficos de Mexico y Zacatecas 
world <- ne_countries(scale = "medium", returnclass = "sf")
mexico <- subset(world, admin == "Mexico")
mex_states <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MEX_1.json")
mex_munis <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MEX_2.json")
zacatecas_munis <- subset(mex_munis, NAME_1 == "Zacatecas")

# Maximas coordenadas en latitud y longitud de Mexico
bbox <- st_bbox(mexico)
coords <- c(
  xmin = floor(bbox[["xmin"]]),
  xmax = ceiling(bbox[["xmax"]]),
  ymin = floor(bbox[["ymin"]]),
  ymax = ceiling(bbox[["ymax"]])
)

# Ejemplo de un punto aleatorio
random.point <- c(
  runif(1, coords[["xmin"]], coords[["xmax"]]),  # random longitude
  runif(1, coords[["ymin"]], coords[["ymax"]])   # random latitude
)

# Convertir el punto a un objeto sf
random_sf <- st_as_sf(
  data.frame(x = random.point[1], y = random.point[2]),
  coords = c("x", "y"),
  crs = 4326
)

# Verifica si este punto aleatorio esta dentro de Mexico
inside <- st_within(random_sf, mexico)
is_inside <- lengths(inside) > 0
is_inside

# Funcion que genera n puntos aleatorios dentro de Mexico como objetos sf usando el metodo Montecarlo
n_points_in_mx <- function(number) {
  points = data.frame(lat = numeric(0), lon = numeric(0))
  pb <- txtProgressBar(min = 0, max = number, style = 3)
  while (nrow(points) < number) {
    random.point <- c(
      runif(1, coords[["xmin"]], coords[["xmax"]]), 
      runif(1, coords[["ymin"]], coords[["ymax"]])   
    )
    random_sf <- st_as_sf(
    data.frame(x = random.point[1], y = random.point[2]),
      coords = c("x", "y"),
      crs = 4326
    )
    inside <- st_within(random_sf, mexico)
    is_inside <- lengths(inside) > 0
    if (is_inside) {
      points <- rbind(points, random_sf)
    }
    setTxtProgressBar(pb, nrow(points))
  }
  close(pb)
  return(points)
}

# Ejemplo de 200 puntos aleatorios dentro de Mexico
random_points <- n_points_in_mx(200)
ggplot() +
    geom_sf(data = mex_states, fill = NA, color = "black", size = 0.5) +
    geom_sf(data = zacatecas_munis, fill = NA, color = "black") +
    geom_sf(data = random_points, color = "red", size = 1) +
    theme_minimal()

# Funcion que genera n puntos aleatorios dentro de Mexico como data.frame sin usar sf
n_points_no_sf <- function(number) {
  points = data.frame(lat = numeric(0), lon = numeric(0))
  while (nrow(points) < number) {
    random.point <- c(
      runif(1, coords[["xmin"]], coords[["xmax"]]),
      runif(1, coords[["ymin"]], coords[["ymax"]]) 
    )
    random_sf <- st_as_sf(
      data.frame(lon = random.point[1], lat = random.point[2]),
      coords = c("lon", "lat"),
      crs = 4326
    )
    is_inside <- lengths(st_within(random_sf, mexico)) > 0
    if (is_inside) {
      points <- rbind(points, data.frame(lat = random.point[2], lon = random.point[1]))
    }
  }
  return(points)
}


# Funcion que genera n puntos medios tomando un total de "trials" de puntos aleatorios dentro de Mexico
n_midpoints_no_sf<- function(number, trials) {
mid_points <- data.frame(lat = numeric(0), lon = numeric(0))
pb <- txtProgressBar(min = 0, max = number, style = 3)
for (i in 1:number) {
    random_points <- n_points_no_sf(trials)
    mid_lat <- mean(random_points$lat)
    mid_lon <- mean(random_points$lon)
    mid_points <- rbind(mid_points, data.frame(lat = mid_lat, lon = mid_lon))
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(mid_points)
}

# Funcion que genera y grafica n puntos medios tomando un total de "trials" de puntos aleatorios dentro de Mexico
plotting_midpoints <- function(number, trials) {
  mid_points <- n_midpoints_no_sf(number, trials)
  mid_points <- st_as_sf(mid_points, coords = c("lon","lat"), crs = 4326)
  ggplot() +
    geom_sf(data = mex_states, fill = NA, color = "black", size = 0.5) +
    geom_sf(data = mid_points, color = "blue", size = 1) +
    geom_sf(data = zacatecas_munis, fill = NA, color = "black") +
    theme_minimal()
}

# Esta funcion genera una aproximacion del gravicentro de Mexico. Para ello, genera "number" cantidad de puntos medios usando "trials"
# cantidad de puntos aleatorios dentro de Mexico. Los puntos medios obtenidos se usan como muestra para calcular un nuevo gravicentro.
# El tomar una muestra permite calcular un intervalo de confianza; se hace un intervalo de confianza del 95% y dependiendo del tamaño
# de la muestra se usa la distribucion t-student o normal. La funcion regresa el tiempo de ejecucion, la grafica, el punto medio aproximado
# y el poligono dado por el intervalo de confianza.
approx_centre <- function(number, trials) {
    start_time <- Sys.time()
    mid_points <- n_midpoints_no_sf(number, trials)
    mean_lon <- mean(mid_points$lon)
    mean_lat <- mean(mid_points$lat)
    se_lat <- sd(mid_points$lat)/sqrt(number)
    se_lon <- sd(mid_points$lon)/sqrt(number)
    approx_midpoint <- st_as_sf(data.frame(lon = mean_lon, lat = mean_lat),
                            coords = c("lon","lat"), crs = 4326)                      
    if (number <= 30) {
        t_val <- qt(0.975, df = number-1)
        ci_lon <- c(mean_lon - t_val*se_lon, mean_lon + t_val*se_lon)
        ci_lat <- c(mean_lat - t_val*se_lat, mean_lat + t_val*se_lat)
    }
    else {
       z_val <- qnorm(0.975)
       ci_lon <- c(mean_lon - z_val*se_lon, mean_lon + z_val*se_lon)
       ci_lat <- c(mean_lat - z_val*se_lat, mean_lat + z_val*se_lat)
    }
    ci_square <- st_as_sfc(st_bbox(c(xmin = ci_lon[1], xmax = ci_lon[2],
                                 ymin = ci_lat[1], ymax = ci_lat[2])))
    st_crs(ci_square) <- 4326
    plot <- ggplot() +
        geom_sf(data = mex_states, fill = "lightgray", color = "black", size = 0.5) +
        geom_sf(data = zacatecas_munis, fill = NA, color = "black", size = 0.25) +
        geom_sf(data = approx_midpoint, color = "red", size = 0.1) +
        geom_sf(data = ci_square, fill = "lightcoral", color = "darkred", 
            linetype = "dashed", size = 0.1, alpha = 0.1) +
        theme_minimal() +
        ggtitle(sprintf("Approximate midpoint and 95%% CI
            \n%d points, each obtained from %d trials", 
                  number, trials))
    end_time <- Sys.time()
    runtime <- end_time - start_time
  return(list(runtime = runtime, plot = plot, approx_midpoint = approx_midpoint, 
         ci_square = ci_square))
}

approx_centre(50, 1000)
ggsave("my_plot.pdf", plot = the_map , width = 7, height = 5)


