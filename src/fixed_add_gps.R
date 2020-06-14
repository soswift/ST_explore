custom_add_points <-function (raster_input, lat, long, alt, zscale, 
          colour = "red", alpha = 0.8, 
          raise_agl = 0, point_size = 20, rad = 2) 
{
  coords <- latlong_to_rayshader_coords(raster_input, lat, 
                                        long)
  distances_x <- coords$x
  distances_y <- coords$y

    sp_gps <- sp::SpatialPoints(cbind(long, lat), proj4string = sp::CRS("+init=epsg:4326"))
    sp_gps <- sp::spTransform(sp_gps, sp::CRS(as.character(raster::crs(raster_input))))
    gps_ground_line <- raster::extract(raster_input, sp_gps)
  

    track_altitude <- gps_ground_line + raise_agl
    
    return(list("x" = distances_x, "y" = track_altitude/zscale, "z" = -distances_y, "col" = colour, "alpha" = alpha, "size" = point_size, "radius" = rad))

}
