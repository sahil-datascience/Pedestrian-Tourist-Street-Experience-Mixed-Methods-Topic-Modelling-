
install.packages(c("sf","ggplot2","rnaturalearth","rnaturalearthdata",
                   "ggspatial","cowplot","png","grid"))
# If you want Stamen/Osm basemaps:
install.packages(c("ggmap","rosm"))  # optional

#--------------------------------------------------------------------#
#
#                             Map Script                             #
#
#--------------------------------------------------------------------#

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(cowplot)
library(png)
library(grid)

# 1. Coordinates for points
sites <- data.frame(
        name = c("Mall Road, Shimla", "M.G. Road, Gangtok"),
        lon = c(77.1734, 88.6138),
        lat = c(31.1048, 27.3314)
)
sites_sf <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)

# 2. Country outline (Natural Earth, public-domain)
india <- ne_countries(scale = "medium", country = "India", returnclass = "sf")

# 3. Main map: India with points
main_map <- ggplot() +
        geom_sf(data = india, fill = "grey95", color = "grey60", size = 0.3) +
        annotation_scale(location = "bl", width_hint = 0.25) +
        annotation_north_arrow(location = "bl", which_north = "true",
                               pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                               style = north_arrow_fancy_orienteering) +
        geom_sf(data = sites_sf, aes(geometry = geometry), color = "#d73027", size = 3) +
        geom_text(data = sites, aes(x = lon, y = lat, label = name),
                  size = 3, hjust = 0, nudge_x = 1.0, family = "serif") +
        coord_sf(xlim = c(68, 98), ylim = c(6, 36), expand = FALSE) + # frame India
        labs(title = "Study area: Shimla (Mall Road) and Gangtok (M.G. Road)",
             caption = "Data: Natural Earth (public domain); points: author coordinates") +
        theme_minimal(base_family = "serif") +
        theme(plot.title = element_text(size = 12, face = "bold"))

# 4. Inset Shimla zoom
shimla_bbox <- st_bbox(c(xmin = 76.6, xmax = 77.7, ymin = 30.6, ymax = 31.5), crs = st_crs(4326))
shimla_map <- ggplot() +
        geom_sf(data = india, fill = "grey95", color = "grey80") +
        geom_sf(data = sites_sf, color = "#d73027", size = 3) +
        coord_sf(xlim = c(shimla_bbox["xmin"], shimla_bbox["xmax"]),
                 ylim = c(shimla_bbox["ymin"], shimla_bbox["ymax"]), expand = FALSE) +
        labs(subtitle = "Shimla (Mall Road)") +
        theme_minimal(base_family = "serif") +
        theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_blank(),
              plot.subtitle = element_text(size = 9, face = "bold"))

# 5. Inset Gangtok zoom
gangtok_bbox <- st_bbox(c(xmin = 88.45, xmax = 88.78, ymin = 27.25, ymax = 27.45), crs = st_crs(4326))
gangtok_map <- ggplot() +
        geom_sf(data = india, fill = "grey95", color = "grey80") +
        geom_sf(data = sites_sf, color = "#d73027", size = 3) +
        coord_sf(xlim = c(gangtok_bbox["xmin"], gangtok_bbox["xmax"]),
                 ylim = c(gangtok_bbox["ymin"], gangtok_bbox["ymax"]), expand = FALSE) +
        labs(subtitle = "Gangtok (M.G. Road)") +
        theme_minimal(base_family = "serif") +
        theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_blank(),
              plot.subtitle = element_text(size = 9, face = "bold"))

# 6. Compose main + insets using cowplot
inset1 <- ggdraw(shimla_map)
inset2 <- ggdraw(gangtok_map)

combined <- ggdraw() +
        draw_plot(main_map, 0, 0, 1, 1) +
        draw_plot(inset1, x = 0.62, y = 0.60, width = 0.33, height = 0.33) +
        draw_plot(inset2, x = 0.62, y = 0.16, width = 0.33, height = 0.33) +
        draw_plot_label(c("A", "B", "C"), c(0.02, 0.62, 0.62), c(0.98, 0.94, 0.50),
                        size = 12, fontface = "bold")

# 7. (Optional) Add representative photos (must be CC-licensed). Example with local files:
# photos: "shimla_cc.png", "gangtok_cc.png" (ensure these are licensed)
# load and convert to grobs
# img1 <- rasterGrob(readPNG("shimla_cc.png"), interpolate=TRUE)
# img2 <- rasterGrob(readPNG("gangtok_cc.png"), interpolate=TRUE)
# You can then assemble with cowplot: use plot_grid(combined, ggdraw(img1), ggdraw(img2), nrow=2, rel_heights=c(2,1))

# 8. Save output (300 dpi)
outfile_png <- "Figure1_StudyArea_Map.png"
ggsave(outfile_png, combined, width = 8, height = 8, dpi = 300, units = "in", bg = "white")

# For TIFF (preferred by many journals)
outfile_tiff <- "Figure1_StudyArea_Map.tiff"
tiff(outfile_tiff, width = 8, height = 8, units = "in", res = 300, compression = "lzw")
print(combined)
dev.off()
