
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
library(ggrepel)
library(jpeg)

# 1. Coordinates for points
sites <- data.frame(
        name = c("Mall Road, Shimla", "M.G. Road, Gangtok"),
        lon = c(77.1734, 88.6138),
        lat = c(31.1048, 27.3314)
)
sites_sf <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)

# 2. Country outline and neighbors (Natural Earth, public-domain)
india <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Neighboring countries of India (land and maritime Sri Lanka)
neighbor_names <- c("Pakistan","China","Nepal","Bhutan","Bangladesh","Myanmar","Sri Lanka")
neighbors <- world[world$admin %in% neighbor_names | world$name %in% neighbor_names, ]

# 2a. Himalayan region approximation
# We approximate the Himalayan belt using Indian Himalayan states + Nepal and Bhutan.
# This is a cartographic approximation (admin boundaries) suitable for highlighting.
states_india <- tryCatch(ne_states(country = "India", returnclass = "sf"), error = function(e) NULL)

himalayan_states_names <- c(
        "Jammu and Kashmir","Jammu & Kashmir","Ladakh",
        "Himachal Pradesh","Uttarakhand","Sikkim","Arunachal Pradesh"
)

himalayan_states <- NULL
if (!is.null(states_india)) {
        himalayan_states <- states_india[states_india$name %in% himalayan_states_names, ]
}

# Add Nepal and Bhutan to the Himalayan band
himalayan_neighbors <- neighbors[neighbors$admin %in% c("Nepal","Bhutan") | neighbors$name %in% c("Nepal","Bhutan"), ]

# Combine available pieces into a single polygon; gracefully handle missing states layer
pieces <- list()
if (!is.null(himalayan_states) && nrow(himalayan_states) > 0) pieces[[length(pieces)+1]] <- st_geometry(himalayan_states)
if (!is.null(himalayan_neighbors) && nrow(himalayan_neighbors) > 0) pieces[[length(pieces)+1]] <- st_geometry(himalayan_neighbors)

himalaya_sf <- NULL
if (length(pieces) > 0) {
        himalaya_geom <- st_union(do.call(c, pieces))
        himalaya_sf <- st_sf(layer = "Himalayan region", geometry = himalaya_geom)
        st_crs(himalaya_sf) <- st_crs(4326)
}

# International boundary lines (for clear borders)
boundaries <- tryCatch(
        ne_download(scale = "medium", type = "admin_0_boundary_lines_land", category = "cultural", returnclass = "sf"),
        error = function(e) NULL
)

# Label positions for neighboring countries (use point-on-surface to avoid off-centroid issues)
neighbors_pts <- tryCatch(st_point_on_surface(neighbors), error = function(e) NULL)
neighbors_lbl_df <- NULL
if (!is.null(neighbors_pts)) {
        coords <- st_coordinates(neighbors_pts)
        neighbors_lbl_df <- cbind(data.frame(admin = neighbors$admin), as.data.frame(coords))
}

# 3. Main map: India with neighbors and Himalayan highlight
main_map <- ggplot() +
                                # Neighboring countries
                                geom_sf(data = neighbors, aes(fill = "Neighboring countries"), color = "grey70", size = 0.2) +
                                # India base
                                geom_sf(data = india, aes(fill = "India"), color = "grey60", size = 0.3) +
                                # Himalayan region overlay (if available)
                                {if (!is.null(himalaya_sf)) geom_sf(data = himalaya_sf, aes(fill = layer), color = "#2c7fb8", size = 0.3, alpha = 0.25) } +
                                # International borders linework (if available)
                                {if (!is.null(boundaries)) geom_sf(data = boundaries, color = "grey40", size = 0.3) } +
        annotation_scale(location = "bl", width_hint = 0.25) +
        annotation_north_arrow(location = "bl", which_north = "true",
                               pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                               style = north_arrow_fancy_orienteering) +
        geom_sf(data = sites_sf, aes(geometry = geometry), color = "#d73027", size = 3) +
                                # Neighboring country labels
                                {if (!is.null(neighbors_lbl_df)) geom_text_repel(data = neighbors_lbl_df,
                                                                aes(x = X, y = Y, label = admin), size = 3, family = "serif",
                                                                min.segment.length = 0, box.padding = 0.3, point.padding = 0.1,
                                                                max.overlaps = Inf) } +
        geom_text(data = sites, aes(x = lon, y = lat, label = name),
                  size = 3, hjust = 0, nudge_x = 1.0, family = "serif") +
        coord_sf(xlim = c(68, 98), ylim = c(6, 36), expand = FALSE) + # frame India
                                scale_fill_manual(
                                        name = "",
                                        values = c(
                                                "India" = "#f5f5f5",
                                                "Neighboring countries" = "#fee8c8",
                                                "Himalayan region" = "#a1d99b"
                                        ),
                                        breaks = c("India","Neighboring countries","Himalayan region")
                                ) +
                                labs(title = "Study area: Shimla (Mall Road) and Gangtok (M.G. Road)",
                                                 caption = "Data: Natural Earth (public domain); points: author coordinates") +
        theme_minimal(base_family = "serif") +
                                theme(plot.title = element_text(size = 12, face = "bold"),
                                                        legend.position = "right")
main_map

# 4. Add representative photos and compose with main map only (no insets)
# Read Image in JPG format
img1 <- rasterGrob(readJPEG("Plots/Map_Locations/images/Shimla_Image.jpg"), interpolate=TRUE)
img2 <- rasterGrob(readJPEG("Plots/Map_Locations/images/Gangtok_Image.jpg"), interpolate=TRUE)

# Arrange the two images side-by-side below the map
images_row <- plot_grid(ggdraw(img1), ggdraw(img2), nrow = 1, rel_widths = c(1, 1))
final_plot <- plot_grid(main_map, images_row, ncol = 1, rel_heights = c(2, 1))

# 5. Save output (300 dpi)
outfile_png <- "Figure1_StudyArea_Map.png"
ggsave(outfile_png, final_plot, width = 8, height = 10, dpi = 300, units = "in", bg = "white")

# For TIFF (preferred by many journals)
outfile_tiff <- "Figure1_StudyArea_Map.tiff"
tiff(outfile_tiff, width = 8, height = 10, units = "in", res = 300, compression = "lzw")
print(final_plot)
dev.off()
