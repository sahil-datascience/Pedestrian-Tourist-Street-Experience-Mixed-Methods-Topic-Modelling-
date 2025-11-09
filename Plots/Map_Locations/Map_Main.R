
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

# Helper: build a small image panel with 1–3 images and a title strip
build_image_panel <- function(title, img_paths, title_height = 0.06, title_y = 0.995) {
        # Read up to 3 images safely (support jpg/jpeg/png)
        imgs <- img_paths[seq_len(min(length(img_paths), 3))]
        read_any_image <- function(p) {
                ext <- tolower(tools::file_ext(p))
                if (!file.exists(p)) return(NULL)
                img <- switch(ext,
                                                                        "jpg" = readJPEG(p),
                                                                        "jpeg" = readJPEG(p),
                                                                        "png" = tryCatch({
                                                                                if (requireNamespace("png", quietly = TRUE)) png::readPNG(p) else return(NULL)
                                                                        }, error = function(e) NULL),
                                                                        NULL)
                if (is.null(img)) return(NULL)
                rasterGrob(img, interpolate = TRUE)
        }
        grobs <- lapply(imgs, read_any_image)
        grobs <- Filter(Negate(is.null), grobs)

        if (length(grobs) == 0) {
                body <- ggplot() + theme_void() +
                        annotate("text", x = 0.5, y = 0.5, label = "No images found", size = 3, family = "serif") +
                        xlim(0,1) + ylim(0,1)
        } else if (length(grobs) == 1) {
                body <- ggdraw(grobs[[1]])
        } else if (length(grobs) == 2) {
                body <- plot_grid(ggdraw(grobs[[1]]), ggdraw(grobs[[2]]), nrow = 1)
        } else if (length(grobs) == 3) {
                # Arrange three images vertically with small spacing
                body <- plot_grid(ggdraw(grobs[[1]]), ggdraw(grobs[[2]]), ggdraw(grobs[[3]]), ncol = 1, rel_heights = c(1,1,1))
        } else {
                body <- plot_grid(lapply(grobs, ggdraw), ncol = 1)
        }

        # Overlay the title text on the same canvas to eliminate any reserved row height
        ggdraw() +
                draw_plot(body) +
                cowplot::draw_text(title, x = 0.01, y = title_y, hjust = 0, vjust = 1,
                                   fontface = "bold", size = 10, fontfamily = "serif", lineheight = 0.85)
}

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

# Label anchor for country name
india_label_pt <- tryCatch({
        pt <- st_point_on_surface(india)
        as.numeric(st_coordinates(pt))
}, error = function(e) c(78, 22))  # fallback to a central lon/lat

# Major cities (Natural Earth populated places)
cities_world <- tryCatch(rnaturalearth::ne_cities(scale = "large", returnclass = "sf"), error = function(e) NULL)
cities_india <- NULL
major_cities <- NULL
if (!is.null(cities_world)) {
        cities_india <- subset(cities_world, adm0name == "India")
        if (nrow(cities_india) > 0) {
                major_cities <- subset(cities_india, (is.na(pop_max) == FALSE & pop_max >= 1500000) | grepl("Admin-0 capital", featurecla) | megacity == 1)
                # Ensure we at least show some key cities if threshold too strict
                if (nrow(major_cities) < 6) {
                        major_cities <- cities_india[order(-cities_india$pop_max), ][1:min(10, nrow(cities_india)), ]
                }
        }
}

# Manual fallback if automatic city selection failed or empty
if (is.null(major_cities) || nrow(major_cities) == 0) {
        fallback_cities <- data.frame(
                name = c("Delhi","Mumbai","Kolkata","Chennai","Bengaluru","Hyderabad","Ahmedabad","Pune","Jaipur","Lucknow"),
                lon = c(77.2090,72.8777,88.3639,80.2707,77.5946,78.4867,72.5714,73.8567,75.7873,80.9462),
                lat = c(28.6139,19.0760,22.5726,13.0827,12.9716,17.3850,23.0225,18.5204,26.9124,26.8467)
        )
        major_cities <- st_as_sf(fallback_cities, coords = c("lon","lat"), crs = 4326)
}

# Disputed area handling (Jammu & Kashmir / PoK) using Natural Earth disputed layers (attribute filter for Kashmir)
disputed_areas_all <- tryCatch(ne_download(scale = "medium", type = "admin_0_disputed_areas", category = "cultural", returnclass = "sf"), error = function(e) NULL)
disputed_lines_all <- tryCatch(ne_download(scale = "medium", type = "admin_0_boundary_lines_disputed_areas", category = "cultural", returnclass = "sf"), error = function(e) NULL)

disputed_kashmir <- NULL
disputed_kashmir_lines <- NULL
if (!is.null(disputed_areas_all)) {
        kash_filter <- grepl("kashmir", paste(disputed_areas_all$name, disputed_areas_all$namealt, disputed_areas_all$adm0_a, disputed_areas_all$adm0_b, sep = " "), ignore.case = TRUE)
        kashmir_poly <- disputed_areas_all[kash_filter, ]
        if (nrow(kashmir_poly) > 0) {
                # Tight bbox to avoid spill into Himachal: slightly north-west focused
                kashmir_bbox <- st_as_sfc(st_bbox(c(xmin = 73, xmax = 77.8, ymin = 33, ymax = 35.8), crs = st_crs(4326)))
                clipped <- tryCatch(suppressWarnings(st_intersection(kashmir_poly, kashmir_bbox)), error = function(e) kashmir_poly)
                if (nrow(clipped) > 0) disputed_kashmir <- clipped else disputed_kashmir <- kashmir_poly
        }
}
if (!is.null(disputed_lines_all)) {
        kash_filter_l <- grepl("kashmir", paste(disputed_lines_all$name, disputed_lines_all$namealt, sep = " "), ignore.case = TRUE)
        kashmir_lines <- disputed_lines_all[kash_filter_l, ]
        if (nrow(kashmir_lines) > 0) {
                kashmir_bbox <- st_as_sfc(st_bbox(c(xmin = 73, xmax = 77.8, ymin = 33, ymax = 35.8), crs = st_crs(4326)))
                clipped_l <- tryCatch(suppressWarnings(st_intersection(kashmir_lines, kashmir_bbox)), error = function(e) kashmir_lines)
                if (nrow(clipped_l) > 0) disputed_kashmir_lines <- clipped_l else disputed_kashmir_lines <- kashmir_lines
        }
}

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
                                # State boundaries (outline only) for India
                                {if (!is.null(states_india)) geom_sf(data = states_india, fill = NA, color = "grey55", size = 0.25) } +
                                # Himalayan region overlay (if available)
                                {if (!is.null(himalaya_sf)) geom_sf(data = himalaya_sf, aes(fill = layer), color = "#2c7fb8", size = 0.3, alpha = 0.25) } +
                                # International borders linework (if available)
                                {if (!is.null(boundaries)) geom_sf(data = boundaries, color = "grey40", size = 0.3) } +
                                # Disputed Kashmir (neutral fill + dashed outline)
                                {if (!is.null(disputed_kashmir)) geom_sf(data = disputed_kashmir, fill = "#fde0dd", color = NA, alpha = 0.6) } +
                                {if (!is.null(disputed_kashmir_lines)) geom_sf(data = disputed_kashmir_lines, color = "#c51b8a", size = 0.5, linetype = "dashed") } +
        # Major Indian cities (points and labels)
        {if (!is.null(major_cities) && nrow(major_cities) > 0) geom_sf(data = major_cities, shape = 21, size = 1.8, stroke = 0.2, fill = "#111111", color = "white") } +
        {if (!is.null(major_cities) && nrow(major_cities) > 0) ggrepel::geom_text_repel(data = st_drop_geometry(major_cities),
                aes(x = st_coordinates(major_cities)[,1], y = st_coordinates(major_cities)[,2], label = name),
                size = 2.7, family = "serif", box.padding = 0.15, point.padding = 0.1, min.segment.length = 0,
                max.overlaps = Inf, color = "#2b2b2b") } +
        annotation_scale(location = "bl", width_hint = 0.25) +
        annotation_north_arrow(location = "bl", which_north = "true",
                               pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                               style = north_arrow_fancy_orienteering) +
        geom_sf(data = sites_sf, aes(geometry = geometry), color = "#d73027", fill = "#d73027", size = 3.2, shape = 21, stroke = 0.5) +
        # Numbered callouts for sites (link to image panels)
        geom_label(data = transform(sites, id = c("1","2")),
                   aes(x = lon, y = lat, label = id),
                   size = 3, family = "serif", label.size = 0.2,
                   label.padding = unit(0.1, "lines"), fill = "white", alpha = 0.85) +
                                # Neighboring country labels
                                {if (!is.null(neighbors_lbl_df)) geom_text_repel(data = neighbors_lbl_df,
                                                                aes(x = X, y = Y, label = admin), size = 3, family = "serif",
                                                                min.segment.length = 0, box.padding = 0.3, point.padding = 0.1,
                                                                max.overlaps = Inf) } +
        # Country label
        annotate("text", x = india_label_pt[1], y = india_label_pt[2], label = "India",
                 family = "serif", fontface = "bold", size = 5, color = "#303030", alpha = 0.9) +
        {if (!is.null(disputed_kashmir)) annotate("text", x = 74.5, y = 34.5, label = "PoK (disputed)",
                  family = "serif", fontface = "italic", size = 3.5, color = "#444444", alpha = 0.9) } +
        geom_text(data = sites, aes(x = lon, y = lat, label = name),
                  size = 4.2, fontface = "bold", hjust = 0, nudge_x = 1.0, family = "serif", color = "#d73027") +
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
                                     x = "Longitude (°E)", y = "Latitude (°N)",
                                     caption = "Data: Natural Earth (public domain); points: author coordinates") +
        theme_minimal(base_family = "serif") +
                                theme(plot.title = element_text(size = 12, face = "bold"),
                                                       legend.position = c(0.83, 0.17),
                                                       legend.background = element_rect(fill = adjustcolor("white", alpha.f = 0.85), color = "grey70", linewidth = 0.2),
                                                       legend.key = element_rect(fill = NA))
main_map

# 4. Publication layout: map + image panels (2–3 per site) side-by-side
imgs_dir <- "Plots/Map_Locations/images"
shimla_imgs <- list.files(imgs_dir, pattern = "(?i)^Shimla.*\\.(jpg|jpeg|png)$", full.names = TRUE)
gangtok_imgs <- list.files(imgs_dir, pattern = "(?i)^Gangtok.*\\.(jpg|jpeg|png)$", full.names = TRUE)

shimla_panel <- build_image_panel("1. Mall Road, Shimla (images)", shimla_imgs, title_height = 0.05)
gangtok_panel <- build_image_panel("2. M.G. Road, Gangtok (images)", gangtok_imgs, title_height = 0.05)

right_col <- plot_grid(shimla_panel, ggdraw(), gangtok_panel, ncol = 1, rel_heights = c(2, 0.02, 2))

# Wrap columns with ggdraw and zero margins to enforce equal heights in SVG export
left_plot  <- ggdraw(main_map)  + theme(plot.margin = margin(0, 0, 0, 0))
right_plot <- ggdraw(right_col) + theme(plot.margin = margin(0, 0, 0, 0))

final_plot <- plot_grid(
        left_plot,
        right_plot,
        ncol = 2,
        rel_widths = c(1.3, 1.7),   # give more space to images than map
        align = "h"
) + theme(plot.margin = margin(2, 4, 2, 2))

final_plot


# 5. Save output (300 dpi)
outfile_svg <- "Plots/Map_Locations/Figure1_StudyArea_Map.svg"
ggsave(outfile_svg, final_plot, width = 15, height = 7.5, units = "in", bg = "white", device = "svg")

