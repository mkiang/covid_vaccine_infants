## Figures


## Imports ----
library(tidyverse)
library(here)
library(readxl)
library(ggsci)
library(patchwork)

## Data ----
full_data <- readRDS(here("data", "vaccine_impact.RDS")) |>
    mutate(scenario_cat = factor(scenario,
        levels = c("observed", "vaccine100", "vaccine50", "vaccine15"),
        labels = c("No vaccination", "100% vaccination", "50% vaccination", "15% vaccination"),
        ordered = TRUE
    ))

## Helper function ----
generate_subplot <- function(current_data) {
    p_lines <- ggplot(
        current_data,
        aes(
            x = date,
            y = estimate,
            ymax = upper,
            ymin = lower,
            color = scenario_cat,
            fill = scenario_cat
        )
    ) +
        geom_ribbon(color = NA, alpha = .25) +
        geom_line(linewidth = 1) +
        scale_x_date("Date (weekly)",
            labels = scales::label_date_short(),
            expand = c(0, 1)
        ) +
        scale_y_continuous("Number of hospitalizations",
            expand = c(0, 1)
        ) +
        scale_fill_manual("Scenario", values = c("grey10", ggsci::pal_jama()(4)[2:4])) +
        scale_color_manual("Scenario", values = c("grey10", ggsci::pal_jama()(4)[2:4])) +
        theme_bw() +
        theme(
            legend.position = c(.98, .98),
            legend.justification = c(1, 1)
        )

    p_bars <- ggplot() +
        geom_col(
            data = current_data |>
                filter(scenario == "vaccine100"),
            aes(
                x = date,
                y = averted,
                fill = scenario_cat
            ),
            position = "identity",
            alpha = 1
        ) +
        geom_col(
            data = current_data |>
                filter(scenario == "vaccine50"),
            aes(
                x = date,
                y = averted,
                fill = scenario_cat
            ),
            position = "identity",
            alpha = 1
        ) +
        geom_col(
            data = current_data |>
                filter(scenario == "vaccine15"),
            aes(
                x = date,
                y = averted,
                fill = scenario_cat
            ),
            position = "identity",
            alpha = 1
        ) +
        scale_fill_manual("Scenario", values = ggsci::pal_jama()(4)[2:4]) +
        scale_color_manual("Scenario", values = ggsci::pal_jama()(4)[2:4]) +
        scale_x_date("Date (weekly)",
            labels = scales::label_date_short(),
            expand = c(0, 1)
        ) +
        scale_y_continuous("Averted hospitalizations",
            expand = c(0, 1)
        ) +
        theme_bw() +
        theme(legend.position = "none")

    p_lines + p_bars + plot_layout(ncol = 1, heights = c(4, 1))
}

plot_a <- generate_subplot(full_data |>
    filter(population == "infant"))
plot_b <- generate_subplot(full_data |>
    filter(population == "pregnant"))

ggsave(here("plots", "FigA_infants.pdf"),
    plot_a,
    width = 5,
    height = 4,
    scale = 1.25,
    device = cairo_pdf
)
ggsave(here("plots", "FigB_pregnant.pdf"),
    plot_b,
    width = 5,
    height = 4,
    scale = 1.25,
    device = cairo_pdf
)
ggsave(here("plots", "FigA_infants.jpg"),
    plot_a,
    width = 5,
    height = 4,
    scale = 1.25,
    dpi = 2000
)
ggsave(here("plots", "FigB_pregnant.jpg"),
    plot_b,
    width = 5,
    height = 4,
    scale = 1.25,
    dpi = 2000
)

write_csv(
    full_data,
    here("output", "figure_data.csv")
)
