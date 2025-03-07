library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(tidyr)
library(tidyverse)
library(httpgd)
library(languageserver)
library(stringr)
library(forcats)

# ! ---- COSTS 2025: 1300 USD (Groceries, Restaurants ------ !

# Load data
data <- data.frame(
    state = character(),
    place = character(),
    position = character(),
    hourly_wage = numeric(),
    starting_date_from = as.Date(character()),
    starting_date_to = as.Date(character()),
    accommodation_weekly = numeric(),
    visa_fee = numeric(),
    registration_fee = numeric(),
    other_costs = numeric(),
    hours_per_week = numeric(),
    ot_hours_per_week = numeric(),
    tips = logical()
)

data <- rbind(data, data.frame(
    state = c(
        "South Carolina",
        "Virginia",
        "Alaska",
        "Texas",
        "New York",
        "Boston",
        "Washington",
        "Washington DC",
        "New York",
        "California",
        "Hawaii",
        "Hawaii"
    ),
    place = c(
        "Myrtle Beach",
        "Northern Virginia",
        "Hoonah",
        "Montgomery",
        "Amangansett",
        "Boston",
        "Seattle-reference",
        "Georgetown",
        "Buffalo",
        "Tuckertee",
        "Maui",
        "Kamuela"
    ),
    position = c(
        "Ocean Lifeguard",
        "Lifeguard",
        "Guest Services",
        "Barista",
        "Waiter",
        "Lifeguard",
        "Customer Service",
        "Lifeguard",
        "Retail",
        "Crew member",
        "Housekeeping",
        "Steward"
    ),
    hourly_wage = c(17, 15, 18, 12, 11, 17, 20, 15.5, 18, 17, 23, 22),
    accommodation_weekly = c(125, 160, 200, 150, 125, 145, 200, 160, 75, 150, 150, 170),
    visa_fee = c(1390, 1390, 1390, 1390, 1390, 1150, 1390, 1150, 1390, 1150, 1390, 1390),
    registration_fee = c(330 , 330, 330, 330, 330, 700, 330, 700, 330, 330, 330, 330),
    other_costs = c(200, 0, 100, 250, 0, 0, 0, 70, 20, 600, 200, 0),
    hours_per_week = c(40, 40, 40, 40, 40, 40, 40, 40, 32, 40, 40, 40),
    ot_hours_per_week = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    tips = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
))

# ! Removing positions I am not considering in
data <- data %>%
    filter(!(place %in% c(
        "Myrtle Beach",
        "Montgomery",
        "Amangansett",
        "Boston",
        "Hoonah"
    )))

# * Scenarios
scenarios <- list(
    list(name = "32 hours per week", hours_per_week = 32, ot_hours_per_week = 0),
    list(name = "40 hours per week", hours_per_week = 40, ot_hours_per_week = 0),
    list(name = "45 hours per week (5 OT)", hours_per_week = 40, ot_hours_per_week = 5),
    list(name = "50 hours per week (10 OT)", hours_per_week = 40, ot_hours_per_week = 10),
    list(name = "55 hours per week (15 OT)", hours_per_week = 40, ot_hours_per_week = 15),
    list(name = "60 hours per week (20 OT)", hours_per_week = 40, ot_hours_per_week = 20)
)

# * Define default duration (in weeks)
total_weeks <- 11

#  * Net income in each scenario
scenario_results <- lapply(scenarios, function(s) {
    data %>%
        rowwise() %>%
        mutate(
            wage_with_tips = ifelse(
                tips,
                hourly_wage * runif(1, min = 1.5, max = 2.0),
                hourly_wage
            ),
            hours_per_week = s$hours_per_week,
            ot_hours_per_week = s$ot_hours_per_week,
            weekly_income = ((wage_with_tips * hours_per_week) +
                (ot_hours_per_week * 1.5 * wage_with_tips)) * 0.9,
            total_income = weekly_income * total_weeks,
            total_accommodation = accommodation_weekly * total_weeks,
            food_costs = 1300,
            net_income = total_income -
                total_accommodation -
                food_costs -
                visa_fee -
                180 - # Visa fee for J1
                registration_fee -
                other_costs,
            scenario = s$name
        ) %>%
        ungroup()
})

combined_scenarios <- bind_rows(scenario_results)

head(combined_scenarios)

combined_scenarios <- combined_scenarios %>%
    mutate(state_position = paste0(state, " (", position, ")"))

ggplot(combined_scenarios, aes(x = state_position, y = net_income, fill = scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(
        aes(label = round(net_income, 2)),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = "Net Income by State (Position) and Scenario",
        x = "State (Position)",
        y = "Net Income"
    ) +
    scale_fill_brewer(palette = "Set1")


ggplot(combined_scenarios, aes(x = state_position, y = net_income, fill = state_position)) +
    geom_bar(stat = "identity") +
    geom_text(
        aes(label = round(net_income, 2)),
        vjust = -0.5,
        size = 3
    ) +
    facet_wrap(~scenario, scales = "free_y") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
    ) +
    labs(
        title = "Net Income by State (Position) and Scenario w/o Flight Tickets - 10 weeks",
        x = "State (Position)",
        y = "Net Income",
        caption = "Note: Net income calculated for 10 weeks, costs inludes accommodation, food, visa and registration fees, and other costs"
    ) +
    scale_fill_brewer(palette = "Set1")



# ! Create realistic scenario with position-specific hours
realistic_scenario <- data %>%
    rowwise() %>%
    mutate(
        # Assign specific hours based on position and location
        hours_per_week = case_when(
            state == "Washington DC" ~ 40,
            TRUE ~ 40 # default for other positions
        ),
        ot_hours_per_week = case_when(
            state == "Washington DC" ~ 15, # 55 total hours
            state == "Virginia" ~ 13, # 53 total hours
            state == "Washington" ~ 5,
            place == "Buffalo" ~ 0,
            TRUE ~ 10 # 50 total hours default
        ),
        # Calculate income with specific hours
        wage_with_tips = ifelse(
            tips,
            hourly_wage * runif(1, min = 1.5, max = 1.),
            hourly_wage
        ),
        weekly_income = ((wage_with_tips * hours_per_week) +
            (ot_hours_per_week * 1.5 * wage_with_tips)) * 0.9,
        total_income = weekly_income * total_weeks,
        total_accommodation = accommodation_weekly * total_weeks,
        food_costs = 1300,
        net_income = total_income -
            total_accommodation -
            food_costs -
            visa_fee -
            180 -
            registration_fee -
            other_costs,
        state_position = paste0(state, " (", position, ")")
    ) %>%
    ungroup()

# Create the graph
ggplot(
    realistic_scenario,
    aes(
        x = reorder(state_position, net_income),
        y = net_income,
        fill = state_position
    )
) +
    geom_bar(stat = "identity") +
    geom_text(
        aes(label = round(net_income, 2)),
        vjust = -0.5,
        size = 3
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
    ) +
    labs(
        title = "Realistic Net Income Scenario by Position",
        subtitle = paste("Based on position-specific working hours (Default: 50h, DC: 55h, VA: 53h, WA: 45h, NY(Buffalo): 40h)"),
        x = "State (Position)",
        y = "Net Income",
        caption = "Note: Includes accommodation, food, visa fees, and all other costs for 10 weeks"
    ) +
    scale_fill_brewer(palette = "Set1")

# ! Weekly facet
# Define weeks vector
weeks_vector <- c(10, 11, 12)

# Create realistic scenarios for different durations
realistic_scenarios <- lapply(weeks_vector, function(weeks) {
    data %>%
        rowwise() %>%
        mutate(
            hours_per_week = case_when(
                state == "Washington DC" ~ 40,
                TRUE ~ 40
            ),
            ot_hours_per_week = case_when(
                state == "Washington DC" ~ 15,
                state == "Virginia" ~ 13,
                state == "Washington" ~ 5,
                place == "Buffalo" ~ 0,
                TRUE ~ 10
            ),
            wage_with_tips = ifelse(
                tips,
                hourly_wage * runif(1, min = 1.5, max = 1.8),
                hourly_wage
            ),
            weekly_income = ((wage_with_tips * hours_per_week) +
                (ot_hours_per_week * 1.5 * wage_with_tips)) * 0.9,
            total_income = weekly_income * weeks,
            total_accommodation = accommodation_weekly * weeks,
            food_costs = 130 * weeks,
            net_income = total_income -
                total_accommodation -
                food_costs -
                visa_fee -
                180 -
                registration_fee -
                other_costs,
            state_position = paste0(state, " (", position, ")"),
            total_weeks = weeks
        ) %>%
        ungroup()
}) %>%
    bind_rows()

# Create faceted graph with fixed number formatting
ggplot(
    realistic_scenarios,
    aes(
        x = reorder(state_position, net_income),
        y = net_income,
        fill = place
    )
) +
    geom_bar(stat = "identity") +
    geom_text(
        aes(label = round(net_income, 0)), # Simplified number formatting
        vjust = -0.5,
        size = 3
    ) +
    facet_wrap(~total_weeks,
        labeller = labeller(total_weeks = function(x) paste(x, "weeks"))
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        panel.spacing = unit(2, "lines")
    ) +
    labs(
        title = "Net Income by Position and Duration",
        subtitle = "Based on location-specific working hours and overtime",
        x = "Location and Position",
        y = "Net Income (USD)",
        caption = "Note: Includes accommodation, food, visa fees, and all other costs"
    ) +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::dollar_format())
