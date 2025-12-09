#run by typing shiny::runApp('app.R') in the console
#data sources: https://www.kaggle.com/datasets/paramvir705/airbnb-data
#data sources: https://www.kaggle.com/datasets/ahmedshahriarsakib/usa-real-estate-dataset


library(shiny)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(scales)

# Keep the same property types used in the M5 model
keep_types <- c("Apartment", "House", "Condominium", "Townhouse", "Loft")
default_room_type <- NULL

prepare_model_data <- function(path) {
  Airbnb <- read_csv(path, show_col_types = FALSE)

  Airbnb2 <- Airbnb %>%
    mutate(
      amenities_clean = stringr::str_remove_all(amenities, "[\\{\\}\"]"),
      amenities_list  = stringr::str_split(amenities_clean, pattern = ",")
    ) %>%
    mutate(
      amenities_list = purrr::map(amenities_list, ~ stringr::str_trim(.x)),
      has_wifi       = purrr::map_lgl(amenities_list, ~ "Wireless Internet" %in% .x),
      has_kitchen    = purrr::map_lgl(amenities_list, ~ "Kitchen" %in% .x),
      has_aircon     = purrr::map_lgl(amenities_list, ~ "Air conditioning" %in% .x),
      has_tv         = purrr::map_lgl(amenities_list, ~ "TV" %in% .x),
      amenity_count  = purrr::map_int(amenities_list, length)
    ) %>%
    mutate(
      has_wifi      = as.integer(has_wifi),
      has_kitchen   = as.integer(has_kitchen),
      has_aircon    = as.integer(has_aircon),
      has_tv        = as.integer(has_tv),
      cleaning_fee  = as.integer(cleaning_fee),
      property_type = factor(property_type),
      room_type     = factor(room_type)
    )

  ModelData <- Airbnb2 %>%
    select(log_price,
           accommodates, bathrooms, bedrooms,
           number_of_reviews, review_scores_rating,
           amenity_count, has_wifi, has_kitchen, has_aircon, has_tv,
           property_type, room_type) %>%
    drop_na() %>%
    filter(property_type %in% keep_types) %>%
    droplevels()

  ranges <- list(
    accommodates        = range(ModelData$accommodates),
    bathrooms           = range(ModelData$bathrooms),
    bedrooms            = range(ModelData$bedrooms),
    number_of_reviews   = range(ModelData$number_of_reviews),
    review_scores_rating = range(ModelData$review_scores_rating),
    amenity_count       = range(ModelData$amenity_count)
  )

  defaults <- list(
    accommodates        = median(ModelData$accommodates),
    bathrooms           = median(ModelData$bathrooms),
    bedrooms            = median(ModelData$bedrooms),
    number_of_reviews   = median(ModelData$number_of_reviews),
    review_scores_rating = median(ModelData$review_scores_rating, na.rm = TRUE),
    amenity_count       = median(ModelData$amenity_count)
  )

  list(data = ModelData, ranges = ranges, defaults = defaults)
}

prep <- prepare_model_data("Airbnb_Data.csv")
ModelData <- prep$data
slider_ranges <- prep$ranges
defaults <- prep$defaults

default_room_type <- if ("Entire home/apt" %in% levels(ModelData$room_type)) {
  "Entire home/apt"
} else {
  levels(ModelData$room_type)[1]
}

M5_model <- lm(log_price ~ accommodates + bathrooms + bedrooms +
                 number_of_reviews + review_scores_rating +
                 amenity_count + has_wifi + has_kitchen + has_aircon + has_tv +
                 property_type + room_type,
               data = ModelData)

tree_model <- rpart(log_price ~ accommodates + bathrooms + bedrooms +
                      amenity_count + property_type + room_type,
                    data = ModelData, method = "anova")

# Load and trim the real estate data once
realtor_data <- read_csv("realtor-data.zip.csv", show_col_types = FALSE) %>%
  filter(status == "for_sale", !is.na(price), !is.na(zip_code)) %>%
  select(price, bed, bath, house_size, zip_code, street, city, state) %>%
  mutate(zip_code = as.character(zip_code))

ui <- fluidPage(
  titlePanel("Airbnb Price Demo — M5 Linear Regression vs Decision Tree"),
  sidebarLayout(
    sidebarPanel(
      helpText("Adjust listing traits to compare the two models."),
      sliderInput("accommodates", "Accommodates",
                  min = slider_ranges$accommodates[1],
                  max = slider_ranges$accommodates[2],
                  value = defaults$accommodates, step = 1),
      sliderInput("bathrooms", "Bathrooms",
                  min = slider_ranges$bathrooms[1],
                  max = slider_ranges$bathrooms[2],
                  value = defaults$bathrooms, step = 0.5),
      sliderInput("bedrooms", "Bedrooms",
                  min = slider_ranges$bedrooms[1],
                  max = slider_ranges$bedrooms[2],
                  value = defaults$bedrooms, step = 1),
      sliderInput("number_of_reviews", "Number of reviews",
                  min = slider_ranges$number_of_reviews[1],
                  max = slider_ranges$number_of_reviews[2],
                  value = defaults$number_of_reviews, step = 1),
      sliderInput("review_scores_rating", "Review score rating",
                  min = slider_ranges$review_scores_rating[1],
                  max = slider_ranges$review_scores_rating[2],
                  value = defaults$review_scores_rating, step = 0.1),
      sliderInput("amenity_count", "Amenity count",
                  min = slider_ranges$amenity_count[1],
                  max = slider_ranges$amenity_count[2],
                  value = defaults$amenity_count, step = 1),
      selectInput("property_type", "Property type",
                  choices = levels(ModelData$property_type)),
      selectInput("room_type", "Room type",
                  choices = levels(ModelData$room_type)),
      checkboxInput("has_wifi", "Has WiFi", value = TRUE),
      checkboxInput("has_kitchen", "Has kitchen", value = TRUE),
      checkboxInput("has_aircon", "Has air conditioning", value = TRUE),
      checkboxInput("has_tv", "Has TV", value = TRUE),
      hr(),
      helpText("Investment finder — zip-based search across homes for sale."),
      textInput("zip_filter", "Zip code", value = ""),
      sliderInput("occupancy", "Occupancy rate", min = 0.3, max = 0.9,
                  value = 0.7, step = 0.05),
      numericInput("interest_rate", "Mortgage rate (annual %)", 6.5, min = 0, max = 20, step = 0.1),
      numericInput("down_payment", "Down payment (%)", 20, min = 0, max = 90, step = 1),
      numericInput("property_tax_rate", "Property tax rate (%)", 1.1, min = 0, max = 5, step = 0.1),
      numericInput("maintenance_rate", "Annual maintenance (% of price)", 1, min = 0, max = 10, step = 0.1),
      numericInput("insurance_annual", "Insurance (annual $)", 1200, min = 0, step = 100),
      actionButton("find_deals", "Find top 5 profitable homes")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Model comparison",
          fluidRow(
            column(
              6,
              h4("M5 Linear Regression"),
              verbatimTextOutput("pred_m5"),
              h5("Model fit (training R-squared)"),
              verbatimTextOutput("m5_r2")
            ),
            column(
              6,
              h4("Decision Tree"),
              verbatimTextOutput("pred_tree"),
              plotOutput("tree_plot", height = "350px")
            )
          ),
          h4("Listing inputs used for prediction"),
          tableOutput("input_echo")
        ),
        tabPanel(
          "Top homes by zip",
          h4("Assumed economics"),
          helpText("Revenue uses the M5 nightly price estimate * 365 * occupancy. Costs include 30-year mortgage, property tax, maintenance, and insurance."),
          h4("Results"),
          textOutput("deal_status"),
          tableOutput("top_homes")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  new_listing <- reactive({
    data.frame(
      accommodates = input$accommodates,
      bathrooms = input$bathrooms,
      bedrooms = input$bedrooms,
      number_of_reviews = input$number_of_reviews,
      review_scores_rating = input$review_scores_rating,
      amenity_count = input$amenity_count,
      has_wifi = as.integer(input$has_wifi),
      has_kitchen = as.integer(input$has_kitchen),
      has_aircon = as.integer(input$has_aircon),
      has_tv = as.integer(input$has_tv),
      property_type = factor(input$property_type, levels = levels(ModelData$property_type)),
      room_type = factor(input$room_type, levels = levels(ModelData$room_type))
    )
  })

  output$pred_m5 <- renderText({
    log_price <- predict(M5_model, newdata = new_listing())
    nightly_price <- exp(log_price)
    paste0("Predicted log price: ", round(log_price, 3),
           " | Nightly price estimate: $", round(nightly_price, 2))
  })

  output$pred_tree <- renderText({
    log_price <- predict(tree_model, newdata = new_listing())
    nightly_price <- exp(log_price)
    paste0("Predicted log price: ", round(log_price, 3),
           " | Nightly price estimate: $", round(nightly_price, 2))
  })

  output$tree_plot <- renderPlot({
    rpart.plot(tree_model, main = "Decision Tree Structure", type = 2, extra = 101)
  })

  output$input_echo <- renderTable({
    listing <- new_listing()
    listing$has_wifi <- ifelse(listing$has_wifi == 1, "Yes", "No")
    listing$has_kitchen <- ifelse(listing$has_kitchen == 1, "Yes", "No")
    listing$has_aircon <- ifelse(listing$has_aircon == 1, "Yes", "No")
    listing$has_tv <- ifelse(listing$has_tv == 1, "Yes", "No")
    listing
  })

  output$m5_r2 <- renderText({
    r2 <- summary(M5_model)$r.squared
    paste0("R-squared: ", round(r2, 3))
  })

  investment_results <- eventReactive(input$find_deals, {
    zip_clean <- gsub("\\D", "", input$zip_filter)
    subset <- realtor_data %>% filter(zip_code == zip_clean)

    if (nrow(subset) == 0) {
      return(NULL)
    }

    listing_template <- function(df) {
      data.frame(
        accommodates = pmax(1, ifelse(is.na(df$bed), defaults$bedrooms, df$bed * 2)),
        bathrooms = ifelse(is.na(df$bath), defaults$bathrooms, df$bath),
        bedrooms = ifelse(is.na(df$bed), defaults$bedrooms, df$bed),
        number_of_reviews = defaults$number_of_reviews,
        review_scores_rating = defaults$review_scores_rating,
        amenity_count = defaults$amenity_count,
        has_wifi = 1,
        has_kitchen = 1,
        has_aircon = 1,
        has_tv = 1,
        property_type = factor("House", levels = levels(ModelData$property_type)),
        room_type = factor(default_room_type, levels = levels(ModelData$room_type))
      )
    }

    predict_revenue <- function(df) {
      listing_rows <- listing_template(df)
      log_price <- predict(M5_model, newdata = listing_rows)
      exp(log_price)
    }

    mortgage_cost <- function(price) {
      dp <- input$down_payment / 100
      rate <- input$interest_rate / 100
      principal <- price * (1 - dp)
      n <- 30 * 12
      monthly_rate <- rate / 12
      if (monthly_rate == 0) {
        return(principal / n * 12)
      }
      monthly <- principal * (monthly_rate / (1 - (1 + monthly_rate)^(-n)))
      monthly * 12
    }

    tax_cost <- function(price) price * (input$property_tax_rate / 100)
    maintenance_cost <- function(price) price * (input$maintenance_rate / 100)

    subset %>%
      mutate(
        nightly_rate = predict_revenue(subset),
        annual_revenue = nightly_rate * 365 * input$occupancy
      ) %>%
      mutate(
        annual_mortgage = mortgage_cost(price),
        annual_tax = tax_cost(price),
        annual_maintenance = maintenance_cost(price),
        annual_insurance = input$insurance_annual,
        annual_costs = annual_mortgage + annual_tax + annual_maintenance + annual_insurance,
        annual_profit = annual_revenue - annual_costs
      ) %>%
      arrange(desc(annual_profit)) %>%
      head(5) %>%
      transmute(
        street, city, state, zip_code,
        price = dollar(price, accuracy = 0.01),
        beds = bed,
        baths = bath,
        sqft = house_size,
        nightly_rate = dollar(nightly_rate, accuracy = 0.01),
        annual_revenue = dollar(annual_revenue, accuracy = 1),
        annual_costs = dollar(annual_costs, accuracy = 1),
        annual_profit = dollar(annual_profit, accuracy = 1)
      )
  })

  output$deal_status <- renderText({
    res <- investment_results()
    if (is.null(res)) {
      "No for-sale listings found for that zip code."
    } else {
      paste0("Showing up to 5 homes in zip ", gsub("\\D", "", input$zip_filter),
             " ranked by estimated annual profit.")
    }
  })

  output$top_homes <- renderTable({
    investment_results()
  })
}

shinyApp(ui = ui, server = server)
