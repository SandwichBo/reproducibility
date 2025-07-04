library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(glue)

# ---- 公共函数封装 ----

loss_plot <- function(dl_train_loss){
  long_df <- pivot_longer(
    dl_train_loss,
    cols = starts_with("loss_"),
    names_to = "model",
    values_to = "loss"
  )
  long_df$model <- sub("^loss_", "", long_df$model)
  
  ggplot(long_df, aes(x = epoch, y = loss, colour = model)) +
    geom_line(linewidth = 0.8) +
    scale_colour_brewer(palette = "Set1") +   
    scale_y_continuous(limits = c(0, 0.3)) + 
    labs(title = "Training Loss vs. Epoch",
         x = "Epoch",
         y = "Training Loss (MSE)",
         colour = "Model") +
    theme_minimal(base_size = 14)
}

make_data_list <- function(country, dir = "history/result") {
  files <- list.files(dir,
                      pattern = paste0("^", country, "_.*_result\\.csv$"),
                      full.names = TRUE)
  
  files |>
    set_names(
      nm = basename(files) |>
        str_remove(paste0("^", country, "_")) |>
        str_remove("_result\\.csv$") |>
        paste0("_result")
    ) |>
    map(read_csv, show_col_types = FALSE)
}

mortality2019_plot <- function(data_list, country_label = "Canada") {
  mortality2019 <- tibble(
    AGE  = 0:100,
    true = data_list$rnn_result  |> filter(year == 2019) |> pull(actual),
    lc   = data_list$lc_result   |> filter(Year == 2019) |> pull(log_mx),
    rnn  = data_list$rnn_result  |> filter(year == 2019) |> pull(pred),
    lstm = data_list$lstm_result |> filter(year == 2019) |> pull(pred),
    cnn  = data_list$cnn_result  |> filter(year == 2019) |> pull(pred),
    trans= data_list$trans_result|> filter(year == 2019) |> pull(pred)
  ) |>
    pivot_longer(-AGE, names_to = "model", values_to = "log_mx")
  
  ggplot(mortality2019, aes(AGE, log_mx, colour = model)) +
    geom_line(linewidth = .6) +
    scale_colour_brewer(palette = "Dark2", name = NULL,
                        breaks = c("true","lc","rnn","lstm","cnn","trans"),
                        labels = c("Truth","Lee-Carter","RNN","LSTM","CNN","Transformer")) +
    labs(title = glue("2019 年 {country_label} 死亡率 log(mₓ) 随年龄变化"),
         x = "Age", y = "log(mₓ)") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

rmse_by_age <- function(data_list) {
  truth_df <- data_list[["rnn_result"]] |> select(age, year, actual)
  model_keys <- list(
    lc   = data_list$lc_result |> filter(Year > 2000) |> mutate(age = Age, year = Year, pred = log_mx),
    rnn  = data_list$rnn_result |> select(age, year, pred),
    lstm = data_list$lstm_result |> select(age, year, pred),
    cnn  = data_list$cnn_result |> select(age, year, pred),
    trans= data_list$trans_result |> select(age, year, pred)
  )
  map2_dfr(model_keys, names(model_keys), ~{
    inner_join(.x, truth_df, by = c("age", "year")) |>
      group_by(age) |>
      summarise(rmse = sqrt(mean((pred - actual)^2)), .groups = "drop") |>
      mutate(model = .y)
  })
}

rmse_age_plot <- function(rmse_long, country_label = "Canada") {
  ggplot(rmse_long, aes(age, rmse, colour = model)) +
    geom_line(linewidth = .6) +
    scale_colour_brewer(palette = "Dark2", name = NULL,
                        breaks = c("lc","rnn","lstm","cnn","trans"),
                        labels = c("Lee-Carter","RNN","LSTM","CNN","Transformer")) +
    labs(title = glue("RMSE 随年龄变化曲线：{country_label}"),
         x = "Age", y = "RMSE") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

rmse_by_year <- function(data_list) {
  truth_df <- data_list[["rnn_result"]] |> select(age, year, actual)
  model_keys <- list(
    lc   = data_list$lc_result |> filter(Year > 2000) |> mutate(age = Age, year = Year, pred = log_mx),
    rnn  = data_list$rnn_result |> select(age, year, pred),
    lstm = data_list$lstm_result |> select(age, year, pred),
    cnn  = data_list$cnn_result |> select(age, year, pred),
    trans= data_list$trans_result |> select(age, year, pred)
  )
  map2_dfr(model_keys, names(model_keys), ~{
    inner_join(.x, truth_df, by = c("age", "year")) |>
      group_by(year) |>
      summarise(rmse = sqrt(mean((pred - actual)^2)), .groups = "drop") |>
      mutate(model = .y)
  })
}

rmse_year_plot <- function(rmse_long, country_label = "Canada") {
  ggplot(rmse_long, aes(year, rmse, colour = model)) +
    geom_line(linewidth = .6) +
    scale_colour_brewer(palette = "Dark2", name = NULL,
                        breaks = c("lc","rnn","lstm","cnn","trans"),
                        labels = c("Lee-Carter","RNN","LSTM","CNN","Transformer")) +
    labs(title = glue("RMSE 随年份变化曲线：{country_label}"),
         x = "Year", y = "RMSE") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

# ---- UI ----

ui <- fluidPage(
  titlePanel("死亡率建模与误差分析"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "选择国家：",
                  choices = c("canada", "denmark", "finland", "france", "italy", "japan", "spain", "uk"),
                  selected = "canada")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("训练损失", plotOutput("lossPlot")),
        tabPanel("2019预测", plotOutput("mortalityPlot")),
        tabPanel("RMSE vs Age", plotOutput("rmseAgePlot")),
        tabPanel("RMSE vs Year", plotOutput("rmseYearPlot"))
      )
    )
  )
)

# ---- Server ----

server <- function(input, output) {
  
  output$lossPlot <- renderPlot({
    file_path <- paste0("history/loss/dl_train_loss_", input$country, ".csv")
    if (file.exists(file_path)) {
      df <- read_csv(file_path, show_col_types = FALSE)
      loss_plot(df) + ggtitle(toupper(input$country))
    } else {
      showNotification("找不到训练损失文件。", type = "error")
    }
  })
  
  output$mortalityPlot <- renderPlot({
    data_list <- make_data_list(input$country)
    mortality2019_plot(data_list, input$country)
  })
  
  output$rmseAgePlot <- renderPlot({
    data_list <- make_data_list(input$country)
    rmse_age_plot(rmse_by_age(data_list), input$country)
  })
  
  output$rmseYearPlot <- renderPlot({
    data_list <- make_data_list(input$country)
    rmse_year_plot(rmse_by_year(data_list), input$country)
  })
}

# ---- Run App ----

shinyApp(ui = ui, server = server)
