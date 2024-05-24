#' Convert strings to its numerical value if possible
#'
#' @param df dataframe
#' @return return the dataframe with numerical values instead of strings when possible 
#' @export
convert_to_numeric_safely <- function(data, skip_columns = character(0)) {
  for (col in names(data)) {
    if (!(col %in% skip_columns)) {
      if (!any(is.na(data[[col]]))) {
        data[[col]] <- ifelse(grepl("^\\d+$", data[[col]]) & !is.na(data[[col]]), as.numeric(data[[col]]), data[[col]])
      }
    }
  }
  return(data)
}

convert_numeric_strings <- function(data, skip_columns = NULL) {
  if (is.null(skip_columns)) {
    skip_columns <- character(0)  # Handle missing skip_columns parameter
  }
  for (col in names(data)) {
    if (!(col %in% skip_columns) && 
        all(grepl("^\\d+$", data[[col]]) | data[[col]] == "")) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  return(data)
}
#' Give numerical values and corresponding labels to related variables for aggregation
#'
#' @param df dataframe
#' @return return the dataframe with numerical values and corresponding labels
#' @export
recode_data <- function(data, column_name, label_values) {
  library(dplyr)
  library(tidyr)

  # Define recoding rules using a data frame
  recode_rules <- data.frame(
    original_value = unlist(sapply(label_values, `[[`, "value")),
    new_label = unlist(sapply(label_values, `[[`, "label")),
    stringsAsFactors = FALSE
  )
  data <- data %>%
    mutate(sapply(!!ensym(column_name), function(x) {
      if (is.character(x)) {
        split_values <- strsplit(x, " ")[[1]]
        if (length(split_values) > 0) {
          paste(recode_rules$new_label[recode_rules$original_value %in% split_values], collapse = ", ")
        } else {
          "No observation"
        }
      } else {
        "No observation"
      }
  }))

  # Convert the column to factor with custom labels
  #data[[column_name]] <- factor(data[[column_name]], levels = unlist(sapply(label_values, `[[`, "new_label")))
  # Recode the column
  #data <- data %>%
  #  mutate(!!column_name := case_when(
  #    TRUE ~ recode_rules$new_label[match(!!sym(column_name), recode_rules$original_value)]
  #  )) %>%
  #mutate(!!sym(column_name) := factor(!!sym(column_name), levels = recode_rules$new_label))
  
  return(data)
}
#' Give numerical values and corresponding labels to related variables for aggregation
#'
#' @param df dataframe
#' @return return the dataframe with numerical values and corresponding labels
#' @export
recode_column <- function(column_name, label_values) {
  library(dplyr)
  # Define recoding rules using a data frame
  recode_rules <- data.frame(
    original_value = unlist(sapply(label_values, `[[`, "value")),
    new_label = unlist(sapply(label_values, `[[`, "label")),
    stringsAsFactors = FALSE
  )
  # Recode the column
  data <- sapply(column_name, function(x) {
    case_when(
      TRUE ~ recode_rules$new_label[match(x, recode_rules$original_value)]
  )}) 

  # Convert the column to factor with custom labels
  #data[[column_name]] <- factor(data[[column_name]], levels = unlist(sapply(label_values, `[[`, "new_label")))
  # Recode the column
  #data <- data %>%
  #  mutate(!!column_name := case_when(
  #    TRUE ~ recode_rules$new_label[match(!!sym(column_name), recode_rules$original_value)]
  #  )) %>%
  #mutate(!!sym(column_name) := factor(!!sym(column_name), levels = recode_rules$new_label))
  
  return(data)
}
#' Give numerical values and corresponding labels to related variables for aggregation
#'
#' @param column column with the data
#' @return return the column with numerical values and corresponding labels
#' @export
assign_labels <- function(column, label_values) {
  labeled_column <- sapply(column, function(value) {
    labels <- unlist(lapply(label_values, function(label_value) {
      if (value %in% label_value$value) {
        return(label_value$label)
      } else {
        return(NULL)
      }
    }))
    return(paste(labels, collapse = ", "))
  })
  return(labeled_column)
}

#' Split words in a string
#'
#' @param column
#' @return return the words split
#' @export
split_concatenated_strings <- function(column) {
  # Split each string into individual words
  words <- unlist(strsplit(column, " "))
  return(words)
}

#' Create a basic contengency table out of a dataframe
#'
#' @param df dataframe
#' @return return the dataframe in contingency table format
#' @export
create_basic_table <- function(df,column_to_aggregate, col_name) {
  library(janitor)
  library("data.table")

  ## agregate the values to create contingency table
  #table_contengincy <- df %>%
  #  dplyr::group_by(!!ensym(column_to_aggregate)) %>%
  #  dplyr::summarise(Frequency = dplyr::n()) %>%
  #  tidyr::spread(!!ensym(column_to_aggregate), Frequency, fill = 0)
  ## Sum values across table and create a new column with the values
  #table_contingency <- table_contengincy %>%
  #  dplyr::bind_rows(dplyr::summarise(.,
  #                    across(where(is.numeric), sum),
  #                    across(where(is.character), ~"Total")))
  # Transpose the table so labels of the value are row names
  column_to_aggregate[is.na(column_to_aggregate)] <- 0
  freq_table <- table(column_to_aggregate)
  print(freq_table)
  freq_df <- as.data.frame(freq_table)
  names(freq_df) <- c("Value","Frequency")

  #freq_df$total <- freq_df$Frequency
  #frequency_total <- sum(freq_df$Frequency)
  #row_total <- sum(freq_df$total)
  #totals <- data.frame(Value="Total", Frequency = frequency_total, 
  #        total = row_total)
  
  merged_df <- freq_df %>%
                adorn_totals('row')
                
  merged_df <- merged_df %>%
                adorn_totals('col')
  print(merged_df)
  freq_table <- xtabs(Frequency ~ Value, data = merged_df)
  
  freq_table <- as.matrix(freq_table)
  dimnames(freq_table) <- list("Value" = rownames(freq_table), "Frequency"=colnames(freq_table))
  print(freq_table)
  dim_names <- dimnames(freq_table)
  row_name <- names(dim_names)[1]
  col_names <- names(dim_names)[2]
  print(row_name)
  print(col_names)

  
  #table_contingency <- table(freq_df, dnn=c("Permissions",col_name))

  #result_transposed <- as.data.frame(t(table_contingency))
  # Calculate column sums and add a row for totals
  #totals <- colSums(result_transposed)
  #result_transposed <- rbind(result_transposed, totals)
  # Name last row and column Total
  #Total <- sum
  #data <- addmargins(result_transposed, FUN = Total)
  #colnames(result_transposed)[ncol(result_transposed)] <- "Total"  # Calculate row sums and add a column for totals
  #rownames(result_transposed)[nrow(result_transposed)] <- "Total"  # Calculate row sums and add a column for totals

  # Calculate the percentages of each cell
  #df_percentage <- result_transposed
  #df_percentage <- df_percentage / totals * 100
  #
  #df_display <- result_transposed
  # Format the cell values to have the absolute value and percentage underneath
  #for (i in 1:ncol(df_display)) {
  #  df_display[, i] <- sprintf("%d\n%.2f%%", df_display[, i], df_percentage[, i])
  #}
  return(freq_table)

}

compare_table <- function(df, column1,column2) {
  freq_table1 <- table(column1)
  freq_table2 <- table(column2)

  freq_df1 <- as.data.frame(freq_table1)
  freq_df2 <- as.data.frame(freq_table2)

  names(freq_df1) <- c("Value", "PRE.ALMANACH")
  names(freq_df2) <- c("Value", "POST.ALMANACH")
  merged_df <- merge(freq_df1, freq_df2, by = "Value", all =TRUE)
  merge_df[is.na(merged_df)] <- 0

  merged_df$Total <- merged_df$PRE.ALMANACH + merged_df$POST.ALMANACH

  freq1_total <- sum(merged_df$PRE.ALMANACH)
  freq2_total <- sum(merged_df$POST.ALMANACH)
  row_total <- sum(merged_df$Total)

  totals_row <- data.frame(Value ="Total", PRE.ALMANACH=freq1_total,
      POST.ALMANACH=freq2_total, Total = row_total)
  merged_df <- rbind(merged_df, totals_row)
  return(merged_df)

}
#' Create a basic contengency table out of a column
#'
#' @param column dataframe
#' @return return the column in contingency table format
#' @export
create_basic_table_col <- function(column) {
  value_counts <- table(unlist(column))
  # Convert the frequency table to a data frame
  df_values <- as.data.frame(value_counts, stringsAsFactors = FALSE)
  # Rename the columns
  colnames(df_values) <- c("Pre ALMANACH", "Count")
  # Add a row for the total count
  df_values <- rbind(df_values, c("Total", sum(df_values$Count)))
  # Return the data frame
  return(df_values)
}

create_2way_table <- function(row, column, col_names) {
  # Create the confusion matrix
  data <- table(row, column, dnn=c(col_names[1], col_names[2]))
  #data <- ftable(row, column, dnn = c("Prediction", "Reference"))
  rows = nrow(data)
  cols = ncol(data)
  Total <- sum  
  if (rows > 1 & cols > 1){
    data <- addmargins(data, FUN = Total, quiet = TRUE)
  }
  return(data)
}
#' Create final report
#'
#' @param title Title of the document
#' @param author Author of the document
#' @param output_format Wether it should be a pdf, doc or html
#' @param dataframes List of names of the dataframes that can be used
#' @return return the dataframe in contingency table format
#' @export
create_report <- function(title, author, output_format, dataframes, captions) {
    threshold_cols<- 6
    rmd_content <- sprintf('---\n
title: "%s"\n
author: "%s"\n
output:\n
    %s_document: default\n
', title, author, output_format)
    rmd_content <- ifelse(output_format!='html', paste0(rmd_content, 
    'header-includes:\n
        \\usepackage[table]{xcolor}\n
        \\maxdeadcycles=500\n
        \\usepackage{booktabs}\n
        \\extrafloats{100}\n
---\n
\\clearpage\n'),
    paste0(rmd_content, '---\n
    \\clearpage\n'))

    for (i in seq_along(dataframes)){
        df <- dataframes[[i]]
        rows <- nrow(df)
        last_row_idx <- nrow(df)
        last_col_idx <- ncol(df)

        # Create a unique variable name for each dataframe otherwise knitr will clone the same one
        # ALSO assign it to global environment, otherwise they won't exist by the time of processing
        if (last_col_idx > threshold_cols) {
          latex_options <- c( "scale_down")
        } else {
          latex_options <- c("striped")
        }
        var_name <- paste0("df", i)
        print(df)
        print(last_col_idx)


        caption <- names(dataframes)[i]
        # Add row and column names to the table
        dim_names <- dimnames(df)
        row_name <- names(dim_names)[1]
        col_name <- names(dim_names)[2]
        print(row_name)
        print(col_name)
        #data_with_names <- cbind(Row_Names = row_names, df)
        #print(data_with_names)
        #rownames(data_with_names) <- c("Row Names", row_names)
        #colnames(data_with_names) <- c("Column Names", col_names)
        assign(var_name, df, envir = .GlobalEnv)
        name_caption = captions[i]
        rmd_content <- paste0(rmd_content, sprintf(
            '
            ```{r, echo = FALSE}
            knitr::kable(%s, caption = "%s", format = "%s", booktabs = TRUE) %%>%%
                kableExtra::kable_styling(latex_options = c("%s")) %%>%%
                kableExtra::add_header_above(c("%s" = %s+1)) %%>%%
                kableExtra::pack_rows("%s",1,%s)
            ', var_name, name_caption, ifelse(output_format == 'html', 'html', 'latex'), 
            latex_options, col_name, last_col_idx, row_name, last_row_idx, sep='\n')
        )
  }
    rmd_content <- paste0(
    grep("\\S", strsplit(rmd_content, "\n")[[1]], value = TRUE),collapse = "\n")
    rmd_filename <- paste0(title, "_report.Rmd")
    cat(rmd_content, file = rmd_filename)
    output_filename <- paste0(title, "_report.", output_format)
    rmarkdown::render(input= rmd_filename, output_file = output_filename)
}