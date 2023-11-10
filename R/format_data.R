#' Convert strings to its numerical value if possible
#'
#' @param df dataframe
#' @return return the dataframe with numerical values instead of strings when possible 
#' @export
convert_to_numeric_safely <- function(x) {
  # Try to convert to numeric
  numeric_x <- as.numeric(x)
  
  # Check for NAs
  if (any(is.na(numeric_x))) {
    warning("NAs would be introduced by coercion.")
    return(x)  # Return the original value if conversion fails
  }
  
  return(numeric_x)
}
#' Give numerical values and corresponding labels to related variables for aggregation
#'
#' @param df dataframe
#' @return return the dataframe with numerical values and corresponding labels
#' @export
recode_column <- function(data, column_name, label_values) {
  library(dplyr)
  
  # Define recoding rules using a data frame
  recode_rules <- data.frame(
    original_value = unlist(sapply(label_values, `[[`, "value")),
    new_label = unlist(sapply(label_values, `[[`, "label")),
    stringsAsFactors = FALSE
  )
  
  # Recode the column
  data <- data %>%
    mutate(!!column_name := case_when(
      is.na(!!sym(column_name)) ~ NA,  # Handle NA values
      TRUE ~ recode_rules$new_label[match(!!sym(column_name), recode_rules$original_value)]
    )) %>%
    mutate(!!sym(column_name) := factor(!!sym(column_name), levels = recode_rules$new_label))
  
  return(data)
}
#' Create a basic contengency table out of a dataframe
#'
#' @param df dataframe
#' @return return the dataframe in contingency table format
#' @export
create_basic_table <- function(df, column_to_aggregate, col_name) {
  ## agregate the values to create contingency table
  table_contengincy <- df %>%
    dplyr::group_by(!!ensym(column_to_aggregate)) %>%
    dplyr::summarise(Frequency = dplyr::n()) %>%
    tidyr::spread(!!ensym(column_to_aggregate), Frequency, fill = 0)
  ## Sum values across table and create a new column with the values
  table_contingency <- table_contengincy %>%
    dplyr::bind_rows(dplyr::summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))
  # Transpose the table so labels of the value are row names
  result_transposed <- as.data.frame(t(table_contingency))
  colnames(result_transposed) <- c(col_name)
  print(result_transposed)
  # Calculate column sums and add a row for totals
  totals <- colSums(result_transposed)
  result_transposed <- rbind(result_transposed, totals)
  # Name last row and column Total
  colnames(result_transposed)[ncol(result_transposed)] <- "Total"  # Calculate row sums and add a column for totals
  rownames(result_transposed)[nrow(result_transposed)] <- "Total"  # Calculate row sums and add a column for totals

  # Calculate the percentages of each cell
  df_percentage <- result_transposed
  df_percentage <- df_percentage / totals * 100

  df_display <- result_transposed
  # Format the cell values to have the absolute value and percentage underneath
  for (i in 1:ncol(df_display)) {
    df_display[, i] <- sprintf("%d\n%.2f%%", df_display[, i], df_percentage[, i])
  }
  df_display

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
    rmd_content <- sprintf('---\n
title: "%s"\n
author: "%s"\n
output:\n
    %s_document: default\n
', title, author, output_format)
    rmd_content <- ifelse(output_format!='html', paste0(rmd_content, 
    'header-includes:\n
        \\usepackage{booktabs}\n
---\n
\\clearpage\n'),
    paste0(rmd_content, '---\n
    \\clearpage\n'))

    for (i in seq_along(dataframes)){
        df <- dataframes[[i]]
        # Create a unique variable name for each dataframe otherwise knitr will clone the same one
        # ALSO assign it to global environment, otherwise they won't exist by the time of processing

        var_name <- paste0("df", i)
        caption <- names(dataframes)[i]
        assign(var_name, df, envir = .GlobalEnv)
        name_caption = captions[i]
        rmd_content <- paste0(rmd_content, sprintf(
    '
    ```{r, echo = FALSE}
    knitr::kable(%s, format = "%s", booktabs = TRUE, caption = "%s") %%>%%
        kableExtra::kable_styling()
    ', var_name, ifelse(output_format == 'html', 'html', 'latex'), sep='\n'), name_caption
    )
} 

    rmd_content <- paste0(
    grep("\\S", strsplit(rmd_content, "\n")[[1]], value = TRUE),collapse = "\n")
    rmd_filename <- paste0(title, "_report.Rmd")
    cat(rmd_content, file = rmd_filename)
    output_filename <- paste0(title, "_report.", output_format)
    rmarkdown::render(input= rmd_filename, output_file = output_filename)
}