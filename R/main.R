#' evalParse
#'
#' Run a string of characters
#'
#' @export
evalParse = function(texts_, env_) {unlist(lapply(texts_, \(text__) eval(parse(text = text__), envir = env_)))}

#' knitBook
#'
#' knit bookdown site
#'
#' @export
knitBook = function() {bookdown::render_book("index.Rmd", output_format="bookdown::gitbook")}

#' capitalize
#'
#' Uppercase the first letter of each word in a sentence.
#'
#' @export
capitalize = function(sentence_) {
  gsub("\\b([a-z])", "\\U\\1", sentence_, perl = TRUE)
}

#' evalPlaceholder
#'
#' Eval where '&&' exists.
#'
#' @importFrom tibble tibble
#' @importFrom stringr str_extract str_replace
#'
#' @param path2raw_ path to raw file
#'
#' @export
evalPlaceholder = function(path2raw_, env_ = .GlobalEnv) {

  df_convert_ = tibble(
    raw = readLines(path2raw_),
    source = str_extract(raw, "&.*?&"),
    toEval = str_extract(raw, "(?<=&).*?(?=&)"),
    UMI = ifelse(is.na(source), NA, evalParse(toEval, env_)),
    lines = ifelse(is.na(source), NA, 1:length(source))
  )

  file_new_ = c()
  for (i_ in seq_along(df_convert_$raw)) {

    file_new_[[i_]] = df_convert_$raw[[i_]]

    if (i_ %in% df_convert_$lines) {

      file_new_[[i_]] = str_replace(file_new_[[i_]], "&.*?&", df_convert_$UMI[[i_]])

    }

  }
  file_new_ = unlist(file_new_)

  return(file_new_)

}

#' newDiary
#'
#' Copy the diary template and execute the marked section.
#'
#' @export
newDiary = function(filename_ = paste0(format(Sys.Date(), "%Y%m%d"), '.Rmd'), template_ = '~/diary.Rmd') {

  if (!file.exists(filename_)) {

    file_new_ = evalPlaceholder(template_)

    writeLines(file_new_, filename_)
    file.edit(filename_)

  } else {

    message('The diary already exists, open the existing one')
    file.edit(filename_)

  }

}

#' editToday
#'
#' Edit today's diary if it exists
#'
#' @export
editToday = function() {

  if (file.exists(paste0(format(Sys.Date(), "%Y%m%d"), '.Rmd'))) {
    file.edit(paste0(format(Sys.Date(), "%Y%m%d"), '.Rmd'))
  } else {
    message('You haven\'t started your writing today')
    newDiary()
  }

}

#' newMonth
#'
#' Copy the month template and execute the marked section.
#'
#' @export
newMonth = function(filename_ = paste0(format(Sys.Date(), "%Y%m"), '.Rmd'), template_ = '~/month.Rmd') {

  if (!file.exists(filename_)) {

    file_new_ = evalPlaceholder(template_)

    writeLines(file_new_, filename_)
    file.edit(filename_)

  } else {

    message('The month mission already exists, open the existing one')
    file.edit(filename_)

  }

}

#' editMonth
#'
#' Edit this month's diary if it exists
#'
#' @export
editMonth = function() {

  if (file.exists(paste0(format(Sys.Date(), "%Y%m"), '.Rmd'))) {
    file.edit(paste0(format(Sys.Date(), "%Y%m"), '.Rmd'))
  } else {
    message('You haven\'t started your writing today')
    newMonth()
  }

}

#' findFile
#'
#' Find the files containing the keywords and print out their paths.
#'
#' @param keyWord_
#' @param path_
#'
#' @importFrom purrr map walk
#' @importFrom cli cli_text
#'
#' @export
findFile = function(keyWord_, path_ = ".") {

  files_ = list.files(path_, pattern = ".R$", recursive = T, full.names = T)

  where_ = files_ |>
    map(~ paste0(readLines(.x), collapse = "")) |>
    map(~ grepl(pattern = keyWord_, .x)) |>
    unlist() |>
    which() |>
    suppressWarnings()

  files_ = files_[where_]
  walk(files_, ~ cli_text("{.href [{.x}](file://{.x})}."))

}
