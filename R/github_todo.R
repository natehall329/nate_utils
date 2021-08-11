
#' Update running TODO.md for display on github using \code{todor}
#'
#' @description Pull all relevant todoR fields and render a TODO.md file that is viewable on github.
#'
#' @importFrom todor todor
#' @importFrom rprojroot find_package_root_file
#' @import tidyverse
#'
#' @export
github_todo <- function(...){
  todo <- todor::todor(output = "markdown")

  x <- todor::todor()

  items <- str_split(todo, c("\n\n", "\n"))[[length(str_split(todo, c("\n\n", "\n")))]] %>% data.frame(str = .) %>%
    filter(str != "") %>% mutate(type = ifelse(grepl("- [", str, fixed = TRUE), "item", "location")) %>%
    group_by(group = cumsum(type == "c")) %>%
    mutate(id = cumsum(type != "item"),
           rn = row_number()) %>% ungroup() %>% select(-group) %>%
    pivot_wider(names_from = type, values_from = str) %>% fill(location) %>% na.omit() %>%
    mutate(item_type = str_extract(str_extract(item, "- \\[ [[:upper:]]+ ]"), "[[:upper:]]+"),
           string = sub("- \\[ [[:upper:]]+ ]  ", "", item)) %>% select(-item, -id, -rn)

  # TODO add functionality that allows for completed todo items to be marked with DONE.
  # TODO add functionality to pull an already initialized todo list and append new items.
  # TODO Use forked todor repo and modify code to pull information on which line the todo occurs.

  md_text <- c()
  for(i in unique(items$item_type)){
    it <- items %>% filter(item_type == i)
    md_text <- c(md_text, paste0("# ", i))
    for(j in 1:nrow(it)){
      # j <- 1
      this.item <- it[j,]
      # if more than one todo item in a script will print to same row
      if(grepl("\\\n", this.item$string)){
        # split_location <-
        str_split(this.item$string, "\n")

      }


      md_checkbox <- paste0("- [ ] ", this.item$string, " (", sub("\\*\\* ", "",sub("\\*\\*", "", this.item$location)) ,")")
      md_text <- c(md_text, md_checkbox)
    }
  }

  writeLines(md_text, file.path(rprojroot::find_package_root_file(), "TODO.md"))
}
