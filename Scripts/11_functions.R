# PLOT FOR CATEGORICAL VARIABLES - IMPUTED VS OBSERVED --------------------

# Credit: https://gist.github.com/NErler/0d00375da460dd33839b98faeee2fdab

# Compare proportions in categorical variables between observed and imputed data
# visually (using ggplot)
#
# Parameters:
# x: mids object (from mice)
# formula: formula describing which variables to plot
# facet: either "wrap" for facet_wrap or "grid" for facet_grid
# ...: additional parameters passed to theme()
#
# Note: if the formula is not specified, all imputed categorical variables are
# plotted. 
# 
# A formula has the structure:
# categorical variables ~ faceting variables | color variable
# 
# By default, .imp (imputation set identifier) will be used as color variable.
#
# This function uses the following packages:
# - mice
# - reshape2
# - RColorBrewer
# - ggplot2

propplot <- function(x, formula, facet = "wrap", ...) {
  library(ggplot2)
  
  cd <- data.frame(mice::complete(x, "long", include = TRUE))
  cd$.imp <- factor(cd$.imp)
  
  r <- as.data.frame(is.na(x$data))
  
  impcat <- x$meth != "" & sapply(x$data, is.factor)
  vnames <- names(impcat)[impcat]
  
  if (missing(formula)) {
    formula <- as.formula(paste(paste(vnames, collapse = "+",
                                      sep = ""), "~1", sep = ""))
  }
  
  tmsx <- terms(formula[-3], data = x$data)
  xnames <- attr(tmsx, "term.labels")
  xnames <- xnames[xnames %in% vnames]
  
  if (paste(formula[3]) != "1") {
    wvars <- gsub("[[:space:]]*\\|[[:print:]]*", "", paste(formula)[3])
    wvars <- attr(terms(as.formula(paste("~", wvars))), "term.labels")
    if (grepl("\\|", formula[3])) {
      svars <- gsub("[[:print:]]*\\|[[:space:]]*", "", paste(formula)[3])
      svars <- all.vars(as.formula(paste("~", svars)))
    } else {
      svars <- ".imp"
    }
  } else {
    wvars <- NULL
    svars <- ".imp"
  }
  
  for (i in seq_along(xnames)) {
    xvar <- xnames[i]
    select <- cd$.imp != 0 & !r[, xvar]
    cd[select, xvar] <- NA
  }
  
  
  for (i in which(!wvars %in% names(cd))) {
    cd[, wvars[i]] <- with(cd, eval(parse(text = wvars[i])))
  }
  
  meltDF <- reshape2::melt(cd[, c(wvars, svars, xnames)], id.vars = c(wvars, svars))
  meltDF <- meltDF[!is.na(meltDF$value), ]
  
  
  wvars <- if (!is.null(wvars)) paste0("`", wvars, "`")
  
  a <- plyr::ddply(meltDF, c(wvars, svars, "variable", "value"), plyr::summarize,
                   count = length(value))
  b <- plyr::ddply(meltDF, c(wvars, svars, "variable"), plyr::summarize,
                   tot = length(value))
  mdf <- merge(a,b)
  mdf$prop <- mdf$count / mdf$tot
  
  plotDF <- merge(unique(meltDF), mdf)
  plotDF$value <- factor(plotDF$value,
                         levels = unique(unlist(lapply(x$data[, xnames], levels))),
                         ordered = T)
  
  p <- ggplot(plotDF, aes(x = value, fill = get(svars), y = prop)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(legend.position = "bottom", ...) +
    ylab("proportion") +
    scale_fill_manual(name = "",
                      values = c("black",
                                 colorRampPalette(
                                   RColorBrewer::brewer.pal(9, "Blues"))(x$m + 3)[1:x$m + 3])) +
    guides(fill = guide_legend(nrow = 1))
  
  if (facet == "wrap")
    if (length(xnames) > 1) {
      print(p + facet_wrap(c("variable", wvars), scales = "free"))
    } else {
      if (is.null(wvars)) {
        print(p)
      } else {
        print(p + facet_wrap(wvars, scales = "free"))
      }
    }
  
  if (facet == "grid")
    if (!is.null(wvars)) {
      print(p + facet_grid(paste(paste(wvars, collapse = "+"), "~ variable"),
                           scales = "free"))
    }
}


# CHECK MICE SETTINGS -----------------------------------------------------

check_mice_settings <- function(specified_vector, mice_vector, sheet_name) {
  # Checking that mice() did not alter the imputation methods, predictor matrix,
  # or visit sequence that were specified.
  # Writes a xlsx file in "../Output/Imputation/Imputation-check-settings*"
  
  if (grepl("predictorMatrix", deparse(substitute(mice_vector)))) {
    y <- data.frame(
      identical_pred_matrix = as.character(identical(specified_vector, mice_vector))
    )
  } else {
    y <- data.frame(specified_vector, mice_vector)
    y$same <- y$specified_vector == y$mice_vector
    y$all_same <- as.character(all(y$same))
    y$same <- as.character(y$same)
  }
  
  writexl::write_xlsx(
    y, 
    paste0(
      "../Output/Imputation/Imputation-check-settings-", 
      deparse(substitute(specified_vector)), 
      "/", sheetname, ".xlsx")
    )

  invisible(y)
}
