No error. :-)

I'm the author of the ggpairs function and maintainer of GGally.  By next week, I'll have a new version up that contains a new function: ggduo.  This function will allow you to produce a pairwise plot matrix for two groups of data.  So, you'll be able to do something like

ggduo(mm, c("Control", "Concept", "Motivation"), c("Read", "Write", "Math", "Science", "Sex"))

I've credited your website in the example section.

ggduo(mm, 3:1, 4:8, mapping = aes(color = Sex), showStrips = FALSE)


tips <- reshape::tips
economics <- ggplot2::economics
swiss <- datasets::swiss

psych <- read.csv("http://www.ats.ucla.edu/stat/data/mmreg.csv")
colnames(psych) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", "Science", "Sex")
psych <- data.frame(
  Motivation = psych$Motivation,
  Self.Concept = psych$Concept,
  Locus.of.Control = psych$Control,
  Read = psych$Read,
  Write = psych$Write,
  Math = psych$Math,
  Science = psych$Science,
  Sex = c("0" = "Male", "1" = "Female")[as.character(psych$Sex)]
)





loess_with_cor <- function(data, mapping, ..., method = "pearson") {
  x <- data[[deparse(mapping$x)]]
  y <- data[[deparse(mapping$y)]]
  cor <- cor(x, y, method = method)
  ggally_smooth_loess(data, mapping, ...) +
    ggplot2::geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 5, fontface = "bold"
    )
}


dt <- mm
dt <- data.frame(
  "Motivation" = mm$Motivation,
  "Self Concept" = mm$Concept,
  "Locus of Control" = mm$Control,
  Read = mm$Read,
  Write = mm$Write,
  Math = mm$Math,
  Science = mm$Science,
  Sex = c("0" = "Male", "1" = "Female")[as.character(mm$Sex)]
)
ggduo(dt, 1:3, 4:8, showStrips = FALSE)
ggduo(dt, 1:3, 4:8, mapping = aes(color = Sex), showStrips = FALSE)
ggduo(dt, 1:3, 4:8, types = list(continuous = loess_with_cor), showStrips = FALSE)


ggduo(
  economics, 1, 2:6,
  columnLabelsX = "date",
  columnLabelsY = c("personal consumption\nexpenditures (B)", "total\npopulation (K)", "personal savings\nrate %", "median duration of\nunemployment (week)", "number of\nunemployed (K)")
) + theme(axis.title.y = element_text(size = 9))



swiss$Residual <- seq_len(nrow(swiss))
 residuals <- lapply(data[2:6], function(x) { 
  summary(lm(Fertility ~ x, data = data))$residuals 
})
  y_range <- range(unlist(residuals))

lm_or_resid <- function(data, mapping, ..., line_color = "red", line_size = 1) {
  if (as.character(mapping$y) != "Residual") {
    return(ggally_smooth_lm(data, mapping, ...))
  }

  resid_data <- data.frame(
    x = data[[as.character(mapping$x)]],
    y = residuals[[as.character(mapping$x)]]
  )

  ggplot(data = data, mapping = mapping) +
    geom_hline(yintercept = 0, color = line_color, size = line_size) +
    ylim(y_range) +
    geom_point(data = resid_data, mapping = aes(x = x, y = y), ...)

}
ggduo(swiss, 2:6, c(1,7), types = list(continuous = lm_or_resid))


pm <- ggduo(swiss, 2:6, c(1,7), types = list(continuous = ggally_smooth_lm))
for (j in 1:5) {
  x_var <- colnames(swiss)[j + 1]
  j_data = data.frame(
    x = swiss[[j + 1]],
    y = residuals[[j]]
  )
  pm[2,j] <- ggplot(data = j_data, mapping = aes(x,y)) +
    ylim(y_range) +
    geom_hline(yintercept = 0, color = "red") +
    geom_point()
}
pm




plotList <- list()
for (letter in strsplit("Questions?", "")[[1]]) {
  plotList[[length(plotList) + 1]] <- ggally_text(letter, size = 25) + scale_x_continuous(breaks = c(0.25,0.5,0.75)) + scale_y_continuous(breaks = c(0.25,0.5,0.75))
}
ggmatrix(plotList, nrow = 2, ncol = 5, showAxisPlotLabels = TRUE)
