##' splom data (johan style)
##'
##' Create underlying data sets to create a Scatter Plot Matrix in ggplot
##' @title Scatter Plot Matrix data sets
##' @param x data frame with the data
##' @param new.names character; labels for variables
##' @param ggcode logical; print example code (ggplot) for creating the splom?
##' @return list with 2 data frames
##' @examples
##' N  <- 100
##' x <- data.frame(foo = rnorm(N, 10, 1))
##' x$bar <- rnorm(N, x$foo-2, 1)
##' x$baz <- rnorm(N, 2*(x$bar-x$foo), 1)
##' x$qux <- rnorm(N, 1, 1)
##' x$quuz <- -x$qux - rnorm(N, 0, 0.1)
##' jsplom(x, new.names = c("The Foo", "The Bar", "The Baz",
##'                         "The Qux", "The Quuz"))
##' @export
jsplom <- function(x, new.names = NULL, ggcode = TRUE){
    m <- ncol(x)
    d <- as.data.table(x)
    if(!is.null(new.names)) setnames(d, old = names(d), new = new.names)
    nm <- names(d)
    R <- NULL
    for(n in nm){
        r <- melt(d, id.vars = n, variable.factor = FALSE)[
          , .(x.term = n, y.term = variable, x = X, y = value),
            env = list(X = n)]
        R <- rbind(R, r)
    }
    mid <- function(z){
        z2 <- z[!is.na(z)]
        if(length(z2) == 0) 0 else (max(z2) + min(z2)) / 2
    }
    RR <- rbind(R, data.table(x.term = nm, y.term = nm, x = NA_real_, y = NA_real_))
    C <- RR[, .(Corr = cor(x, y, method = "spearman")), by = .(x.term, y.term)]
    C[R[, .(x = mid(x)), by = x.term], x := i.x, on = "x.term"]
    C[R[, .(y = mid(y)), by = y.term], y := i.y, on = "y.term"]
    setorder(C, -Corr, na.last = TRUE)
    tmp <- C[1:.N %% 2 == 1][, i := .I][][, c(x.term, y.term), by = i]
    term.order <- tmp[!duplicated(V1), V1]
    C[, x.term := factor(x.term, levels = term.order)]
    C[, y.term := factor(y.term, levels = rev(term.order))]
    setorder(C, x.term, y.term)
    C[, tri := rep(1:0, c(m-.GRP, .GRP)), by = x.term]
    C[, diag := fifelse(x.term == y.term, 1L, 0L)]
    C[, Ctxt := fcase(diag == 1, as.character(x.term),
                     tri == 1, sprintf("%.2f", Corr),
                     default = NA_character_)]
    C[, Color := fcase(diag == 1,
                       "white",
                       tri == 1,
                       colorspace::diverge_hcl(19)[round(-Corr * 9 + 10)],
                       default = NA_character_)]
    if(ggcode){
        cat("
ggplot(OBJECT$scatter, aes(x, y)) +
    geom_point() +
    geom_rect(data = OBJECT$other[!is.na(Color)], aes(fill = Color), xmin = -Inf, ymin = -Inf,
              xmax = +Inf, ymax = +Inf) +
    geom_text(data = OBJECT$other[!is.na(Ctxt)], aes(x, y, label = Ctxt)) +
    scale_fill_identity() +
    facet_grid(y.term ~ x.term, scales = 'free') +
    labs(x = NULL, y = NULL) +
    theme(strip.text.x = element_blank(),
          strip.text.y = element_blank())\n")
    }
    R[, `:=`(x.term = factor(x.term, levels = term.order),
             y.term = factor(y.term, levels = rev(term.order)))]
    setorder(R, x.term, y.term)
    list(scatter = R, other = C)
}
