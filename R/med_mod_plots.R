.coef_offset_3 <- tibble::tribble(
  ~obj, ~h_off, ~v_off,
    #Top 1
    "a", +1, -0.3,
    "aa", -1.2, -0.3,
    #Top 2
    "c", 0.5, -0.2,
    "cc", -0.5,-0.2,
    #Bottom 1
    "b", 0.4, -0.15,
    "bb", -0.7, -0.15,

  )




plot_mediation <- function(IV, DV, Ms, df, digits = 2, coef_offset = length(Ms), filename = NULL) {

  .check_req_packages(c("glue", "DiagrammeR"))


  stylec <- ifelse(df$pvalue[df$type=="direct"] < .05, "solid", "dashed")

  determine_positions <- function(num_Ms) {

     pos <- tibble::tribble(
      ~obj, ~h, ~v,
      NA_character_, NA_real_, NA_real_) %>% .[-1,]

    for (i in 1:ceiling(num_Ms/2)) {
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2*i-1), v=i, h=2.5))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = letters[2*i-1], h=0.6, v=0.6+i-1))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0(letters[2*i-1], letters[2*i-1]), h=4.6, v=0.6+i-1))

      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2*i), v=-i, h=2.5))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = letters[2*i], h=0.6, v=-0.6-i+1))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0(letters[2*i], letters[2*i]), h=4.6, v=-0.6-i+1))
    }
   pos

  }


  pos <- determine_positions(length(Ms))
  pos %<>% dplyr::bind_rows(tibble::tibble(obj = "note", h=0.5, v=-0.1))


  df$type[df$type=="indirect"] <- paste0("M", 1:length(Ms))

  pos <- df %>% dplyr::mutate(ci = .fmt_ci(.data$ci.lower, .data$ci.upper, digits), est = paste0(round(.data$est.std, digits), sigstars(.data$pvalue))) %>% dplyr::select(obj = .data$type, .data$est, .data$ci) %>% dplyr::full_join(pos, by = "obj")

  pos$est[pos$obj == "note"] <- paste("<i>Direct effect:</i> ", pos$est[pos$obj == "direct"], pos$ci[pos$obj == "direct"], "<br />", "<i>Total effect: </i>",  pos$est[pos$obj == "total"], pos$ci[pos$obj == "total"] )

  pos %<>% .[!is.na(pos$est),]


if (!is.null(coef_offset)) {
  if (!coef_offset == 3 || (tibble::is_tibble(coef_offset) && nrow(coef_offset) == length(Ms) * 2)) {
    warning("Valid coef_offset tibble is not provided and automatic alignment of coefficients is
                                                                                                        not yet implemented for this number of mediators - you will likely need to either provide a valid
                                                                                                        coef_offset tibble or edit the returned grViz code manually")
    coef_offset <- NULL
  } else if (coef_offset == 3) {
    coef_offset <- .coef_offset_3
  }
}


  if (!is.null(coef_offset)) {
    for (i in seq_len(nrow(coef_offset))) {
      pos$h[pos$obj == coef_offset[i,]$obj] <-  pos$h[pos$obj == coef_offset[i,]$obj] + coef_offset[i,]$h_off
      pos$v[pos$obj == coef_offset[i,]$obj] <-  pos$v[pos$obj == coef_offset[i,]$obj] + coef_offset[i,]$v_off
    }
  }



    code <- glue::glue("digraph  {{

            graph [layout = 'neato',
            outputorder = 'edgesfirst',
            bgcolor = 'white', rankdir=LR,]

            node [fontname = 'Helvetica',
            fontsize = '10',
            shape = 'circle',
            fixedsize = 'true',
            width = '0.5',
            style = 'filled',
            fillcolor = 'white',
            color = 'black',
            fontcolor = 'black']



            'x' [label = <{IV}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '0,0!']
            'y' [label = <{DV}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '5,0!']
                 {purrr::map2(Ms, paste0('M', 1:length(Ms)), function(x,y) paste0('\\'', y, '\\' [label = <', x, '<br />', pos$est[pos$obj == y], '>,
                 color = \\'black\\', shape = \\'rectangle\\', height = \\'0.5\\', width = \\'1.5\\',
                 pos = \\'', pos$h[pos$obj == y], ',',  pos$v[pos$obj == y], '!\\']')) %>% glue::glue_collapse(sep = '\\n')}

                 {pos[match('a', pos$obj):(length(pos$obj)-1),] %>% purrr::pmap_chr(function(...) {
                 current <- tibble::tibble(...)
                 paste0('\\'', current$obj, '\\' [label = <', current$est, '<br />', current$ci, '>,
                 color = \\'black\\', shape = \\'plaintext\\',
                 pos = \\'', current$h, ',',  current$v, '!\\']')}) %>% glue::glue_collapse(sep = '\\n')}

            edge [fontname = 'Helvetica',
            fontsize = '10',
            len = '1.5',
            color = 'black',
            arrowsize = '0.5']

            {paste0('x->', paste0('M', 1:length(Ms)), collapse = '\\n')}
            {paste0(paste0('M', 1:length(Ms)), '->y', collapse = '\\n')}

            x->y  [style = {stylec}, headlabel= <{pos$est[pos$obj == 'note']}> labeldistance=13, labelangle=7]

             }}")

graph <- code %>% DiagrammeR::grViz()

if (!is.null(filename)) {
  if (suppressWarnings(!all(lapply(c("DiagrammeRsvg", "rsvg"), requireNamespace, quietly = TRUE)))) {
    warning("To save the diagramme, you need the following packages: DiagrammeRsvg & rsvg. File not saved.")
  } else {
    ext <- stringr::str_sub(filename, -3)
    if (!(ext %in% c("pdf", "svg", "png"))) {
      warning("File extension should be pdf, png or svg. Defaults to png now, but you might need to rename the file to open it")
      ext <- "svg"
    }
    if (ext == "pdf") {
      graph %>%
        DiagrammeRsvg::export_svg() %>%
        charToRaw() %>%
        rsvg::rsvg_pdf(filename)
    } else if (ext == "svg") {
      graph %>%
        DiagrammeRsvg::export_svg() %>%
        charToRaw() %>%
        rsvg::rsvg_svg(filename)
    } else if (ext == "png") {
      graph %>%
        DiagrammeRsvg::export_svg() %>%
        charToRaw() %>%
        rsvg::rsvg_png(filename)
    }
  }
}
      out <- .named_list(code, graph)



}

.null_transformer <- function(str = "") {
  function(text, envir) {
    out <- glue::identity_transformer(text, envir)
    if (is.null(out)) {
      return(str)
    }

    out
  }
}

moderated_mediation <- function(X, M, W, Y, mod_direct_path = TRUE, labels = list(a="+", b="+", c="+", a_mod = "+", c_mod = "+"), filename = NULL) {



    .check_req_packages(c("glue", "DiagrammeR"))

  #Set parameters

  a <- labels$a
  b <- labels$b
  c <- labels$c
  a_mod <- labels$a_mod
  c_mod <- labels$c_mod


code <-  glue::glue(.transformer = .null_transformer(), "digraph {{

        graph [layout = 'neato',
        outputorder = 'edgesfirst',
        bgcolor = 'white', rankdir=LR,]

        node [fontname = 'Helvetica',
        fontsize = '10',
        shape = 'circle',
        fixedsize = 'true',
        width = '0.5',
        style = 'filled',
        fillcolor = 'white',
        color = 'black',
        fontcolor = 'black']



        'X' [label = <{X}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '0,0!']
        'Y' [label = <{Y}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '5,0!']
        'M' [label = <{M}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '2.5,1!']
        'a' [label = <{a}>  color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '0.75,0.5!']
        'amod' [label = <{a_mod}>  color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '0.9,0.8!']
        'b' [label = <{b}>, color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '3.8,0.65!']
        'c' [label = <{c}>, color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '2.45,-0.15!']
        'W' [label = <{W}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.2', pos = '0.75,1.4!']
        'MW' [style = invis, pos = '1.3,0.5!', height = '0', width = '0']
        {if(mod_direct_path) '\\'XW\\' [style = invis, pos = \\'2,0!\\', height = \\'0\\', width = \\'0\\']'}
        {if(mod_direct_path) glue::glue('\\'cmod\\' [label = <{c_mod}>, color = \\'black\\', shape = \\'plaintext\\', fillcolor=\\'transparent\\', pos = \\'1.9,0.4!\\']')}

        edge [fontname = 'Helvetica',
        fontsize = '10',
        len = '1.5',
        color = 'black',
        arrowsize = '0.5']

        X->MW  [arrowhead=none]
        MW -> M
        M->Y
        W->MW
        {ifelse(mod_direct_path, '    X->XW [arrowhead=none] \n    XW -> Y \n    W->XW', '    X->Y')}
}



        }}")

browser()

  graph <- code %>% DiagrammeR::grViz()

  if (!is.null(filename)) {
    if (suppressWarnings(!all(lapply(c("DiagrammeRsvg", "rsvg"), requireNamespace, quietly = TRUE)))) {
      warning("To save the diagramme, you need the following packages: DiagrammeRsvg & rsvg. File not saved.")
    } else {
      ext <- stringr::str_sub(filename, -3)
      if (!(ext %in% c("pdf", "svg", "png"))) {
        warning("File extension should be pdf, png or svg. Defaults to png now, but you might need to rename the file to open it")
        ext <- "svg"
      }
      if (ext == "pdf") {
        graph %>%
          DiagrammeRsvg::export_svg() %>%
          charToRaw() %>%
          rsvg::rsvg_pdf(filename)
      } else if (ext == "svg") {
        graph %>%
          DiagrammeRsvg::export_svg() %>%
          charToRaw() %>%
          rsvg::rsvg_svg(filename)
      } else if (ext == "png") {
        graph %>%
          DiagrammeRsvg::export_svg() %>%
          charToRaw() %>%
          rsvg::rsvg_png(filename)
      }
    }
  }
  out <- .named_list(code, graph)




}


.named_list <- function(...) {
  out <- list(...)
  names(out) <- match.call() %>% as.list() %>% .[-1]
  out
}
