library(instrumentr)
library(pryr)
library(dplyr)
library(stringr)

call_arguments_data <- tibble(call_exit_id = numeric(0), fun_name = str(0), package_details=str(0), is_call_user = numeric(0), call_stack_depth = numeric(0), is_user_call_with_pipe_operator = numeric(0), param_index = numeric(0), caller = str(0), line_number = str(0), type_of_argument = str(0), class_of_argument = str(0), address_of_argument = character(0))
call_exit_data <- tibble(call_exit_id = numeric(0), fun_name = str(0), type_of_output = str(0), class_of_output = str(0), address_of_output = character(0))

functions <- get_package_function_names("dplyr", public = TRUE, private = FALSE)

application_load_callback <- function(context, application) {
  set_data(context, new.env(parent = emptyenv()))
}

caller_info <- function(fmtstring = NULL, level = 1) # https://stackoverflow.com/q/59537482/684229
  # and https://stackoverflow.com/a/59544073/13221681
  {
  x <- .traceback(x = level + 1)

  i <- 1
  repeat { # loop for subexpressions case; find the first one with source reference
    srcref <- getSrcref(x[[i]])
    if (is.null(srcref)) {
      if (i < length(x)) {
        i <- i + 1
        next;
      } else {
        warning("caller_info(): not found\n")
        return(NULL)
      }
    }
    srcloc <- list(fun = getSrcref(x[[i + 1]]), file = getSrcFilename(x[[i]]), line = getSrcLocation(x[[i]]))
    break;
  }

  if (is.null(fmtstring))
    return(srcloc)

  fmtstring <- sub("%f", paste0(srcloc$fun, collapse = ""), fmtstring)
  fmtstring <- sub("%s", srcloc$file, fmtstring)
  fmtstring <- sub("%l", srcloc$line, fmtstring)
  fmtstring
}

application_unload_callback <- function(context, application) {
  data <- get_data(context)

  for (name in ls(data)) {
    cat(name, ":: ")

    type_sig <- get(name, envir = data)
    l <- length(type_sig)

    index <- 1

    cat("<")
    while (index < l) {
      if (index != 1) {
        cat(", ")
      }
      arg_type <- type_sig[[index]]
      cat(paste(arg_type, collapse = "|"))
      index <- index + 1
    }
    cat("> => ")

    cat(paste(type_sig[[index]], collapse = "|"))

    cat("\n")
  }
}
package_load_callback <- function(context, application, package) { }
package_unload_callback <- function(context, application, package) { }
function_attach_callback <- function(context, application, package, func) { }
function_detach_callback <- function(context, application, package, func) { }


call_entry_callback <- function(context, application, package, func, call) { }

create_type_sig <- function(n) {
  type_sig <- list()
  index <- 1
  while (index <= n) {
    type_sig[[index]] <- character(0)
    index <- index + 1
  }
  type_sig
}

infer_type <- function(val) {
  class(val)
}

merge_type <- function(type_sig, index, type) {
  types <- type_sig[[index]]
  type_sig[[index]] <- unique(c(types, type))
  type_sig
}

call_exit_id <- 0;

call_exit_callback <- function(context, application, package, func, call) {
  call_exit_id <<- call_exit_id + 1

  fun_name <- get_name(func)

  parameters <- get_parameters(call)

  data <- get_data(context)

  if (exists(fun_name, envir = data)) {
    type_sig <- get(fun_name, envir = data)
  }
  else {
    type_sig <- create_type_sig(length(parameters) + 1)
  }

  index <- 1

  str <- caller_info("Called from %f at %s#%l\n", level = 2)
  caller <- paste(deparse(sys.call(which = -2)), collapse = '\n')

  # Look at the model call stack to determine where the function was called from
  call_stack <- capture.output(get_call_stack(application))
  call_stack_depth <- as.numeric(str_sub(c(call_stack[1]), 11, -9))
  is_call_user <- call_stack_depth == 1
  package_details <- capture.output(package)

  for (parameter in parameters) {

    arguments <- get_arguments(parameter)

    if (is_vararg(parameter)) {
      type_sig <- merge_type(type_sig, index, "...")

      call_arguments_data <<- call_arguments_data %>% add_row(
        call_exit_id = call_exit_id,
        fun_name = fun_name,
        param_index = index,
        line_number = str,
        is_user_call_with_pipe_operator = startsWith(caller, "function_list[[") & endsWith(caller, "]](value)"),
        caller = caller,
        type_of_argument = "vararg",
        class_of_argument = "vararg",
        call_stack_depth = call_stack_depth,
        is_call_user = is_call_user,
        package_details = package_details
      )
    }
    else if (length(arguments) == 0) {
      type_sig <- merge_type(type_sig, index, "any")

      call_arguments_data <<- call_arguments_data %>% add_row(
        call_exit_id = call_exit_id,
        fun_name = fun_name,
        param_index = index,
        line_number = str,
        is_user_call_with_pipe_operator = startsWith(caller, "function_list[[") & endsWith(caller, "]](value)"),
        caller = caller,
        type_of_argument = "No arguments",
        class_of_argument = "No arguments",
        call_stack_depth = call_stack_depth,
        is_call_user = is_call_user,
        package_details = package_details
      )
    }
    else if (!is_evaluated(arguments[[1]])) {
      type_sig <- merge_type(type_sig, index, "any")

      call_arguments_data <<- call_arguments_data %>% add_row(
        call_exit_id = call_exit_id,
        fun_name = fun_name,
        param_index = index,
        line_number = str,
        is_user_call_with_pipe_operator = startsWith(caller, "function_list[[") & endsWith(caller, "]](value)"),
        caller = caller,
        type_of_argument = typeof(get_expression(arguments[[1]])),
        class_of_argument = paste(class(get_expression(arguments[[1]])), collapse = '; '),
        address_of_argument = paste("Not evaluated, expression is: ", capture.output(get_expression(arguments[[1]]))),
        call_stack_depth = call_stack_depth,
        is_call_user = is_call_user,
        package_details = package_details
      )
    }
    else {
      argument_value <- get_result(arguments[[1]])
      type_sig <- merge_type(type_sig, index, infer_type(argument_value))

      arg_class <- paste(class(argument_value), collapse = '; ')

      call_arguments_data <<- call_arguments_data %>% add_row(
        call_exit_id = call_exit_id,
        fun_name = fun_name,
        param_index = index,
        line_number = str,
        is_user_call_with_pipe_operator = startsWith(caller, "function_list[[") & endsWith(caller, "]](value)"),
        caller = caller,
        type_of_argument = typeof(argument_value),
        class_of_argument = arg_class,
        address_of_argument = address(argument_value),
        call_stack_depth = call_stack_depth,
        is_call_user = is_call_user,
        package_details = package_details
      )
    }

    index <- index + 1
  }


  result_type <- "any"

  if (is_successful(call)) {
    result <- get_result(call)
    result_type <- infer_type(result)
  }
  call_exit_data <<- call_exit_data %>% add_row(
    call_exit_id = call_exit_id,
    fun_name = fun_name,
    type_of_output = typeof(result),
    class_of_output = paste(class(result), collapse = '; '),
    address_of_output = address(result))

  type_sig <- merge_type(type_sig, index, result_type)

  assign(fun_name, type_sig, envir = data)
}

context <- create_context(application_load_callback = application_load_callback,
                          application_unload_callback = application_unload_callback,
                          package_load_callback = package_load_callback,
                          package_unload_callback = package_unload_callback,
                          function_attach_callback = function_attach_callback,
                          function_detach_callback = function_detach_callback,
                          call_entry_callback = call_entry_callback,
                          call_exit_callback = call_exit_callback,
                          functions = functions)

trace_code(context = context,
           code = {
             library(dplyr)

             starwars %>%
               filter(species == "Droid")

             starwars %>%
               select(name, ends_with("color"))

             starwars %>%
               mutate(name, bmi = mass / ((height / 100)^2)) %>%
               select(name:mass, bmi)

             starwars %>%
               arrange(desc(mass))

             starwars %>%
               group_by(species) %>%
               summarise(
                 n = n(),
                 mass = mean(mass, na.rm = TRUE)
               ) %>%
               filter(n > 1)

             starwars %>%
               summarise(across(where(is.character), ~length(unique(.x))))

             starwars %>%
               group_by(species) %>%
               filter(n() > 1) %>%
               summarise(across(c(sex, gender, homeworld), ~length(unique(.x))))

             starwars %>%
               group_by(homeworld) %>%
               filter(n() > 1) %>%
               summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))


             df <- data.frame(g = c(1, 1, 2), x = c(-1, 1, 3), y = c(-1, -4, -9))
             df %>%
               group_by(g) %>%
               summarise(across(where(is.numeric), sum))


             min_max <- list(
               min = ~min(.x, na.rm = TRUE),
               max = ~max(.x, na.rm = TRUE)
             )

             starwars %>% summarise(across(where(is.numeric), min_max))


             starwars %>% summarise(across(where(is.numeric), min_max, .names = "{fn}.{col}"))


             starwars %>% summarise(
               across(where(is.numeric), ~min(.x, na.rm = TRUE), .names = "min_{col}"),
               across(where(is.numeric), ~max(.x, na.rm = TRUE), .names = "max_{col}")
             )

             df <- tibble(x = 1:3, y = 3:5, z = 5:7)
             mult <- list(x = 1, y = 10, z = 100)
             df %>% mutate(across(all_of(names(mult)), ~.x * mult[[cur_column()]]))

             df <- data.frame(x = c(1, 2, 3), y = c(1, 4, 9))
             df %>%
               summarise(n = n(), across(where(is.numeric), sd))

             df %>%
               summarise(across(where(is.numeric), sd), n = n())


             df %>%
               summarise(n = n(), across(where(is.numeric) & !n, sd))

             rescale01 <- function(x) {
               rng <- range(x, na.rm = TRUE)
               (x - rng[1]) / (rng[2] - rng[1])
             }

             df <- tibble(x = 1:4, y = rnorm(4))

             df %>% mutate(across(where(is.numeric), rescale01))

             starwars %>% filter(across(everything(), ~!is.na(.x)))


             starwars %>% distinct(across(contains("color")))

             starwars %>% count(across(contains("color")), sort = TRUE)

             df %>% mutate_if(is.numeric, mean, na.rm = TRUE)
             # ->
             df %>% mutate(across(where(is.numeric), mean, na.rm = TRUE))
             df %>% mutate_at(vars(c(x, starts_with("y"))), mean)
             # ->

             df %>% mutate(across(c(x, starts_with("y")), mean, na.rm = TRUE))
             with_tracing_disabled({
                                     df %>% mutate_all(mean)
                                   })
             # ->
             df %>% mutate(across(everything(), mean))

             df <- tibble(x = c("a", "b"), y = c(1, 1), z = c(-1, 1))
             df %>% filter(across(where(is.numeric), ~.x > 0))

             rowAny <- function(x) rowSums(x) > 0

             df %>% filter(rowAny(across(where(is.numeric), ~.x > 0)))

             df <- tibble(x = 2, y = 4, z = 8)
             with_tracing_disabled({
                                     with_tracing_enabled({
                                                            df %>% mutate_all(~.x / y)
                                                          })
                                   })

             df %>% mutate(across(everything(), ~.x / y))
           })

call_arguments_data %>% write.csv(., "call_arguments_data.csv")
call_exit_data %>% write.csv(., "call_exit_data.csv")