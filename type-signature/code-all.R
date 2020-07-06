library(instrumentr)
library(pryr)
library(dplyr)
library(stringr)

call_arguments_data <- tibble(call_exit_id = numeric(0),
                              fun_name = str(0),
                              is_call_user = numeric(0),
                              call_stack_depth = numeric(0),
                              is_user_call_with_pipe_operator = numeric(0),
                              caller = str(0),
                              line_number = str(0),
                              param_index = numeric(0),
                              is_param_missing = numeric(1),
                              name_of_param = str(0),
                              is_evaled = numeric(0),
                              expr = str(0),
                              is_vararg = numeric(0),
                              type_of_argument = str(0),
                              class_of_argument = str(0),
                              address_of_argument = character(0),
                              package_details = str(0))
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

FILTER_BY_CALL_USER <- TRUE

call_exit_callback <- function(context, application, package, func, call) {

  # Look at the model call stack to determine where the function was called from
  call_stack_depth <- get_size(get_call_stack(application))
  is_call_user <- call_stack_depth == 1
  package_details <- capture.output(package)

  # Determine if this call should be measured
  if (is_call_user == FILTER_BY_CALL_USER) {


    # Increment pk of index
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

    # Determine which file and location the original call attached to this call is from
    str <- caller_info("Called from %f at %s#%l\n", level = 2)
    # Determine who the caller was via a sys call
    caller <- paste(deparse(sys.call(which = -2)), collapse = '\n')

    # Determine if the call loopup was done via how the pipe operator does it
    is_user_call_w_pipe <- startsWith(caller, "function_list[[") & endsWith(caller, "]](value)")


    for (parameter in parameters) {


      # Unwrap parameter
      arguments <- get_arguments(parameter)
      # Get metadata about param
      is_param_missing <- is_missing(parameter)
      name_of_param <- get_name(parameter)

      if (is_vararg(parameter)) {

        type_sig <- merge_type(type_sig, index, "...")

        ye <- get_arguments(parameter)

        i <- 1
        for (arg in ye) {

          # Using capture output instead of get_string to get actual data out instead of type and memory addresss
          expr <- capture.output(get_expression(arg))
          is_evaled <- is_evaluated(arg)
          name_of_param <- get_name(arg)
          result <- to_string(get_result(arg))
          arg_class <- paste(class(arg), collapse = '; ')


          call_arguments_data <<- call_arguments_data %>% add_row(
            call_exit_id = call_exit_id,
            fun_name = fun_name,
            param_index = index,
            line_number = str,
            is_user_call_with_pipe_operator = is_user_call_w_pipe,
            caller = caller,
            type_of_argument = typeof(get_expression(arg)),
            class_of_argument = arg_class,
            call_stack_depth = call_stack_depth,
            is_call_user = is_call_user,
            package_details = package_details,
            name_of_param = name_of_param,
            is_evaled = is_evaled,
            expr = expr,
            is_vararg = TRUE,
            is_param_missing = FALSE
          )
          ## Each var arg shall go in as its own unique param index
          #if (length(ye) != i) {
          #  index <- index + 1
          #}
          i <- i + 1
        }

      }
      else if (length(arguments) == 0) {
        type_sig <- merge_type(type_sig, index, "any")


        call_arguments_data <<- call_arguments_data %>% add_row(
          call_exit_id = call_exit_id,
          fun_name = fun_name,
          param_index = index,
          line_number = str,
          is_user_call_with_pipe_operator = is_user_call_w_pipe,
          caller = caller,
          type_of_argument = "No arguments",
          class_of_argument = "No arguments",
          call_stack_depth = call_stack_depth,
          is_call_user = is_call_user,
          package_details = package_details,
          is_param_missing = is_param_missing,
          name_of_param = name_of_param,
          is_vararg = FALSE
        )
      }
        #  AKA promises that were never evaled or symbols, etc...
      else if (!is_evaluated(arguments[[1]])) {

        type_sig <- merge_type(type_sig, index, "any")
        arg_expr <- get_expression(arguments[[1]])

        call_arguments_data <<- call_arguments_data %>% add_row(
          call_exit_id = call_exit_id,
          fun_name = fun_name,
          param_index = index,
          line_number = str,
          is_user_call_with_pipe_operator = is_user_call_w_pipe,
          caller = caller,
          type_of_argument = typeof(arg_expr),
          class_of_argument = paste(class(arg_expr), collapse = '; '),
          address_of_argument = paste("Not evaluated, expression is: ", capture.output(arg_expr)),
          call_stack_depth = call_stack_depth,
          is_call_user = is_call_user,
          package_details = package_details,
          is_param_missing = is_param_missing,
          name_of_param = name_of_param,
          is_evaled = FALSE,
          expr = capture.output(arg_expr),
          is_vararg = FALSE
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
          is_user_call_with_pipe_operator = is_user_call_w_pipe,
          caller = caller,
          type_of_argument = typeof(argument_value),
          class_of_argument = arg_class,
          address_of_argument = address(argument_value),
          call_stack_depth = call_stack_depth,
          is_call_user = is_call_user,
          package_details = package_details,
          is_param_missing = is_param_missing,
          name_of_param = name_of_param,
          is_evaled = TRUE,
          expr = capture.output(get_expression(arguments[[1]])),
          is_vararg = FALSE
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
}

context <- create_context(
  application_load_callback = application_load_callback,
  application_unload_callback = application_unload_callback,
  package_load_callback = package_load_callback,
  package_unload_callback = package_unload_callback,
  function_attach_callback = function_attach_callback,
  function_detach_callback = function_detach_callback,
  call_entry_callback = call_entry_callback,
  call_exit_callback = call_exit_callback,
  packages = c('ggplot2', 'dplyr')
)

trace_code(context = context,
           code = {
             library(dplyr)
             library(ggplot2)

             #Pipe operator
             starwars %>%
               filter(species == "Droid")

             # Pass by parameter in user code
             filter(starwars, species == "Droid", .preserve = True)

             # Call via Thunc
             thunc <- function() {
               filter(starwars, species == "Droid", .preserve = True)
             }

             thunc()

             #Recursive Test Case
             recur <- function(i, df) {
               tmp <- df %>%
                 filter(species == "Droid")

               if (i != 0) {
                 return(recur(i - 1, tmp))
               }
               else {
                 return(tmp)
               }
             }

             recur(6, starwars)

             starwars %>%
               filter(species == "Droid") %>%
               ggplot(aes(x = height, y = birth_year)) + geom_point()

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