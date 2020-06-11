###############################################################################
## PACKAGE, FUNCTION and CALL CALLBACKS
###############################################################################

library(instrumentr)

functions <- get_package_function_names("dplyr", public = TRUE, private = FALSE)

application_load_callback <- function(context, application) {
    cat("** Application Load **\n")
    print(context)
    print(application)

    set_data(context, "tracing-state")
}

application_unload_callback <- function(context, application) {
    cat("** Application Unload **\n")
    print(context)
    print(application)

    print(get_data(context))
}

package_load_callback <- function(context, application, package) {
    cat("** Package Load **\n")
    print(package)
}

package_unload_callback <- function(context, application, package) {
    cat("** Package Unload **\n")
    print(package)
}

function_attach_callback <- function(context, application, package, func) {
    #cat("** Function Attach **\n")
    #print(func)
}

function_detach_callback <- function(context, application, package, func) {
    ##cat("** Function Detach **\n")
    ##print(func)
}

call_entry_callback <- function(context, application, package, func, call) {
    ##cat("** Call Entry **\n")
    print(call)
    ##print(func)
}

call_exit_callback <- function(context, application, package, func, call) {
    ##cat("** Call Exit **\n")
    ##print(func)
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
                 mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
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
                 summarise(across(where(is.character), ~ length(unique(.x))))

               starwars %>%
                 group_by(species) %>%
                 filter(n() > 1) %>%
                 summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))))

               starwars %>%
                 group_by(homeworld) %>%
                 filter(n() > 1) %>%
                 summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


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
               df %>% mutate(across(all_of(names(mult)), ~ .x * mult[[cur_column()]]))

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

               starwars %>% filter(across(everything(), ~ !is.na(.x)))


               starwars %>% distinct(across(contains("color")))

               starwars %>% count(across(contains("color")), sort = TRUE)

               df %>% mutate_if(is.numeric, mean, na.rm = TRUE)
                                        # ->
               df %>% mutate(across(where(is.numeric), mean, na.rm = TRUE))
               df %>% mutate_at(vars(c(x, starts_with("y"))), mean)
                                        # ->
               df %>% mutate(across(c(x, starts_with("y")), mean, na.rm = TRUE))
               df %>% mutate_all(mean)
                                        # ->
               df %>% mutate(across(everything(), mean))

               df <- tibble(x = c("a", "b"), y = c(1, 1), z = c(-1, 1))
               df %>% filter(across(where(is.numeric), ~ .x > 0))

               rowAny <- function(x) rowSums(x) > 0

               df %>% filter(rowAny(across(where(is.numeric), ~ .x > 0)))

               df <- tibble(x = 2, y = 4, z = 8)
               df %>% mutate_all(~ .x / y)

               df %>% mutate(across(everything(), ~ .x / y))
           })
