context('formulas')

test_that('Creating a Formula', {

    # Creating a valid formula
    valid_formula_string <- 'foo + bar + 1'
    valid_res_expect <- list(
        text = valid_formula_string,
        expr = parse(text = valid_formula_string),
        refs = c('+', 'foo', 'bar')
    )
    attributes(valid_res_expect$expr) <- NULL
    class(valid_res_expect) <- c('hero_formula', 'list')
    valid_res <- define_formula(valid_formula_string)
    expect_equal(
        capture.output(str(valid_res)),
        capture.output(str(valid_res_expect))
    )

    # Creating a formula with a syntax error
    invalid_formula_string <- 'foo + bar + 1 +'
    invalid_res_expect <- list(
        text = invalid_formula_string,
        expr = parse(text = "define_error('Syntax error')"),
        refs = character()
    )
    attributes(invalid_res_expect$expr) <- NULL
    class(invalid_res_expect) <- c('hero_formula', 'list')
    invalid_res <- define_formula(invalid_formula_string)
    expect_equal(
        capture.output(str(invalid_res)),
        capture.output(str(invalid_res_expect))
    )
})

test_that('Evaluating a Formula', {

    # Set up an environment to evaluate formulas in
    test_env <- new.env()
    test_env$foo <- 1
    test_env$bar <- 2

    # Evaluate a valid formula with no runtime errors
    valid_formula <- define_formula('(foo + bar) ^ bar')
    expect_equal(
        eval_formula(valid_formula, test_env),
        9
    )

    # Evaluate a valid formula with runtime errors
    runtime_err_formula <- define_formula('(foo + bar) ^ baz')
    runtime_err_test_expect <- list(message = 'Variable "baz" not found.')
    class(runtime_err_test_expect) <- 'hero_error'
    expect_equal(
        eval_formula(runtime_err_formula, test_env),
        runtime_err_test_expect
    )


})