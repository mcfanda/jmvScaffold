test_that("empty roots apply unprefixed CI supertitles during initialization", {
    table <- make_ci_table()
    smart <- SmartTable$new(table)

    smart$ci("", 95)
    expect_ci_titles(table, c("ci.lower", "ci.upper"), NULL)

    smart$initTable()
    expect_ci_titles(table, c("ci.lower", "ci.upper"), " 95% Confidence Intervals")
})

test_that("zero-length and NULL roots retain unprefixed CI supertitles", {
    for (root in list(character(0), NULL)) {
        table <- make_ci_table()
        smart <- SmartTable$new(table)
        smart$ci(root, 95)

        smart$initTable()
        expect_ci_titles(table, c("ci.lower", "ci.upper"), " 95% Confidence Intervals")
    }
})

test_that("nonempty roots update only their prefixed interval columns", {
    prefixed_columns <- c("estimate.ci.lower", "estimate.ci.upper")
    unprefixed_columns <- c("ci.lower", "ci.upper")
    table <- make_ci_table(c(prefixed_columns, unprefixed_columns))
    smart <- SmartTable$new(table)
    smart$ci("estimate", 95)

    smart$initTable()
    expect_ci_titles(table, prefixed_columns, " 95% Confidence Intervals")
    expect_ci_titles(table, unprefixed_columns, NULL)
})

test_that("CI supertitles use nondefault confidence widths", {
    table <- make_ci_table()
    smart <- SmartTable$new(table)
    smart$ci("", 90)

    smart$initTable()
    expect_ci_titles(table, c("ci.lower", "ci.upper"), " 90% Confidence Intervals")
})

test_that("CI supertitles preserve custom labels and formats", {
    table <- make_ci_table()
    smart <- SmartTable$new(table)
    smart$ci("", 90, label = "Custom", format = "{}% interval")

    smart$initTable()
    expect_ci_titles(table, c("ci.lower", "ci.upper"), "Custom 90% interval")
})

test_that("missing interval columns are safely skipped", {
    for (column_names in list("ci.lower", "ci.upper", "unrelated")) {
        table <- make_ci_table(column_names)
        smart <- SmartTable$new(table)
        smart$ci("", 95)

        expect_no_error(smart$initTable())
        if (identical(column_names, "unrelated")) {
            expect_ci_titles(table, column_names, NULL)
        } else {
            expect_ci_titles(table, column_names, " 95% Confidence Intervals")
        }
    }
})

test_that("multiple CI roots retain independent supertitles", {
    unprefixed_columns <- c("ci.lower", "ci.upper")
    prefixed_columns <- c("estimate.ci.lower", "estimate.ci.upper")
    table <- make_ci_table(c(unprefixed_columns, prefixed_columns))
    smart <- SmartTable$new(table)
    smart$ci("", 90)
    smart$ci("estimate", 99, label = "Estimate", format = "{}% CI")

    smart$initTable()
    expect_ci_titles(table, unprefixed_columns, " 90% Confidence Intervals")
    expect_ci_titles(table, prefixed_columns, "Estimate 99% CI")
})

test_that("SmartArray children inherit CI configuration during initialization", {
    table <- make_ci_table()
    group <- jmvcore::Group$new(options = jmvcore::Options$new(), name = "group")
    group$add(table)
    smart_array <- SmartArray$new(group)
    smart_array$ci("", 90, label = "Inherited", format = "{}% CI")
    smart_array$initSource <- list(intervals = NULL)

    expect_ci_titles(table, c("ci.lower", "ci.upper"), NULL)
    smart_array$initTable()
    expect_length(smart_array$childrenObjs, 1)
    expect_identical(smart_array$childrenObjs[[1]]$ci_info, smart_array$ci_info)
    expect_ci_titles(table, c("ci.lower", "ci.upper"), "Inherited 90% CI")
})

test_that("the distributed SmartTabs copy preserves CI initialization", {
    skip_if_not_installed("jmvcore")
    distributed <- new.env(parent = environment(SmartTable$public_methods$ci))
    sys.source(system.file("SmartTabs.R", package = "jmvScaffold"), envir = distributed)

    expect_identical(
        body(distributed$SmartTable$private_methods$.ci),
        body(SmartTable$private_methods$.ci)
    )

    for (root in list("", character(0), NULL, "estimate")) {
        prefix <- if (length(root) == 0 || identical(root, "")) "" else paste0(root, ".")
        column_names <- paste0(prefix, c("ci.lower", "ci.upper"))
        table <- make_ci_table(column_names)
        smart <- distributed$SmartTable$new(table)
        smart$ci(root, 90, label = "Distributed", format = "{}% CI")

        smart$initTable()
        expect_ci_titles(table, column_names, "Distributed 90% CI")
    }
})
