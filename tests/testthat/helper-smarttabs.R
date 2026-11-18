make_ci_table <- function(column_names = c("ci.lower", "ci.upper")) {
    skip_if_not_installed("jmvcore")
    jmvcore::Table$new(
        name = "intervals",
        columns = lapply(column_names, function(column_name) {
            list(name = column_name, type = "number")
        })
    )
}

expect_ci_titles <- function(table, column_names, expected) {
    for (column_name in column_names) {
        expect_identical(
            table$getColumn(column_name)$superTitle,
            expected,
            info = column_name
        )
    }
}
