test_that("distributed R files match their package source copies", {
    source_root <- test_path("..", "..")
    inst_directory <- file.path(source_root, "inst")
    skip_if_not(dir.exists(inst_directory), "Source checkout is required")
    distributed_files <- list.files(inst_directory, pattern = "\\.R$", full.names = TRUE)

    expect_gt(length(distributed_files), 0)
    for (distributed_file in distributed_files) {
        source_file <- file.path(source_root, "R", basename(distributed_file))
        expect_true(file.exists(source_file), info = basename(distributed_file))
        if (file.exists(source_file)) {
            expect_identical(
                readBin(source_file, what = "raw", n = file.info(source_file)$size),
                readBin(distributed_file, what = "raw", n = file.info(distributed_file)$size),
                info = basename(distributed_file)
            )
        }
    }
})
