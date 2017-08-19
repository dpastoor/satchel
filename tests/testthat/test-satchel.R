context("satchel")

describe("satchel basics", {
    tiny_theoph <- head(Theoph)
    tmpdir_top <- tempdir()
    tmpdir1 <- file.path(tmpdir_top, "set1")
    cleanup <- function() {
        message("cleaning up temporary directories")
        unlink(tmpdir1, recursive = TRUE, force = TRUE)
    }
    on.exit(cleanup(), add = TRUE)
    dir.create(tmpdir1, recursive = TRUE)
    it("initialization works as expected", {
        expect_error(Satchel$new("tmp", file.path(tmpdir1, "notexist")), "no directory detected at")
        expect_message(satchel1 <- Satchel$new("tmp", tmpdir1))
        expect_equal(satchel1$available(), list(tmp = character(0)))
    })

    #globally use satchel1 now
    satchel1 <- Satchel$new("tmp", tmpdir1)

    it("can save files", {
       satchel1$save(tiny_theoph)
       satchel_files <- dir(tmpdir1, recursive = TRUE)
       expect_equal(length(satchel_files), 2)
       satchel1$save(Theoph)
       satchel_files <- dir(tmpdir1, recursive = TRUE)
       expect_equal(length(satchel_files), 4)
    })
    it("can reload objects that have been saved", {
       expect_equal(satchel1$use("tiny_theoph"), tiny_theoph)
       expect_equal(satchel1$use("Theoph"), Theoph)
    })

    satchel2 <- Satchel$new("sat2", tmpdir1)
    it("can pick up from another satchel", {
        expect_equal(satchel2$available(), list(sat2 = character(0), tmp = c("Theoph", "tiny_theoph")))
    })
})