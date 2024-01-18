
test_that("re-proportioning to 100% works", {
    expect_warning(expect_equal(
        object = category(
            name = "foo",
            groups = c("a", "b"),
            targets = c(1/4, 1/4)
        )$foo$targets_sum1,
        expected = c(0.5, 0.5)
    ))
})
