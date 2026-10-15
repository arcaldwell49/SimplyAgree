# hand-built long data ----
# 3 methods, 4 items; replicate counts vary by method and item
mc_test_data = function() {
  data.frame(
    subj = c(rep("s1", 6), rep("s2", 5), rep("s3", 4), rep("s4", 2)),
    meth = c("B", "B", "A", "A", "C", "C",
             "A", "A", "A", "B", "C",
             "A", "B", "B", "C",
             "A", "A"),
    occ = c(1, 2, 1, 2, 1, 2,
            1, 2, 3, 1, 1,
            1, 1, 2, 1,
            1, 2),
    val = c(5.1, 5.3, 5.0, 4.8, 6.1, 6.0,
            3.1, 3.3, 3.2, 3.6, 4.0,
            7.2, 7.5, 7.1, 8.0,
            2.0, 2.2)
  )
}

# mc_transform ----
test_that("built-in transforms round-trip and have correct derivatives", {
  for (nm in c("none", "log", "logit", "pctlogit", "sqrt", "asinsqrt")) {
    tf = mc_transform(nm)
    expect_named(tf, c("name", "fun", "inv", "deriv", "domain_check",
                       "domain", "label"))
    expect_equal(tf$name, nm)
    x = if (nm == "pctlogit") c(5, 30, 72.5) else c(0.05, 0.3, 0.725)
    expect_true(tf$domain_check(x))
    expect_equal(tf$inv(tf$fun(x)), x, tolerance = 1e-10)
    num_deriv = (tf$fun(x + 1e-6) - tf$fun(x - 1e-6)) / 2e-6
    expect_equal(tf$deriv(x), num_deriv, tolerance = 1e-6)
    expect_type(tf$label("y"), "character")
  }
})

test_that("transform domains are enforced", {
  expect_false(mc_transform("log")$domain_check(c(1, 0)))
  expect_false(mc_transform("logit")$domain_check(c(0.5, 1)))
  expect_false(mc_transform("pctlogit")$domain_check(c(50, 100)))
  expect_false(mc_transform("sqrt")$domain_check(c(-1, 2)))
  expect_false(mc_transform("asinsqrt")$domain_check(c(0.5, 1.2)))
  expect_true(mc_transform("asinsqrt")$domain_check(c(0, 1)))
})

test_that("custom transforms are validated", {
  tf = mc_transform(list(fun = log10, inv = function(z) 10^z, name = "log10"))
  expect_equal(tf$name, "log10")
  expect_true(tf$domain_check(c(1, 10, 50)))
  expect_false(tf$domain_check(c(-1, 10)))
  expect_equal(tf$deriv(10), 1 / (10 * log(10)), tolerance = 1e-6)

  # wrong inverse fails the numerical check
  bad = mc_transform(list(fun = log, inv = function(z) z))
  expect_equal(bad$name, "custom")
  expect_false(bad$domain_check(c(2, 3)))

  expect_error(mc_transform(list(fun = log)), "custom transform")
  expect_error(mc_transform("boxcox"), "Unknown transform")
  expect_error(mc_transform(c("log", "sqrt")), "single character")
})

# mc_data_org ----
test_that("mc_data_org standardizes columns and keeps level order", {
  dat = mc_test_data()
  dat$meth = factor(dat$meth, levels = c("C", "A", "B"))
  df = suppressMessages(
    mc_data_org(dat, y = "val", method = "meth", id = "subj",
                replicate = "occ")
  )
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("y", "y_t", "method", "id", "repl"))
  expect_equal(levels(df$method), c("C", "A", "B"))
  expect_s3_class(df$id, "factor")
  expect_equal(df$y_t, df$y)
})

test_that("mc_data_org drops items measured by one method and counts design", {
  dat = mc_test_data()
  expect_message(
    df <- mc_data_org(dat, y = "val", method = "meth", id = "subj",
                      replicate = "occ"),
    "1 item\\(s\\) measured by fewer than two methods removed"
  )
  expect_equal(levels(df$id), c("s1", "s2", "s3"))
  expect_equal(nrow(df), 15)
  expect_equal(attr(df, "n_items"), 3)
  expect_equal(attr(df, "n_methods"), 3)

  # A: s1 x2, s2 x3, s3 x1; B: s1 x2, s2 x1, s3 x2; C: s1 x2, s2 x1, s3 x1
  rt = attr(df, "rep_table")
  expect_equal(as.character(rt$method),
               c("A", "A", "A", "B", "B", "C", "C"))
  expect_equal(rt$n_reps, c(1, 2, 3, 1, 2, 1, 2))
  expect_equal(rt$n_items, c(1, 1, 1, 1, 2, 2, 1))
  expect_true(attr(df, "has_reps"))
  expect_equal(attr(df, "transform")$name, "none")

  # no replicates of C
  df2 = suppressMessages(
    mc_data_org(dat[!(dat$meth == "C" & dat$occ == 2), ], y = "val",
                method = "meth", id = "subj", replicate = "occ")
  )
  expect_false(attr(df2, "has_reps"))
})

test_that("mc_data_org handles missing values and linked replicates", {
  dat = mc_test_data()
  dat$val[2] = NA
  expect_message(
    df <- mc_data_org(dat[dat$meth != "C", ], y = "val", method = "meth",
                      id = "subj", replicate = "occ", replicates = "linked"),
    "1 row\\(s\\) with a missing measurement removed"
  )
  expect_true("ir" %in% names(df))
  expect_equal(nlevels(df$ir), nlevels(interaction(df$id, df$repl,
                                                   drop = TRUE)))
  expect_true(attr(df, "has_reps"))
  expect_equal(attr(df, "replicates"), "linked")
})

test_that("mc_data_org applies the transformation", {
  dat = mc_test_data()
  df = suppressMessages(
    mc_data_org(dat, y = "val", method = "meth", id = "subj",
                replicate = "occ", transform = "log")
  )
  expect_equal(df$y_t, log(df$y))
  dat$val[1] = -1
  expect_error(
    suppressMessages(mc_data_org(dat, y = "val", method = "meth",
                                 id = "subj", replicate = "occ",
                                 transform = "log")),
    "outside the domain"
  )
})

test_that("mc_data_org input validation", {
  dat = mc_test_data()
  expect_error(mc_data_org(dat, y = "nope", method = "meth", id = "subj"),
               "not found")
  expect_error(mc_data_org(dat, y = c("val", "occ"), method = "meth",
                           id = "subj"),
               "single column name")
  expect_error(mc_data_org(dat, y = "val", method = "meth", id = "subj"),
               "supply `replicate`")
  expect_error(mc_data_org(dat, y = "val", method = "meth", id = "subj",
                           replicates = "linked"),
               "requires `replicate`")
  dup = rbind(dat, dat[1, ])
  expect_error(mc_data_org(dup, y = "val", method = "meth", id = "subj",
                           replicate = "occ"),
               "same method, id, and replicate")
  one = dat[dat$meth == "A", ]
  expect_error(mc_data_org(one, y = "val", method = "meth", id = "subj",
                           replicate = "occ"),
               "At least two methods")
  chr = dat
  chr$val = as.character(chr$val)
  expect_error(mc_data_org(chr, y = "val", method = "meth", id = "subj",
                           replicate = "occ"),
               "must be numeric")
  expect_error(mc_data_org(as.list(dat), y = "val", method = "meth",
                           id = "subj"),
               "data frame")
})

# mc_pair ----
test_that("mc_pair pairs linked replicates by occasion", {
  dat = mc_test_data()
  df = suppressMessages(
    mc_data_org(dat, y = "val", method = "meth", id = "subj",
                replicate = "occ", replicates = "linked")
  )
  pp = mc_pair(df, "A", "B", replicates = "linked")
  expect_named(pp, c("id", "repl", "y1", "y2", "d", "avg",
                     "y1_orig", "y2_orig"))
  # s1: occ 1, 2; s2: occ 1; s3: occ 1
  expect_equal(nrow(pp), 4)
  expect_equal(pp$y1, c(5.0, 4.8, 3.1, 7.2))
  expect_equal(pp$y2, c(5.1, 5.3, 3.6, 7.5))
  expect_equal(pp$d, pp$y1 - pp$y2)
  expect_equal(pp$avg, (pp$y1 + pp$y2) / 2)

  expect_error(mc_pair(df, "A", "A"), "two different methods")
  expect_error(mc_pair(df, "A", "Z"), "two different methods")
})

test_that("mc_pair with a seed is reproducible and keeps the global RNG", {
  dat = mc_test_data()
  df = suppressMessages(
    mc_data_org(dat, y = "val", method = "meth", id = "subj",
                replicate = "occ")
  )
  set.seed(123)
  seed_before = .Random.seed
  p1 = mc_pair(df, "A", "B", seed = 42)
  p2 = mc_pair(df, "A", "B", seed = 42)
  expect_identical(p1, p2)
  expect_identical(.Random.seed, seed_before)

  # number of pairs per item is the smaller replicate count
  expect_equal(as.vector(table(p1$id)), c(2, 1, 1))
  # every pair uses observed values of the item
  expect_true(all(p1$y1_orig %in% dat$val[dat$meth == "A"]))
  # differences on the transformed scale
  df_log = suppressMessages(
    mc_data_org(dat, y = "val", method = "meth", id = "subj",
                replicate = "occ", transform = "log")
  )
  pl = mc_pair(df_log, "A", "C", seed = 1)
  expect_equal(pl$d, log(pl$y1_orig) - log(pl$y2_orig))
})

test_that("mc_pair without replicates pairs by item", {
  dat = mc_test_data()
  dat = dat[dat$occ == 1, ]
  df = suppressMessages(
    mc_data_org(dat, y = "val", method = "meth", id = "subj")
  )
  expect_true(all(df$repl == 1))
  pp = mc_pair(df, "B", "C")
  expect_equal(nrow(pp), 3)
  expect_equal(pp$d, c(5.1 - 6.1, 3.6 - 4.0, 7.5 - 8.0))
})

# mc_long ----
test_that("mc_long converts wide data", {
  wide = data.frame(a = 1:4, b = c(2, 3, NA, 5))
  long = mc_long(wide, methods = c("b", "a"))
  expect_s3_class(long, "tbl_df")
  expect_named(long, c("id", "method", "y"))
  expect_equal(nrow(long), 8)
  expect_equal(levels(long$method), c("b", "a"))
  expect_equal(long$y[long$method == "b"], wide$b)

  long2 = mc_long(wide, methods = c("a", "b"), names_to = "meth",
                  values_to = "val")
  expect_named(long2, c("id", "meth", "val"))
})

test_that("mc_long creates replicates for repeated ids and keeps names", {
  data("reps", package = "SimplyAgree", envir = environment())
  expect_message(long <- mc_long(reps, methods = c("x", "y"), id = "id"),
                 "replicate")
  expect_named(long, c("id", "replicate", "method", "y"))
  expect_equal(nrow(long), 2 * nrow(reps))
  expect_equal(long$replicate[long$method == "x"],
               stats::ave(reps$id, reps$id, FUN = seq_along))

  reps2 = reps
  reps2$occ = stats::ave(reps$id, reps$id, FUN = seq_along)
  long2 = mc_long(reps2, methods = c("x", "y"), id = "id", replicate = "occ")
  expect_named(long2, c("id", "occ", "method", "y"))

  # usable by mc_data_org
  df = suppressMessages(
    mc_data_org(long2, y = "y", method = "method", id = "id",
                replicate = "occ", replicates = "linked")
  )
  expect_equal(attr(df, "n_items"), 4)
})

test_that("mc_long input validation", {
  wide = data.frame(a = 1:4, b = 4:1, id = 1:4)
  expect_error(mc_long(wide, methods = "a"), "at least two")
  expect_error(mc_long(wide, methods = c("a", "z")), "not found")
  expect_error(mc_long(wide, methods = c("a", "a")), "duplicates")
  expect_error(mc_long(wide, methods = c("a", "b")), "already has a column")
  expect_error(mc_long(wide, methods = c("a", "b"), id = "a"),
               "must not be method columns")
  expect_error(mc_long(as.list(wide), methods = c("a", "b")), "data frame")
})
