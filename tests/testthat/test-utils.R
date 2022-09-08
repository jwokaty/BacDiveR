empty_list <- list()

acetobacter_orientali <- list(
    `General` = list(
        `@ref`= 5996,
        `BacDive-ID` = 16,
        `DSM-Number` = 15550,
        `keywords` = list("Bacteria", "16S sequence", "genome sequence", "mesophilic"),
        `description` = "Acetobacter orientalis 21F-2 is a mesophilic bacterium that was isolated from canna flower, Canna hybrida.",
        `strain history` =  "<- IFO",
        `doi` = "10.13145/bacdive16.20201210.5"
    ),
    `Isolation, sampling and environmental information` = list(
        isolation = list(
            `@ref` = 5996,
            `sample type` = "canna flower, <I>Canna hybrida</I>",
            `host species` = "Canna hybrida",
            country = "Indonesia",
            `origin.country` = "IDN",
            continent = "Asia"
        ),
        `isolation source categories` = list(
            `isolation source categories` = list(
              Cat1 = "#Host",
              Cat2 = "#Plants",
              Cat3 = "#Herbaceous plants (Grass,Crops)"
            ),
            `isolation source categories` = list(
              Cat1 = "#Host Body-Site",
              Cat2 = "#Plant",
              Cat3 = "#Flower"
            )
        )
    )
)

test_that("getValues returns an empty string if key is a list.", {
    expect_warning(BacDiveR:::.getValues(acetobacter_orientali, "General"))
})

test_that("getValues returns a value from a two keywords.", {
    expect_equal(16, BacDiveR:::.getValues(acetobacter_orientali,
                                           "General",
                                           "BacDive-ID"))
})

test_that("getValues returns an empty string given keys that don't exist.", {
    expect_equal("", BacDiveR:::.getValues(acetobacter_orientali,
                                           "General",
                                           "Stuff"))
})

test_that("getValues returns a value given a key with a comma.", {
    expect_equal("Indonesia",
                 BacDiveR:::.getValues(acetobacter_orientali,
                                       "Isolation, sampling and environmental information",
                                       "isolation",
                                       "country"))
})

test_that(".toVector returns a single value when given no commas or quotes.", {
    expect_equal("one", BacDiveR:::.toVector("one"))
})

test_that(".toVector returns n-1 values when given n commas.", {
  expect_equal(c("one", "two", "three", "four"),
               BacDiveR:::.toVector("one, two, three, four"))
})

test_that(".toVector doesn't split quoted phrases.", {
  expect_equal(c("one", "two three", "four"),
               BacDiveR:::.toVector("one, “two three”, four"))
})

test_that(".toVector doesn't split quoted phrases with commas.", {
  expect_equal(c("one", "two, three", "four"),
               BacDiveR:::.toVector("one, “two, three”, four"))
})
