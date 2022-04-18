


test_that("css aesthetic utilities works", {

  expect_true(is_valid_css_aes("css=#thing:crap"))
  expect_true(is_valid_css_aes("css=#thing .yes rect:crap"))

  expect_false(is_valid_css_aes("css=#thing:"))
  expect_false(is_valid_css_aes("css=:crap"))
  expect_false(is_valid_css_aes("css:#thing:crap"))
  expect_false(is_valid_css_aes("css="))
  expect_false(is_valid_css_aes(" css=#thing:crap"))
  expect_false(is_valid_css_aes("colour"))
})
