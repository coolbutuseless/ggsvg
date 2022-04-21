


test_that("css aesthetic utilities works", {

  expect_true(is_valid_css_aes("css=#thing_crap"))
  expect_true(is_valid_css_aes("css=#thing .yes rect_crap"))

  expect_false(is_valid_css_aes("css=#thing_"))
  expect_false(is_valid_css_aes("css=_crap"))
  expect_false(is_valid_css_aes("css:#thing_crap"))
  expect_false(is_valid_css_aes("css="))
  expect_false(is_valid_css_aes(" css=#thing_crap"))
  expect_false(is_valid_css_aes("colour"))
})
