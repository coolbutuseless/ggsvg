


test_that("css aesthetic utilities works", {

  expect_true(is_valid_css_aes("css_#thing_crap"))
  expect_true(is_valid_css_aes("css_#thing .yes rect_crap"))
  expect_true(is_valid_css_aes(" css_#thing_crap"))

  expect_false(is_valid_css_aes("css_#thing_"))
  expect_false(is_valid_css_aes("css_crap"))
  expect_false(is_valid_css_aes("css_"))
  expect_false(is_valid_css_aes("colour"))
})
