context("teste da funcao primeirasMedicoes")

test_that("firstsMeasurements", {
  dtf = data.frame(
    grupo = c(1,2,3,4,1,2,3,4),
    medicoes= c(10,20,30,40,5,30,1,52))
  primeiraMedicao = c(5, 5, 20, 20, 1, 1, 40, 40)
  expect_equal(primeiraMedicao, firstsMeasurements(dtf, group = "grupo", age = "medicoes")$primeiraMedicao)
})
