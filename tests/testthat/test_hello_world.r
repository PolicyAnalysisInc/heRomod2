context('hello world')

test_that('tests are working', {
    expect_output(hello_world(), 'hello world')
})