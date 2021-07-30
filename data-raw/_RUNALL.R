for (f in dir("data-raw", pattern = "^[A-Za-z]*.R$", full.names = TRUE))
  source(f)

