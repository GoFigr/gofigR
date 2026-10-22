# gofigR (development version)

* Jupyter (IRkernel) support: `enable(auto_publish = TRUE)` hooks the kernel's
  display so every plot a cell shows is published, and published figures are
  shown as rich output (image + published-revision widget + the GoFigr figure
  mime type the GoFigr canvas renders natively). `enable()` shows a startup
  widget in notebooks.
* `use_asset()`: materialize a GoFigr asset revision in the working directory
  and link it to figures published afterwards.

# gofigR 0.2.1

* Initial CRAN submission. This release supports both automatic and manual
publication of figures to GoFigr.io.
