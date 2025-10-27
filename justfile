# List inputs
default:
    @just --list

# Build the paper
build:
    cd paper && latexmk

# Clean build artifacts
clean:
    cd paper && latexmk -c
    find . -name "*.Rhistory" -delete
    find . -name "*.RData" -delete
    find . -name "*.Rproj.user" -type d -exec rm -rf {} +

alias fmt := format

# Format all the code
format:
    Rscript -e "styler::style_dir('.')"
    Rscript -e "styler::style_dir('tests')"
    nix fmt flake.nix
    just --unstable --fmt

# Lint R code
lint:
    Rscript -e "lintr::lint_dir('R')"
    Rscript -e "lintr::lint_dir('tests')"

# Start shinymapper app
app:
    R -e "shiny::runApp('app.R', host='0.0.0.0', port=3838)" 2>&1 | tee app.log

# Test the code
test:
    echo "Tests not yet implemented."
