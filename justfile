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
    Rscript -e "styler::style_dir('.', exclude_files = c('^.direnv', '^paper', '^rsconnect', '^tests'), recursive = TRUE)"
    Rscript -e "if (dir.exists('tests')) styler::style_dir('tests') else cat('No tests/ directory found\n')"
    nix fmt flake.nix
    just --unstable --fmt

# Lint R code
lint:
    Rscript -e "lintr::lint_dir('.', exclusions = c('.direnv', 'paper', 'rsconnect', 'tests'))"
    Rscript -e "if (dir.exists('tests')) lintr::lint_dir('tests') else cat('No tests/ directory found\n')"

# Start shinymapper app
app:
    R -e "shiny::runApp('app.R', host='0.0.0.0', port=3838)" 2>&1 | tee app.log

# Test the code
test:
    echo "Tests not yet implemented."
