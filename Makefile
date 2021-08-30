# Determine package name and version from DESCRIPTION file
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

# Name of built package
PKG_TAR=$(PKG_NAME)_$(PKG_VERSION).tar.gz

# Install package
install:
	cd .. && R CMD INSTALL $(PKG_NAME)

# Build documentation with roxygen
# 2) Remove old doc
# 3) Generate documentation
roxygen:
	rm -f man/*.Rd
	cd .. && Rscript -e "library(roxygen2); roxygenize('$(PKG_NAME)')"

# Generate PDF output from the Rd sources
# 1) Rebuild documentation with roxygen
# 2) Generate pdf, overwrites output file if it exists
pdf: roxygen
	cd .. && R CMD Rd2pdf --force $(PKG_NAME)

# Build and check package
check: clean
	cd .. && R CMD build --no-build-vignettes $(PKG_NAME)
	cd .. && _R_CHECK_CRAN_INCOMING_=FALSE R CMD check \
          --as-cran --no-stop-on-test-error --run-dontrun $(PKG_TAR)

# Build and check package on https://win-builder.r-project.org/
.PHONY: winbuilder
winbuilder: clean check
	cd .. && curl -T $(PKG_TAR) ftp://win-builder.r-project.org/R-oldrelease/
	cd .. && curl -T $(PKG_TAR) ftp://win-builder.r-project.org/R-release/
	cd .. && curl -T $(PKG_TAR) ftp://win-builder.r-project.org/R-devel/

.PHONY: clean
clean:
	-rm -f src/*.o
	-rm -f src/*.so
	-rm -rf src-x64
	-rm -rf src-i386

.PHONY: lintr
lintr:
	Rscript -e "library(lintr);lint_package(linters = with_defaults(object_name_linter = NULL, object_usage_linter = NULL))"

.PHONY: install roxygen pdf check
