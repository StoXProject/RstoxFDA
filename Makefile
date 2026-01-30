PACKAGE=$(shell awk '/^Package: / { print $$2 }' DESCRIPTION)
VERSION=$(shell awk '/^Version: / { print $$2 }' DESCRIPTION)
TARBALL=$(PACKAGE)_$(VERSION).tar.gz

roxygen:
	Rscript -e "library(roxygen2); roxygenize('.')"

build:
	R CMD build .

build-lite:
	R CMD build --no-build-vignettes .

check:
	R CMD check "$(TARBALL)"

check-as-cran: roxygen build
	R CMD check --as-cran "$(TARBALL)"

install:
	R CMD INSTALL --install-tests --html --example "$(TARBALL)"

test:
	parallel -j 8 --halt now,fail=1 Rscript ::: tests/test*.R

coverage:
	Rscript -e "library(covr); coverage <- package_coverage(path='.', type='tests'); print(coverage); to_cobertura(coverage, file='coverage.xml'); report(coverage, 'coverage.html', FALSE)"

clean:
	$(RM) $(TARBALL)
	$(RM) -r $(PACKAGE).Rcheck
	$(RM) -r coverage.* lib/

all: roxygen build check install

.PHONY: all clean build build-lite check check-as-cran install test coverage
