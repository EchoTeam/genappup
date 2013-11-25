.PHONY: all escriptize rpm rpm_ll clean

prefix="/usr/local/bin"

epoch=$(shell date +%s)
year=$(shell date +%Y)

vendor="jacknyfe"
license="Copyright ${year} by Jacknyfe Inc. All Rights Reserved"
url="http://aboutecho.com/"

project_name="genappup"
project_version=$(shell git describe --always --tags)

commit_hash=$(shell git log -n 1 --format="%H")

all: escriptize

escriptize:
	./rebar compile escriptize

rpm: all rpm_ll

rpm_ll:
	fpm -s dir \
		-t rpm \
		-a all \
		--prefix=${prefix} \
		--vendor=${vendor} \
		--license=${license} \
		--url=${url} \
		--epoch=${epoch} \
		--name=${project_name} \
		--version=${project_version} \
		--provides=${project_name} \
		./genappup

clean:
	./rebar clean
	rm -rf ./genappup*.rpm
