CFLAGS += -fprofile-arcs -ftest-coverage
LDFLAGS += -fprofile-arcs -ftest-coverage

srcfiles = main.c extra.c
objfiles = $(srcfiles:.c=.o)
htmldir = lcov-html
htmlfiles = $(addprefix $(htmldir)/same-dir/,$(srcfiles:.c=.c.gcov.html))
testcases = testprog1 testprog2
testinfo = $(addsuffix .info,$(testcases))
lcov_flags = --directory . --checksum --no-external

testprog2_args = 1 2 3

# The testinfo targets touch the same files (*.gcda)
# The htmlfiles targets touch the same files
.NOTPARALLEL:

all: lcov.info expected.el

# Create an info file from more than one tracefile.
# The combined files may list the same file and lines more than once.
lcov.info: $(testinfo)
	$(RM) $@
	lcov $(lcov_flags) --add-tracefile baseline.info $(addprefix --add-tracefile ,$^) --output-file $@
# Remove the local path to simplify tests
	sed -i -e 's!SF:.*\(/test/lcov/\)!SF:\1!' $@

# Create zero-coverage baseline tracefile
baseline.info: testprog
	lcov $(lcov_flags) --capture --initial --output-file baseline.info

# Generate a tracefile for a single test run.
$(testinfo): testprog baseline.info
	$(RM) $@
# Clear any accumulated coverage data from last run
	lcov $(lcov_flags) --zerocounters
	./$< $($(basename $@)_args) >/dev/null || true
	lcov $(lcov_flags) --capture --test-name $(basename $@) --output-file $@

html $(htmlfiles): $(testinfo)
	genhtml -o $(htmldir) $^

# This is the data we expect the elisp lcov-parser to generate
# Obviously this is fragile, using sed to generate lisp data from lcov-generated html.
expected.el: $(sort $(htmlfiles))
	sed -n -e '1i (' \
		-e '/.*title.*LCOV.* \(.*\)<\/title>.*/s__ ("/test/lcov/\1" . (_p' \
		-e '/lineCov/{' \
		-e  's_<span class="lineNum"> *\([0-9]\+\) *</span>_  (\1_' \
		-e  's_<span class="lineCov"> *\([0-9]\+\) *:\(.*\)</span>_ \1) ; \2_' \
		-e  'p' \
		-e '}' \
		-e '/<\/html>/s/.*/  ))/p' \
		-e '$$i )' $^ >$@

testprog: $(objfiles)
	$(LINK.o) $^ $(LOADLIBES) $(LDLIBS) -o $@

clean:
	$(RM) -f testprog *.o baseline.info lcov.info $(testinfo) *.gcda *.gcno expected.el
	$(RM) -rf $(htmldir)
