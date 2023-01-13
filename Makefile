.PHONY: test test_all

##### DEFINES #####

CFLAGS := -Wall -Werror -ggdb3 -std=c11

### DIRS ###
SRCS_DIR := srcs
TEST_DIR := test

### BINS ###
RUNNER := runner
TEST_RUNNER := $(TEST_DIR)/test

### DEFAULT TARGET ###
all: run

##### PL-S #####  <-- please fill this section when add new PL

# Files need to be executed (generated files for compiled pl) or
# interpret (for interpreted pl)
C := ./$(SRCS_DIR)/c/core
JS := ./$(SRCS_DIR)/js/core.js
JAVA := ./$(SRCS_DIR)/java/core.class
HASKELL := ./$(SRCS_DIR)/haskell/core

# Default pl. easy for develop
PL ?= HASKELL

# Add PL to it's category. PL_LIST for all)
PL_LIST := C JS JAVA
PL_CMPL_LIST := C JAVA HASKELL
PL_INTRP_LIST := JS JAVA

# Way of execute interpreted files
INTRP_JS := "node $(JS)"
INTRP_JAVA := "java -cp $(dir $(JAVA)) $(notdir $(JAVA:.class=))"

# Way of compile compiled PL-s
$(C): %: %.c %.h
	$(CC) $(CFLAGS) -o $@ $(addsuffix .c,$@)
$(JAVA): $(JAVA:.class=.java)
	javac -Xlint:all $(JAVA:.class=.java)
$(HASKELL): %: %.hs
	ghc -Wall $@


# Way of cleanup
clean_C:
	rm -f $(C)
clean_JAVA:
	rm -f $(dir $(JAVA))/*.class
clean_HASKELL:
	rm -f $(HASKELL)
	rm -f $(addsuffix .hi,$(HASKELL))
	rm -f $(addsuffix .o,$(HASKELL))

# Way of install compiler or sdk
inst_C:
	sudo apt install -y gcc
inst_JS:
	curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
	sudo apt install -y nodejs
inst_JAVA:
	sudo apt install -y openjdk-17-jdk openjdk-17-jre
	sudo update-java-alternatives -s $$(sudo update-java-alternatives -l | grep 1.17 | cut -d " " -f1) || echo '.'
	javac --version
	java --version
inst_HASKELL:
	sudo apt install -y cabal-install #ghc
	cabal update
	cabal install --lib random
	#sudo apt install -y libghc-random-dev

##### END PL-S #####

### ALL PL-S ###
cmpl_all: $(foreach PL, $(PL_CMPL_LIST), $($(PL)))
test_all: $(TEST_RUNNER) cmpl_all inst_all | $(ROTATE_COMMON)
	$(foreach PL, $(PL_LIST), $(MAKE) test PL=$(PL) &&) true
inst_all: $(foreach PL, $(PL_LIST), $(MAKE) inst PL=$(PL) &&) true
clean_all: $(foreach PL, $(PL_CMPL_LIST), clean_$(PL))

### DIFF_PL ###
# used to apply actions (cmpl, test) for PL list that changed in repo
DIFF_PL_LIST ?= $(PL)
cmpl_diff:$(foreach PL, $(filter $(DIFF_PL_LIST), $(PL_CMPL_LIST)), $($(PL)))

inst_diff:
	 $(foreach PL, $(DIFF_PL_LIST),$(MAKE) inst PL=$(PL) &&) true

test_diff: $(TEST_RUNNER) inst_diff cmpl_diff | $(ROTATE_COMMON)
	$(foreach PL, $(DIFF_PL_LIST),$(MAKE) test PL=$(PL) &&) true

### RUNNNER ###
$(RUNNER): runner.c utils.c
one-runner: one-runner.c utils.c

### CURRENT PL ###
cmpl: $(if $(filter $(PL), $(PL_CMPL_LIST)), $($(PL)))

# command to exec core at current PL
run_$(PL) := $(if $(filter $(PL), $(PL_INTRP_LIST)), $(INTRP_$(PL)), $($(PL)))
run: $(RUNNER) cmpl
	./$(RUNNER) $(run_$(PL))
inst: inst_$(PL)

### TEST ###
TEST_INIT_CASES_PATH := $(TEST_DIR)/0-init-cases/init-case-results.txt
TEST_MAIN_CASES_PATH := $(TEST_DIR)/1-main-cases
TEST_END_CASES_PATH := $(TEST_DIR)/2-end-cases

$(TEST_RUNNER): %: %.c utils.c

# generate tests for rotate counter-clockwise from rotate clockwise tests 
# for figures that have two or one rotations (I, S, Z, O) (results are the same, only command is diffirent)
# find lines with arguments; change first '3' to '4'; write changes in-place (echo do the trick)
ROTATE_COMMON := 0_I 3_S 4_Z 5_O
$(ROTATE_COMMON): %: ./$(TEST_MAIN_CASES_PATH)/05_rotate-counter-clockwise/%
$(addprefix ./$(TEST_MAIN_CASES_PATH)/05_rotate-counter-clockwise/, $(ROTATE_COMMON)):
	cp -r $(TEST_MAIN_CASES_PATH)/04_rotate-clockwise/$(notdir $@) $(TEST_MAIN_CASES_PATH)/05_rotate-counter-clockwise
	for f in $$(find $@ -type f); \
	        do \
		echo "$$(awk '{if (((NR - 1) % 25 == 0) || ((NR - 2) % 25 == 0)) $$1=4; print $$0}' $$f)" > $$f; \
	done


test: $(TEST_RUNNER) cmpl | $(ROTATE_COMMON)
	$(TEST_DIR)/test $(run_$(PL)) $(TEST_INIT_CASES_PATH) $(TEST_MAIN_CASES_PATH) $(TEST_END_CASES_PATH)

### CLEAN ###
clean: clean_$(PL)
	rm -f $(RUNNER)
	rm -f one-runner
	rm -f $(TEST_RUNNER)
	rm -rf ./$(TEST_MAIN_CASES_PATH)/05_rotate-counter-clockwise/0_I
	rm -rf ./$(TEST_MAIN_CASES_PATH)/05_rotate-counter-clockwise/3_S
	rm -rf ./$(TEST_MAIN_CASES_PATH)/05_rotate-counter-clockwise/4_Z
	rm -rf ./$(TEST_MAIN_CASES_PATH)/05_rotate-counter-clockwise/5_O


### OTHER ###
one: one-runner
	> logs.txt
	xargs -a ./one-cases.txt -L 1 ./one-runner $($(PL))

check_test:
	# exclude game over files
	for f in $$(find $(TEST_MAIN_CASES_PATH) -type f); \
	do awk -f $(TEST_DIR)/check_test.awk $$f; \
	done

change_tests:
	for f in $$(find $(TEST_DIR)/$(TEST_MAIN_CASES_PATH)/06_drop -type f); \
	do \
		echo "$$(awk '((NR - 2) % 25 == 0) {$$8="?";$$9="?"} {print $$0}' $$f)" > $$f; \
	done
