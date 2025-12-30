# -------- Toolchain --------
CC      ?= cc

# Optional: Homebrew LLVM (macOS)
# If you installed LLVM via brew, tools may live under:
#   /opt/homebrew/opt/llvm/bin (Apple Silicon) or /usr/local/opt/llvm/bin (Intel)
# This is only used for linting unless you explicitly override CC.
LLVM_PREFIX ?= $(shell brew --prefix llvm 2>/dev/null)
CLANG_TIDY_BREW := $(if $(LLVM_PREFIX),$(LLVM_PREFIX)/bin/clang-tidy,)
CLANG_BREW      := $(if $(LLVM_PREFIX),$(LLVM_PREFIX)/bin/clang,)

# Base warnings + language standard
BASE_CFLAGS := -std=c11 -Wall -Wextra

# clang-tidy default checks are typically clang-analyzer-* + clang-diagnostic-*.
# On macOS, the insecureAPI.DeprecatedOrUnsafeBufferHandling check is very noisy
# and suggests non-portable *_s functions; we disable it by default.
CLANG_TIDY_CHECKS ?= clang-diagnostic-*,clang-analyzer-*,-clang-analyzer-security.insecureAPI.DeprecatedOrUnsafeBufferHandling,-clang-analyzer-security.ArrayBound

# Default build (release)
CFLAGS  ?= $(BASE_CFLAGS) -O2
LDFLAGS ?=

# -------- Project layout --------
SRC_DIR   := src
EX_DIR    := examples
BIN_DIR   := bin
BUILD_DIR := build

TOOLS := svm_asm svm_vm svm_disasm zmc

ZMC_SRCS := zmc_main.c zmc_util.c zmc_lexer.c zmc_ir.c zmc_parser.c zmc_codegen.c
ZMC_OBJS := $(addprefix $(BUILD_DIR)/,$(ZMC_SRCS:.c=.o))

SRCS := $(SRC_DIR)/svm_asm.c $(SRC_DIR)/svm_vm.c $(SRC_DIR)/svm_disasm.c $(addprefix $(SRC_DIR)/,$(ZMC_SRCS))
OBJS := $(BUILD_DIR)/svm_asm.o $(BUILD_DIR)/svm_vm.o $(BUILD_DIR)/svm_disasm.o $(ZMC_OBJS)
DEPS := $(OBJS:.o=.d)

.PHONY: all release debug asan ubsan clean lint run disasm test web help
all: release

help:
	@echo "Targets:"
	@echo "  release   Build optimized binaries (default)"
	@echo "  debug     Build with debug symbols"
	@echo "  asan      Build with AddressSanitizer"
	@echo "  ubsan     Build with UndefinedBehaviorSanitizer"
	@echo "  lint      Run static analysis (clang-tidy if available, else clang --analyze)"
	@echo "  run       Assemble+run: make run PROG=examples/foo.asm"
	@echo "  disasm    Disassemble:  make disasm ZVM=program.zvm"
	@echo "  test      Run golden output tests (./run_test.sh)"
	@echo "  clean     Remove build/ and bin/"
	@echo ""
	@echo "Tools (built into bin/): $(TOOLS)"

# -------- Build modes --------
release: CFLAGS := $(BASE_CFLAGS) -O2
release: all_tools

debug: CFLAGS := $(BASE_CFLAGS) -O0 -g
debug: all_tools

asan: CFLAGS := $(BASE_CFLAGS) -O1 -g -fsanitize=address -fno-omit-frame-pointer
asan: LDFLAGS := -fsanitize=address
asan: all_tools

ubsan: CFLAGS := $(BASE_CFLAGS) -O1 -g -fsanitize=undefined -fno-omit-frame-pointer
ubsan: LDFLAGS := -fsanitize=undefined
ubsan: all_tools

# -------- Main build --------
all_tools: $(BIN_DIR) $(BUILD_DIR) $(addprefix $(BIN_DIR)/,$(TOOLS))

$(BIN_DIR) $(BUILD_DIR):
	mkdir -p $@

# Compile each tool (one .c per tool) with dependency generation
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -MMD -MP -c -o $@ $<

# Link
$(BIN_DIR)/svm_asm: $(BUILD_DIR)/svm_asm.o | $(BIN_DIR)
	$(CC) -o $@ $^ $(LDFLAGS)

$(BIN_DIR)/svm_vm: $(BUILD_DIR)/svm_vm.o | $(BIN_DIR)
	$(CC) -o $@ $^ $(LDFLAGS)

$(BIN_DIR)/svm_disasm: $(BUILD_DIR)/svm_disasm.o | $(BIN_DIR)
	$(CC) -o $@ $^ $(LDFLAGS)

$(BIN_DIR)/zmc: $(ZMC_OBJS) | $(BIN_DIR)
	$(CC) -o $@ $^ $(LDFLAGS)

-include $(DEPS)

# -------- Lint / Static analysis --------
# Usage: make lint
# Prefers clang-tidy if installed; otherwise uses clang static analyzer.
lint: | $(BUILD_DIR)
	@set -eu; \
	CT=""; \
	CL=""; \
	if command -v clang-tidy >/dev/null 2>&1; then CT="clang-tidy"; \
	elif [ -n "$(CLANG_TIDY_BREW)" ] && [ -x "$(CLANG_TIDY_BREW)" ]; then CT="$(CLANG_TIDY_BREW)"; \
	fi; \
	if command -v clang >/dev/null 2>&1; then CL="clang"; \
	elif [ -n "$(CLANG_BREW)" ] && [ -x "$(CLANG_BREW)" ]; then CL="$(CLANG_BREW)"; \
	fi; \
	if [ -n "$$CT" ]; then \
		echo "Running $$CT..."; \
		"$$CT" -checks="$(CLANG_TIDY_CHECKS)" $(SRCS) -- -I$(SRC_DIR) $(BASE_CFLAGS) ; \
	elif [ -n "$$CL" ]; then \
		echo "clang-tidy not found; running $$CL --analyze..."; \
		cd $(BUILD_DIR) && "$$CL" --analyze -I../$(SRC_DIR) $(BASE_CFLAGS) $(addprefix ../,$(SRCS)) ; \
		echo "Analysis results saved in $(BUILD_DIR)/*.plist"; \
	else \
		echo "No clang-tidy or clang found for linting."; \
		exit 2; \
	fi

# -------- Convenience --------
clean:
	rm -rf $(BIN_DIR) $(BUILD_DIR) *.plist

# Assemble + run a program:
#   make run PROG=examples/hello.asm
run: release
	@if [ -z "$(PROG)" ]; then \
		echo "Usage: make run PROG=$(EX_DIR)/program.asm"; \
		exit 2; \
	fi
	$(BIN_DIR)/svm_asm "$(PROG)" program.zvm
	$(BIN_DIR)/svm_vm program.zvm

# Disassemble a .zvm:
#   make disasm ZVM=program.zvm
disasm: release
	@if [ -z "$(ZVM)" ]; then \
		echo "Usage: make disasm ZVM=path/to/program.zvm"; \
		exit 2; \
	fi
	$(BIN_DIR)/svm_disasm "$(ZVM)"

# Run golden output tests
test: release
	./run_test.sh

# Build browser (Emscripten) bundles
web:
	cd $(EX_DIR)/web && ./build_web.sh
