# Config
PYTHON      := python3
NUITKA      := $(PYTHON) -m nuitka
MAIN        := elkeep-cli.py
OUTPUT      := $(basename $(MAIN))

# Nuitka options (tweak as needed)
NUITKA_FLAGS := --standalone --onefile --remove-output

# Default target
all: build

# Build the binary
build:
	$(NUITKA) $(NUITKA_FLAGS) $(MAIN)

# Run the compiled binary
run: build
	./$(OUTPUT).bin

# Clean Nuitka output
clean:
	rm -rf $(OUTPUT).dist $(OUTPUT).build $(OUTPUT).bin __pycache__

.PHONY: all build run clean
