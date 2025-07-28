# Emacs Introspection MCP Tools

## Quick Start

This MCP server provides tools to introspect and interact with your running Emacs instance via `emacsclient`. 

## Core Workflow

1. **Buffer Analysis**: Use `emacs_buffer_info` to get comprehensive buffer information
2. **Symbol Lookup**: Use `emacs_describe` to understand functions/variables  
3. **Search & Discovery**: Use `emacs_search` to find relevant symbols
4. **Keymap Exploration**: Use `emacs_keymap_analysis` for key binding context

## Tool Reference

### Meta-Tools (Recommended)

**`emacs_describe(symbol_names, type="symbol")`**
- Get comprehensive documentation for one or more symbols
- `symbol_names`: array of symbol names to describe
- `type`: "function", "variable", or "symbol" (both)
- Handles Lisp-2 namespace correctly
- Example: `emacs_describe(["save-buffer", "kill-buffer"], "function")`

**`emacs_search(pattern, type="all")`**  
- Search for symbols matching a pattern
- `type`: "all", "commands", "variables", "functions"
- Example: `emacs_search("buffer", "commands")`

**`emacs_buffer_info(buffer_names, include_content=true, include_variables=true)`**
- Complete buffer analysis with mode info and content for one or more buffers
- `buffer_names`: array of buffer names to analyze
- Writes to `/tmp/ClaudeWorkingFolder/buffer_info_<buffer>.txt` for each buffer
- Example: `emacs_buffer_info(["*scratch*", "*Messages*"])`

**`emacs_keymap_analysis(buffer_names, include_global=false)`**
- Analyze keymaps for one or more buffer contexts
- `buffer_names`: array of buffer names to analyze
- Shows major/minor mode keymaps
- Writes to `/tmp/ClaudeWorkingFolder/keymap_analysis_<buffer>.txt` for each buffer

### Core Tools

**`get_variable_value(variable_names)`** - Current values of one or more variables
**`get_buffer_list(include_details=false)`** - Get list of all live buffers, optionally with detailed info

### Buffer Operations

**`view_buffer(buffer_names)`** - Get contents of one or more buffers, each written to separate file with line numbers
**`open_file(file_paths)`** - Open one or more files in Emacs, return buffer names (restricted to current working directory and /tmp/ClaudeWorkingFolder, relative paths resolved from current working directory)
**`check_parens(file_paths)`** - Validate parentheses balance in one or more Lisp files, reports location of syntax errors (restricted to current working directory and /tmp/ClaudeWorkingFolder, relative paths resolved from current working directory)


### Org Mode

**`get_agenda(agenda_type="a")`** - Get org agenda view (outputs to file)

## Common Use Cases

### Exploring Unknown Codebase
```
1. emacs_buffer_info(["main.py"]) → understand buffer context
2. emacs_search("def", "commands") → find definition-related commands  
3. emacs_keymap_analysis(["main.py"]) → see available keybindings
```

### Understanding Emacs Functions
```
1. emacs_search("save", "functions") → find save-related functions
2. emacs_describe(["save-buffer", "save-some-buffers"], "function") → get full documentation + keybindings
```

### Buffer Investigation
```
1. get_buffer_list() → see all open buffers
2. emacs_buffer_info(["*Messages*", "*scratch*"]) → analyze multiple buffers
3. emacs_keymap_analysis(["*Messages*"]) → see available key bindings
4. view_buffer(["*scratch*", "*Messages*", "main.py"]) → get content of multiple buffers
```

### Workflow Analysis
```
1. get_agenda() → see current org agenda  
2. emacs_buffer_info(["todo.org"]) → analyze org file structure
3. emacs_search("org-", "commands") → find org commands
```

### Debugging Elisp Syntax Errors
```
1. check_parens(["/path/to/my-elisp-file.el", "/path/to/other.el"]) → validate parentheses and get error locations
2. view_buffer(["my-elisp-file.el"]) → examine file content around error line  
3. emacs_buffer_info(["my-elisp-file.el"]) → check major mode and syntax settings
```

## Tips

- **Use meta-tools first** - They provide better context than individual functions
- **File outputs** - Large results go to `/tmp/ClaudeWorkingFolder/` for further analysis
- **Symbol types** - Remember Emacs is Lisp-2, symbols can be both functions and variables
- **Key sequences** - View in keymap analysis output using Emacs notation: "C-x" (Ctrl-x), "M-x" (Alt-x), "SPC" (space)
- **Buffer names** - Use exact buffer names including asterisks: "*scratch*", "*Messages*"
- **File paths** - `open_file` and `check_parens` only accept paths within current working directory or `/tmp/ClaudeWorkingFolder`, relative paths resolved from current directory
- **Elisp debugging** - Use `check_parens` with file path first for syntax errors; it opens file fresh and reports exact line/column location where errors occur

## Prerequisites

- Running Emacs with `server-start` 
- `emacsclient` available in PATH
- `/tmp/ClaudeWorkingFolder` added to Claude's `additionalDirectories` setting