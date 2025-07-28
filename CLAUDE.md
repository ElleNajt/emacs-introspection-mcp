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

**`emacs_describe(symbol_name, type="symbol")`**
- Get comprehensive documentation for symbols
- `type`: "function", "variable", or "symbol" (both)
- Handles Lisp-2 namespace correctly
- Example: `emacs_describe("save-buffer", "function")`

**`emacs_search(pattern, type="all")`**  
- Search for symbols matching a pattern
- `type`: "all", "commands", "variables", "functions"
- Example: `emacs_search("buffer", "commands")`

**`emacs_buffer_info(buffer_name, include_content=true, include_variables=true)`**
- Complete buffer analysis with mode info and content
- Writes to `/tmp/ClaudeWorkingFolder/buffer_info_<buffer>.txt`
- Example: `emacs_buffer_info("*scratch*")`

**`emacs_keymap_analysis(buffer_name, include_global=false)`**
- Analyze keymaps for buffer context
- Shows major/minor mode keymaps
- Writes to `/tmp/ClaudeWorkingFolder/keymap_analysis_<buffer>.txt`

### Core Tools

**`get_variable_value(variable_name)`** - Current variable value (raw value only)
**`get_buffer_list(include_details=false)`** - Get list of all live buffers, optionally with detailed info

### Buffer Operations

**`view_buffer(buffer_name)`** - Get buffer contents with line numbers (outputs to file)
**`open_file(file_path)`** - Open file in Emacs, return buffer name
**`check_parens(file_path)`** - Validate parentheses balance in Lisp code by opening file fresh, reports location of syntax errors


### Org Mode

**`get_agenda(agenda_type="a")`** - Get org agenda view (outputs to file)

## Common Use Cases

### Exploring Unknown Codebase
```
1. emacs_buffer_info("main.py") → understand buffer context
2. emacs_search("def", "commands") → find definition-related commands  
3. emacs_keymap_analysis("main.py") → see available keybindings
```

### Understanding Emacs Functions
```
1. emacs_search("save", "functions") → find save-related functions
2. emacs_describe("save-buffer", "function") → get full documentation + keybindings
```

### Buffer Investigation
```
1. get_buffer_list() → see all open buffers
2. emacs_buffer_info("*Messages*") → analyze specific buffer
3. emacs_keymap_analysis("*Messages*") → see available key bindings
```

### Workflow Analysis
```
1. get_agenda() → see current org agenda
2. emacs_buffer_info("todo.org") → analyze org file structure
3. emacs_search("org-", "commands") → find org commands
```

### Debugging Elisp Syntax Errors
```
1. check_parens("/path/to/my-elisp-file.el") → validate parentheses and get error location
2. view_buffer("my-elisp-file.el") → examine file content around error line  
3. emacs_buffer_info("my-elisp-file.el") → check major mode and syntax settings
```

## Tips

- **Use meta-tools first** - They provide better context than individual functions
- **File outputs** - Large results go to `/tmp/ClaudeWorkingFolder/` for further analysis
- **Symbol types** - Remember Emacs is Lisp-2, symbols can be both functions and variables
- **Key sequences** - View in keymap analysis output using Emacs notation: "C-x" (Ctrl-x), "M-x" (Alt-x), "SPC" (space)
- **Buffer names** - Use exact buffer names including asterisks: "*scratch*", "*Messages*"
- **Elisp debugging** - Use `check_parens` with file path first for syntax errors; it opens file fresh and reports exact line/column location where errors occur

## Prerequisites

- Running Emacs with `server-start` 
- `emacsclient` available in PATH
- `/tmp/ClaudeWorkingFolder` added to Claude's `additionalDirectories` setting