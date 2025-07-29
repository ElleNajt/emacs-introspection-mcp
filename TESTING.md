# Testing Instructions for Emacs Introspection MCP

This document provides comprehensive testing instructions for the Emacs Introspection MCP server.

## Prerequisites

1. **Running Emacs with server mode**:
   ```elisp
   (server-start)
   ```
   Or add `(server-start)` to your Emacs init file.

2. **emacsclient available in PATH**:
   ```bash
   which emacsclient
   # Should return a path like /usr/local/bin/emacsclient
   ```

3. **Claude Code with MCP server configured**:
   - Server should be installed and configured in Claude Code
   - `/tmp/ClaudeWorkingFolder` should be added to `additionalDirectories` in settings

## Basic Connection Tests

### Test 1: Variable Retrieval
```
Test command: mcp__emacs-introspection__get_variable_value
Parameters: variable_name = "emacs-version"
Expected: Returns your Emacs version (e.g., "30.1")
```

### Test 2: Default Directory
```
Test command: mcp__emacs-introspection__get_variable_value  
Parameters: variable_name = "default-directory"
Expected: Returns current working directory path
```

### Test 3: Invalid Variable
```
Test command: mcp__emacs-introspection__get_variable_value
Parameters: variable_name = "nonexistent-variable"
Expected: Error about void variable
```

## Buffer Operations Tests

### Test 4: View Scratch Buffer
```
Test command: mcp__emacs-introspection__view_buffer
Parameters: buffer_name = "*scratch*"
Expected: 
- Creates file /tmp/ClaudeWorkingFolder/_scratch_.txt
- Contains numbered lines with buffer content
- Returns success message with file path
```

### Test 5: Open File in Background
```
Test command: mcp__emacs-introspection__open_file
Parameters: file_path = "/path/to/existing/file.txt"
Expected:
- File opens in Emacs but doesn't switch to it in current window
- Returns "File opened in buffer: filename.txt"
- Verify in Emacs that file is open but current window unchanged
```

### Test 6: Open Non-existent File
```
Test command: mcp__emacs-introspection__open_file
Parameters: file_path = "/path/to/nonexistent/file.txt"
Expected: Creates new buffer for the file path
```

## Symbol Search and Documentation Tests

### Test 7: Search for Commands
```
Test command: mcp__emacs-introspection__emacs_search
Parameters: pattern = "save", type = "commands"
Expected:
- Returns list of commands containing "save"
- Should include save-buffer, save-some-buffers, etc.
```

### Test 8: Search for Functions
```
Test command: mcp__emacs-introspection__emacs_search
Parameters: pattern = "buffer", type = "functions"
Expected: Returns functions with "buffer" in name
```

### Test 9: Search All Symbols
```
Test command: mcp__emacs-introspection__emacs_search
Parameters: pattern = "file", type = "all"
Expected: Returns functions, variables, and commands with "file"
```

### Test 10: Describe Function
```
Test command: mcp__emacs-introspection__emacs_describe
Parameters: symbol_name = "save-buffer", type = "function"
Expected:
- Comprehensive documentation for save-buffer
- Includes key bindings (C-x C-s)
- Shows function signature and description
```

### Test 11: Describe Variable
```
Test command: mcp__emacs-introspection__emacs_describe
Parameters: symbol_name = "fill-column", type = "variable"
Expected: Documentation for fill-column variable
```

### Test 12: Describe Symbol (Both)
```
Test command: mcp__emacs-introspection__emacs_describe
Parameters: symbol_name = "buffer-list", type = "symbol"
Expected: Shows both function and variable info if symbol serves both roles
```

## Meta-Tools Tests

### Test 13: Buffer Info Analysis
```
Test command: mcp__emacs-introspection__emacs_buffer_info
Parameters: buffer_name = "*scratch*"
Expected:
- Creates /tmp/ClaudeWorkingFolder/buffer_info__scratch_.txt
- Contains: buffer name, file, major mode, minor modes, size, modification status
- Includes mode descriptions
- Shows key variables (default-directory, tab-width, etc.)
- Includes buffer content with line numbers
```

### Test 14: Buffer Info Without Content
```
Test command: mcp__emacs-introspection__emacs_buffer_info
Parameters: buffer_name = "*scratch*", include_content = false
Expected: Buffer info without content section
```

### Test 15: Keymap Analysis
```
Test command: mcp__emacs-introspection__emacs_keymap_analysis
Parameters: buffer_name = "*scratch*"
Expected:
- Creates /tmp/ClaudeWorkingFolder/keymap_analysis__scratch_.txt
- Shows major mode keymap
- Lists minor mode keymaps
- Does not include global keymap (include_global = false by default)
```

### Test 16: Keymap Analysis with Global
```
Test command: mcp__emacs-introspection__emacs_keymap_analysis
Parameters: buffer_name = "*scratch*", include_global = true
Expected: Includes global keymap section
```

## Parentheses Checking Tests

### Test 17: Check Balanced Parens
Create a test elisp buffer with balanced parentheses:
```
Test command: mcp__emacs-introspection__check_parens
Parameters: buffer_name = "test.el" (with balanced parens)
Expected: "Parentheses are balanced correctly"
```

### Test 18: Check Unbalanced Parens
Create a test elisp buffer with unbalanced parentheses:
```elisp
(defun test-function ()
  (message "missing closing paren"
```
```
Test command: mcp__emacs-introspection__check_parens
Parameters: buffer_name = "test.el" (with unbalanced parens)
Expected: Error message indicating parentheses problem
```

## Org Mode Tests

### Test 19: Get Agenda
```
Test command: mcp__emacs-introspection__get_agenda
Parameters: agenda_type = "a"
Expected:
- Creates /tmp/ClaudeWorkingFolder/agenda_a.txt
- Contains current org agenda view
- Shows scheduled items, deadlines, etc.
```

### Test 20: Get Weekly Agenda
```
Test command: mcp__emacs-introspection__get_agenda
Parameters: agenda_type = "w"
Expected: Weekly agenda view
```

## Error Handling Tests

### Test 21: Invalid Buffer Name
```
Test command: mcp__emacs-introspection__view_buffer
Parameters: buffer_name = "nonexistent-buffer"
Expected: Error about buffer not existing
```

### Test 22: Invalid Symbol Name
```
Test command: mcp__emacs-introspection__emacs_describe
Parameters: symbol_name = "invalid-symbol-name!!!"
Expected: Error about invalid symbol name format
```

### Test 23: Long File Path
```
Test command: mcp__emacs-introspection__open_file
Parameters: file_path = "/very/long/path/that/exceeds/limits/..." (over 1024 chars)
Expected: Validation error about invalid file path
```

## Integration Tests

### Test 24: Full Workflow Test
1. Open a file in background
2. Get buffer info for that file
3. Analyze keymaps for the buffer
4. Search for relevant symbols
5. Describe a symbol used in the file
6. Check parentheses (if elisp file)

### Test 25: Multiple Buffer Test
1. Open several files in background
2. View different buffers
3. Get buffer info for each
4. Verify files opened without switching windows

## File Output Verification

After each test that creates files in `/tmp/ClaudeWorkingFolder/`:

1. **Verify file exists**: `ls /tmp/ClaudeWorkingFolder/`
2. **Check file contents**: Use Read tool or `cat` to verify content
3. **Verify file permissions**: Files should be readable by Claude Code

## Performance Tests

### Test 26: Large Buffer
Test with a large file (>10MB) to ensure reasonable performance.

### Test 27: Many Symbols Search
Search for very common patterns (e.g., "e") to test with large result sets.

## Cleanup

After testing:
```bash
rm -rf /tmp/ClaudeWorkingFolder/*
```

## Common Issues and Troubleshooting

1. **"Command failed: emacsclient"**: Ensure Emacs server is running
2. **File permission errors**: Check `/tmp/ClaudeWorkingFolder` permissions
3. **Empty buffer content**: Buffer might be genuinely empty
4. **Symbol not found**: Symbol might not exist or be misspelled
5. **Keymap errors**: Some keymaps might not be describable

## Security Attack Tests

**⚠️ ONLY test these in isolated environments!**

### Test A1: Elisp Code Injection via Symbol
```
Tool: emacs_describe
Payload: symbol_name = 'save-buffer"); (shell-command "touch /tmp/pwned"); ("dummy'
Expected: Should be rejected by sanitization or safely escaped
```

### Test A2: String Escape Injection
```
Tool: emacs_search
Payload: pattern = 'test\"); (delete-file \"/tmp/important.txt\"); (\"dummy'
Expected: Should be blocked by input validation
```

### Test A3: Buffer Name Injection
```
Tool: emacs_buffer_info
Payload: buffer_name = '*scratch*"); (write-region "attack" nil "/tmp/evil.txt"); ("dummy'
Expected: Should reject malicious buffer name
```

### Test A4: Path Traversal via Symlink
```bash
# Setup
ln -s /etc/passwd /tmp/ClaudeWorkingFolder/passwd_link

# Test
Tool: open_file
Payload: file_path = "passwd_link"
Expected: Should reject or safely handle symlink
```

### Test A5: Long Input DoS
```
Tool: emacs_search
Payload: pattern = "a" * 10000  # Very long string
Expected: Should reject with length validation error
```

## Security Test Results
After testing, verify:
- [ ] All injection attempts are blocked
- [ ] Error messages don't reveal internal details
- [ ] No files created outside allowed directories
- [ ] Emacs remains stable and responsive
- [ ] No shell commands executed

## Expected Performance

- Simple variable retrieval: < 100ms
- Buffer operations: < 500ms
- Symbol searches: < 1s for common patterns
- Buffer info generation: < 2s for large buffers
- File operations should not affect current Emacs window focus