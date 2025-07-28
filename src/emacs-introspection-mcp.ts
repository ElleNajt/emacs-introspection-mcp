#!/usr/bin/env node

import { FastMCP } from "fastmcp";
import { z } from "zod";
import { execFile } from "child_process";
import { writeFileSync, mkdirSync } from "fs";
import { join } from "path";

const mcp = new FastMCP({
    name: "emacs-introspection",
    version: "1.0.0",
});

// TODO[A5OhAyxaiJ] Overly restrictive? Not sufficient sanitization?
const isValidEmacsSymbol = (str: string) => /^[a-zA-Z0-9-_]+$/.test(str);



mcp.addTool({
    name: "get_variable_value",
    description: "Get the current value of an Emacs variable",
    parameters: z.object({
        variable_name: z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            execFile(
                "emacsclient",
                ["-e", `(symbol-value '${args.variable_name})`],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    resolve(stdout.trim());
                },
            );
        });
    },
});

mcp.addTool({
    name: "view_buffer",
    description: "Get the contents of a specific Emacs buffer and write it to /tmp/ClaudeWorkingFolder/<buffer_name>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the buffer content.",
    parameters: z.object({
        buffer_name: z.string().min(1, "Buffer name cannot be empty"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            execFile(
                "emacsclient",
                ["-e", `(with-current-buffer "${args.buffer_name}" (let ((content (buffer-string)) (lines (split-string (buffer-string) "\\n"))) (mapconcat (lambda (line) (format "%4d→%s" (1+ (cl-position line lines :test 'equal)) line)) lines "\\n")))`],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    
                    const content = stdout.trim();
                    
                    try {
                        // Ensure /tmp/ClaudeWorkingFolder directory exists
                        mkdirSync("/tmp/ClaudeWorkingFolder", { recursive: true });
                        
                        // Create filename from buffer name (sanitize special characters)
                        const sanitizedBufferName = args.buffer_name.replace(/[^a-zA-Z0-9-_]/g, "_");
                        const filename = join("/tmp/ClaudeWorkingFolder", `${sanitizedBufferName}.txt`);
                        
                        writeFileSync(filename, content, "utf8");
                        resolve(`Buffer content written to ${filename}`);
                    } catch (writeError) {
                        reject(new Error(`Failed to write file: ${writeError}`));
                    }
                },
            );
        });
    },
});

mcp.addTool({
    name: "get_agenda",
    description: "Get the org-agenda view and write it to /tmp/ClaudeWorkingFolder/agenda_<agenda_type>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the agenda content.",
    parameters: z.object({
        agenda_type: z.string().optional().default("a"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            // Ensure /tmp/ClaudeWorkingFolder directory exists
            mkdirSync("/tmp/ClaudeWorkingFolder", { recursive: true });
            
            // Create filename from agenda type
            const filename = join("/tmp/ClaudeWorkingFolder", `agenda_${args.agenda_type}.txt`);
            const absolutePath = filename;
            
            execFile(
                "emacsclient",
                ["-e", `(save-window-excursion (let ((org-agenda-window-setup 'current-window)) (org-agenda nil "${args.agenda_type}") (with-current-buffer "*Org Agenda*" (write-file "${absolutePath}"))))`],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    } else {
                        resolve(`Agenda content written to ${filename}`);
                    }
                },
            );
        });
    },
});

const isValidBufferName = (str: string) => str.length > 0 && str.length < 256;
const isValidSymbolName = (str: string) => /^[a-zA-Z0-9-_:]+$/.test(str);
const isValidFilePath = (str: string) => str.length > 0 && str.length < 1024;

mcp.addTool({
    name: "open_file",
    description: "Open a file in Emacs in the background (without switching to it) and return the buffer name",
    parameters: z.object({
        file_path: z.string().refine(isValidFilePath, "Invalid file path"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            const elisp = `(progn (find-file-noselect "${args.file_path}") (buffer-name (find-buffer-visiting "${args.file_path}")))`;
            
            execFile(
                "emacsclient",
                ["-e", elisp],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    } else {
                        const bufferName = stdout.trim().replace(/^"(.*)"$/, '$1');
                        resolve(`File opened in buffer: ${bufferName}`);
                    }
                },
            );
        });
    },
});


mcp.addTool({
    name: "emacs_search",
    description: "Search for Emacs symbols, commands, or variables matching a pattern using various apropos functions",
    parameters: z.object({
        pattern: z.string().min(1, "Pattern cannot be empty"),
        type: z.enum(["all", "commands", "variables", "functions"]).default("all"),
        predicate: z.string().optional().default(""),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            let elisp: string;
            
            if (args.type === "commands") {
                elisp = `(let ((symbols (apropos-internal "${args.pattern}" 'commandp))
                               (result "=== COMMANDS MATCHING '${args.pattern}' ===\\n"))
                           (dolist (sym symbols)
                             (when (commandp sym)
                               (setq result (concat result (symbol-name sym) "\\n"))))
                           result)`;
            } else if (args.type === "variables") {
                elisp = `(let ((symbols (apropos-internal "${args.pattern}"))
                               (result "=== VARIABLES MATCHING '${args.pattern}' ===\\n"))
                           (dolist (sym symbols)
                             (when (boundp sym)
                               (setq result (concat result (symbol-name sym) "\\n"))))
                           result)`;
            } else if (args.type === "functions") {
                elisp = `(let ((symbols (apropos-internal "${args.pattern}" 'fboundp))
                               (result "=== FUNCTIONS MATCHING '${args.pattern}' ===\\n"))
                           (dolist (sym symbols)
                             (when (fboundp sym)
                               (setq result (concat result (symbol-name sym) "\\n"))))
                           result)`;
            } else {
                // type === "all" - use apropos-internal with optional predicate
                const predicateArg = args.predicate ? ` '${args.predicate}` : "";
                elisp = `(let ((symbols (apropos-internal "${args.pattern}"${predicateArg}))
                               (result "=== ALL SYMBOLS MATCHING '${args.pattern}' ===\\n"))
                           (dolist (sym symbols)
                             (let ((types '()))
                               (when (fboundp sym) (push "function" types))
                               (when (boundp sym) (push "variable" types))
                               (when (commandp sym) (push "command" types))
                               (setq result (concat result (symbol-name sym)
                                                  (if types (concat " (" (mapconcat 'identity types ", ") ")") "")
                                                  "\\n"))))
                           result)`;
            }
            
            execFile(
                "emacsclient",
                ["-e", elisp],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    resolve(stdout.trim());
                },
            );
        });
    },
});




mcp.addTool({
    name: "emacs_describe",
    description: "Get comprehensive documentation for an Emacs symbol. For functions, includes key bindings. Handles Lisp-2 namespace by allowing explicit type specification.",
    parameters: z.object({
        symbol_name: z.string().refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
        type: z.enum(["function", "variable", "symbol"]).default("symbol"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            let elisp: string;
            
            if (args.type === "function") {
                elisp = `(let ((sym '${args.symbol_name}))
                           (if (fboundp sym)
                               (save-window-excursion
                                 (with-temp-buffer
                                   (describe-function sym)
                                   (with-current-buffer "*Help*"
                                     (buffer-string))))
                             "Symbol is not a function"))`;
            } else if (args.type === "variable") {
                elisp = `(let ((sym '${args.symbol_name}))
                           (if (boundp sym)
                               (save-window-excursion
                                 (with-temp-buffer
                                   (describe-variable sym)
                                   (with-current-buffer "*Help*"
                                     (buffer-string))))
                             "Symbol is not a variable"))`;
            } else {
                // type === "symbol" - get both function and variable info
                elisp = `(let ((sym '${args.symbol_name})
                               (result ""))
                           (when (fboundp sym)
                             (setq result (concat result "=== FUNCTION ===\\n"))
                             (setq result (concat result
                               (save-window-excursion
                                 (with-temp-buffer
                                   (describe-function sym)
                                   (with-current-buffer "*Help*"
                                     (buffer-string))))
                               "\\n\\n")))
                           (when (boundp sym)
                             (setq result (concat result "=== VARIABLE ===\\n"))
                             (setq result (concat result
                               (save-window-excursion
                                 (with-temp-buffer
                                   (describe-variable sym)
                                   (with-current-buffer "*Help*"
                                     (buffer-string)))))))
                           (if (string-empty-p result)
                               "Symbol not found as function or variable"
                             result))`;
            }
            
            execFile(
                "emacsclient",
                ["-e", elisp],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    resolve(stdout.trim());
                },
            );
        });
    },
});

mcp.addTool({
    name: "emacs_buffer_info",
    description: "Get comprehensive buffer information including content, mode details, and key variables. Writes content to /tmp/ClaudeWorkingFolder/buffer_info_<buffer_name>.txt",
    parameters: z.object({
        buffer_name: z.string().refine(isValidBufferName, "Invalid buffer name"),
        include_content: z.boolean().default(true),
        include_variables: z.boolean().default(true),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            try {
                // Ensure /tmp/ClaudeWorkingFolder directory exists
                mkdirSync("/tmp/ClaudeWorkingFolder", { recursive: true });
                
                // Create filename from buffer name
                const sanitizedBufferName = args.buffer_name.replace(/[^a-zA-Z0-9-_]/g, "_");
                const filename = join("/tmp/ClaudeWorkingFolder", `buffer_info_${sanitizedBufferName}.txt`);
                
                const elisp = `(with-current-buffer "${args.buffer_name}"
                    (let ((info ""))
                      (setq info (concat info "=== BUFFER INFO: " (buffer-name) " ===\\n"))
                      (setq info (concat info "File: " (or (buffer-file-name) "no file") "\\n"))
                      (setq info (concat info "Major mode: " (symbol-name major-mode) "\\n"))
                      (setq info (concat info "Minor modes: " (mapconcat 'symbol-name 
                                                                        (delq nil (mapcar (lambda (mode) 
                                                                                            (and (boundp mode) (symbol-value mode) mode))
                                                                                          minor-mode-list)) " ") "\\n"))
                      (setq info (concat info "Buffer size: " (number-to-string (buffer-size)) " characters\\n"))
                      (setq info (concat info "Modified: " (if (buffer-modified-p) "yes" "no") "\\n\\n"))
                      
                      ;; Mode description - use save-window-excursion to prevent buffer opening
                      (setq info (concat info "=== MODE DESCRIPTION ===\\n"))
                      (setq info (concat info 
                        (save-window-excursion
                          (with-temp-buffer
                            (describe-mode)
                            (with-current-buffer "*Help*"
                              (buffer-string)))) "\\n\\n"))
                      
                      ${args.include_variables ? 
                        `(setq info (concat info "=== KEY VARIABLES ===\\n"))
                         (setq info (concat info "default-directory: " default-directory "\\n"))
                         (setq info (concat info "tab-width: " (number-to-string tab-width) "\\n"))
                         (setq info (concat info "fill-column: " (number-to-string fill-column) "\\n"))
                         (setq info (concat info "buffer-read-only: " (if buffer-read-only "yes" "no") "\\n\\n"))` : 
                        ""}
                      
                      ${args.include_content ? 
                        `(setq info (concat info "=== BUFFER CONTENT ===\\n"))
                         (let ((content (buffer-string)) (lines (split-string (buffer-string) "\\\\n")))
                           (setq info (concat info (mapconcat (lambda (line) 
                                                               (format "%4d→%s" (1+ (cl-position line lines :test 'equal)) line)) 
                                                             lines "\\\\n"))))` : 
                        ""}
                      
                      (write-region info nil "${filename}")
                      (format "Buffer info written to %s" "${filename}")))`;
                
                execFile(
                    "emacsclient",
                    ["-e", elisp],
                    { encoding: "utf8", shell: false },
                    (error, stdout, stderr) => {
                        if (error) {
                            reject(error);
                        } else {
                            resolve(stdout.trim());
                        }
                    },
                );
            } catch (writeError) {
                reject(new Error(`Failed to prepare buffer info: ${writeError}`));
            }
        });
    },
});

mcp.addTool({
    name: "check_parens",
    description: "Run check-parens on a file by opening it fresh in Emacs to validate parentheses balance in Lisp code",
    parameters: z.object({
        file_path: z.string().refine(isValidFilePath, "Invalid file path"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            const elisp = `(let ((temp-buffer (find-file-noselect "${args.file_path}")))
                (unwind-protect
                    (with-current-buffer temp-buffer
                        (condition-case err
                            (progn
                                (check-parens)
                                "Parentheses are balanced correctly")
                            (error 
                                (format "Parentheses error at line %d, column %d: %s" 
                                        (line-number-at-pos (point))
                                        (current-column)
                                        (error-message-string err)))))
                    (kill-buffer temp-buffer)))`;
            
            execFile(
                "emacsclient",
                ["-e", elisp],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    } else {
                        resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                    }
                },
            );
        });
    },
});

mcp.addTool({
    name: "get_buffer_list",
    description: "Get a list of all live buffers in Emacs. Returns buffer names with their associated files (if any).",
    parameters: z.object({
        include_details: z.boolean().default(false).describe("Include buffer details like file, size, and modification status"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            let elisp: string;
            
            if (args.include_details) {
                elisp = `(let ((buffers (buffer-list))
                              (result "=== BUFFER LIST WITH DETAILS ===\\n"))
                          (dolist (buf buffers)
                            (with-current-buffer buf
                              (setq result (concat result
                                (format "Buffer: %s\\n" (buffer-name))
                                (format "  File: %s\\n" (or (buffer-file-name) "no file"))
                                (format "  Size: %d characters\\n" (buffer-size))
                                (format "  Modified: %s\\n" (if (buffer-modified-p) "yes" "no"))
                                (format "  Major mode: %s\\n\\n" (symbol-name major-mode))))))
                          result)`;
            } else {
                elisp = `(let ((buffers (buffer-list))
                              (result "=== BUFFER LIST ===\\n"))
                          (dolist (buf buffers)
                            (with-current-buffer buf
                              (setq result (concat result
                                (format "%s%s\\n"
                                  (buffer-name)
                                  (if (buffer-file-name)
                                    (format " (%s)" (buffer-file-name))
                                    ""))))))
                          result)`;
            }
            
            execFile(
                "emacsclient",
                ["-e", elisp],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    resolve(stdout.trim());
                },
            );
        });
    },
});

mcp.addTool({
    name: "emacs_keymap_analysis",
    description: "Analyze keymaps for a buffer context and write to /tmp/ClaudeWorkingFolder/keymap_analysis_<buffer_name>.txt. Shows major mode keymap, minor mode keymaps, and local bindings.",
    parameters: z.object({
        buffer_name: z.string().refine(isValidBufferName, "Invalid buffer name"),
        include_global: z.boolean().default(false),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            try {
                // Ensure /tmp/ClaudeWorkingFolder directory exists
                mkdirSync("/tmp/ClaudeWorkingFolder", { recursive: true });
                
                // Create filename from buffer name
                const sanitizedBufferName = args.buffer_name.replace(/[^a-zA-Z0-9-_]/g, "_");
                const filename = join("/tmp/ClaudeWorkingFolder", `keymap_analysis_${sanitizedBufferName}.txt`);
                
                const elisp = `(with-current-buffer "${args.buffer_name}"
                    (let ((analysis "")
                          (major-keymap (current-local-map))
                          (minor-keymaps (current-minor-mode-maps)))
                      (setq analysis (concat analysis "=== BUFFER: " (buffer-name) " ===\\n"))
                      (setq analysis (concat analysis "Major mode: " (symbol-name major-mode) "\\n\\n"))
                      
                      ;; Major mode keymap
                      (when major-keymap
                        (setq analysis (concat analysis "=== MAJOR MODE KEYMAP ===\\n"))
                        (setq analysis (concat analysis (save-window-excursion (substitute-command-keys "\\\\{major-keymap}")) "\\n\\n")))
                      
                      ;; Minor mode keymaps
                      (when minor-keymaps
                        (setq analysis (concat analysis "=== MINOR MODE KEYMAPS ===\\n"))
                        (let ((keymap-index 0))
                          (dolist (keymap minor-keymaps)
                            (when keymap
                              (setq keymap-index (1+ keymap-index))
                              (setq analysis (concat analysis (format "Minor mode keymap %d:\\n" keymap-index)))
                              (condition-case err
                                (let ((keymap-desc (substitute-command-keys (format "\\\\{%s}" keymap))))
                                  (setq analysis (concat analysis keymap-desc "\\n\\n")))
                                (error 
                                 (setq analysis (concat analysis "Error describing keymap: " (error-message-string err) "\\n\\n"))))))))
                      
                      ${args.include_global ? 
                        `(setq analysis (concat analysis "=== GLOBAL KEYMAP ===\\n"))
                         (setq analysis (concat analysis (save-window-excursion (substitute-command-keys "\\\\{global-map}")) "\\n\\n"))` : 
                        ""}
                      
                      (write-region analysis nil "${filename}")
                      (format "Keymap analysis written to %s" "${filename}")))`;
                
                execFile(
                    "emacsclient",
                    ["-e", elisp],
                    { encoding: "utf8", shell: false },
                    (error, stdout, stderr) => {
                        if (error) {
                            reject(error);
                        } else {
                            resolve(stdout.trim());
                        }
                    },
                );
            } catch (writeError) {
                reject(new Error(`Failed to prepare keymap analysis: ${writeError}`));
            }
        });
    },
});

mcp.start({
    transportType: "stdio",
});
