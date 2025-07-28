#!/usr/bin/env node
"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const fastmcp_1 = require("fastmcp");
const zod_1 = require("zod");
const child_process_1 = require("child_process");
const fs_1 = require("fs");
const path_1 = require("path");
const mcp = new fastmcp_1.FastMCP({
    name: "emacs-introspection",
    version: "1.0.0",
});
// Allowed directories for file operations
const ALLOWED_DIRS = [
    (0, path_1.resolve)(process.cwd()), // Current working directory
    (0, path_1.resolve)("/tmp/ClaudeWorkingFolder"), // Claude's working folder
];
// Validate file path is within allowed directories and prevent path traversal
const isValidFilePath = (filePath) => {
    if (!filePath || filePath.length === 0 || filePath.length > 1024) {
        return false;
    }
    // Prevent path traversal attacks
    if (filePath.includes("../") || filePath.includes("..\\")) {
        return false;
    }
    try {
        const resolvedPath = (0, path_1.resolve)(filePath);
        return ALLOWED_DIRS.some(allowedDir => {
            const relativePath = (0, path_1.relative)(allowedDir, resolvedPath);
            return !relativePath || (!relativePath.startsWith("..") && !relativePath.startsWith("/"));
        });
    }
    catch (_a) {
        return false;
    }
};
// TODO[A5OhAyxaiJ] Overly restrictive? Not sufficient sanitization?
const isValidEmacsSymbol = (str) => {
    // Allow common Emacs symbol characters including those needed for Doom workspaces
    // Specifically: letters, numbers, hyphens, underscores, forward slashes, plus signs, colons
    return /^[a-zA-Z0-9\-_/+:]+$/.test(str) && str.length > 0 && str.length < 100;
};
mcp.addTool({
    name: "get_variable_value",
    description: "Get the current value of one or more Emacs variables",
    parameters: zod_1.z.object({
        variable_names: zod_1.z
            .array(zod_1.z.string().refine(isValidEmacsSymbol, "Invalid Emacs symbol name"))
            .min(1, "Must provide at least one variable name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const variableQueries = args.variable_names.map(varName => `(format "%s: %s" "${varName}" (condition-case err (symbol-value '${varName}) (error (format "Error: %s" (error-message-string err)))))`).join(" \"\\n\" ");
            const elisp = `(concat ${variableQueries})`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "get_agenda",
    description: "Get the org-agenda view and write it to /tmp/ClaudeWorkingFolder/agenda_<agenda_type>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the agenda content.",
    parameters: zod_1.z.object({
        agenda_type: zod_1.z.string().optional().default("a"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            // Ensure /tmp/ClaudeWorkingFolder directory exists
            (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
            // Create filename from agenda type
            const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `agenda_${args.agenda_type}.txt`);
            const absolutePath = filename;
            (0, child_process_1.execFile)("emacsclient", ["-e", `(save-window-excursion (let ((org-agenda-window-setup 'current-window)) (org-agenda nil "${args.agenda_type}") (with-current-buffer "*Org Agenda*" (write-file "${absolutePath}"))))`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(`Agenda content written to ${filename}`);
                }
            });
        });
    }),
});
const isValidBufferName = (str) => str.length > 0 && str.length < 256;
const isValidSymbolName = (str) => /^[a-zA-Z0-9-_:]+$/.test(str);
mcp.addTool({
    name: "open_file",
    description: "Open one or more files in Emacs in the background (without switching to them) and return the buffer names. Files must be within the current working directory or /tmp/ClaudeWorkingFolder. Relative paths are resolved from the current working directory.",
    parameters: zod_1.z.object({
        file_paths: zod_1.z.array(zod_1.z.string().refine(isValidFilePath, "Invalid or restricted file path")).min(1, "Must provide at least one file path"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const fileOperations = args.file_paths.map(filePath => `(let ((buffer (find-file-noselect "${filePath}")))
                   (format "%s -> %s" "${filePath}" (buffer-name buffer)))`).join(" \"\\n\" ");
            const elisp = `(concat ${fileOperations})`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(`Files opened:\n${stdout.trim()}`);
                }
            });
        });
    }),
});
mcp.addTool({
    name: "emacs_search",
    description: "Search for Emacs symbols, commands, or variables matching a pattern using various apropos functions",
    parameters: zod_1.z.object({
        pattern: zod_1.z.string().min(1, "Pattern cannot be empty"),
        type: zod_1.z.enum(["all", "commands", "variables", "functions"]).default("all"),
        predicate: zod_1.z.string().optional().default(""),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            let elisp;
            if (args.type === "commands") {
                elisp = `(let ((symbols (apropos-internal "${args.pattern}" 'commandp))
                               (result "=== COMMANDS MATCHING '${args.pattern}' ===\\n"))
                           (dolist (sym symbols)
                             (when (commandp sym)
                               (setq result (concat result (symbol-name sym) "\\n"))))
                           result)`;
            }
            else if (args.type === "variables") {
                elisp = `(let ((symbols (apropos-internal "${args.pattern}"))
                               (result "=== VARIABLES MATCHING '${args.pattern}' ===\\n"))
                           (dolist (sym symbols)
                             (when (boundp sym)
                               (setq result (concat result (symbol-name sym) "\\n"))))
                           result)`;
            }
            else if (args.type === "functions") {
                elisp = `(let ((symbols (apropos-internal "${args.pattern}" 'fboundp))
                               (result "=== FUNCTIONS MATCHING '${args.pattern}' ===\\n"))
                           (dolist (sym symbols)
                             (when (fboundp sym)
                               (setq result (concat result (symbol-name sym) "\\n"))))
                           result)`;
            }
            else {
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
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "emacs_describe",
    description: "Get comprehensive documentation for one or more Emacs symbols. For functions, includes key bindings. Handles Lisp-2 namespace by allowing explicit type specification.",
    parameters: zod_1.z.object({
        symbol_names: zod_1.z.array(zod_1.z.string().refine(isValidEmacsSymbol, "Invalid Emacs symbol name")).min(1, "Must provide at least one symbol name"),
        type: zod_1.z.enum(["function", "variable", "symbol"]).default("symbol"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const symbolDescriptions = args.symbol_names.map((symbolName, index) => {
                if (args.type === "function") {
                    return `(let ((sym '${symbolName}))
                             (concat "=== SYMBOL: " (symbol-name sym) " ===\\n"
                               (if (fboundp sym)
                                 (save-window-excursion
                                   (with-temp-buffer
                                     (describe-function sym)
                                     (with-current-buffer "*Help*"
                                       (buffer-string))))
                                 "Symbol is not a function")))`;
                }
                else if (args.type === "variable") {
                    return `(let ((sym '${symbolName}))
                             (concat "=== SYMBOL: " (symbol-name sym) " ===\\n"
                               (if (boundp sym)
                                 (save-window-excursion
                                   (with-temp-buffer
                                     (describe-variable sym)
                                     (with-current-buffer "*Help*"
                                       (buffer-string))))
                                 "Symbol is not a variable")))`;
                }
                else {
                    // type === "symbol" - get both function and variable info
                    return `(let ((sym '${symbolName})
                                   (result ""))
                             (setq result (concat result "=== SYMBOL: " (symbol-name sym) " ===\\n"))
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
                             (if (and (not (fboundp sym)) (not (boundp sym)))
                                 (concat result "Symbol not found as function or variable")
                               result))`;
                }
            });
            const elisp = `(concat ${symbolDescriptions.join(' "\\n\\n" ')})`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "emacs_buffer_info",
    description: "Get comprehensive buffer information including content, mode details, and key variables. Writes content to /tmp/ClaudeWorkingFolder/buffer_info_<buffer_name>.txt for each buffer.",
    parameters: zod_1.z.object({
        buffer_names: zod_1.z.array(zod_1.z.string().refine(isValidBufferName, "Invalid buffer name")).min(1, "Must provide at least one buffer name"),
        include_content: zod_1.z.boolean().default(true),
        include_variables: zod_1.z.boolean().default(true),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            try {
                // Ensure /tmp/ClaudeWorkingFolder directory exists
                (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
                const bufferProcessing = args.buffer_names.map((bufferName) => {
                    const sanitizedBufferName = bufferName.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `buffer_info_${sanitizedBufferName}.txt`);
                    return `(condition-case err
                        (with-current-buffer "${bufferName}"
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
                            "${filename}"))
                        (error 
                          (format "Error processing buffer '${bufferName}': %s" (error-message-string err))))`;
                }).join("\n        ");
                const elisp = `(let ((successful-files '()))
                    ${bufferProcessing}
                    ${args.buffer_names.map((bufferName) => {
                    const sanitizedBufferName = bufferName.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `buffer_info_${sanitizedBufferName}.txt`);
                    return `(when (file-exists-p "${filename}") 
                                  (push "${filename}" successful-files))`;
                }).join("\n                    ")}
                    (format "Buffer info written to files: %s" (mapconcat 'identity (reverse successful-files) ", ")))`;
                (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    else {
                        resolve(stdout.trim());
                    }
                });
            }
            catch (writeError) {
                reject(new Error(`Failed to prepare buffer info: ${writeError}`));
            }
        });
    }),
});
mcp.addTool({
    name: "check_parens",
    description: "Run check-parens on one or more files by opening them fresh in Emacs to validate parentheses balance in Lisp code. Files must be within the current working directory or /tmp/ClaudeWorkingFolder. Relative paths are resolved from the current working directory.",
    parameters: zod_1.z.object({
        file_paths: zod_1.z.array(zod_1.z.string().refine(isValidFilePath, "Invalid or restricted file path")).min(1, "Must provide at least one file path"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const fileChecks = args.file_paths.map(filePath => `(let ((temp-buffer (find-file-noselect "${filePath}")))
                   (unwind-protect
                       (with-current-buffer temp-buffer
                           (condition-case err
                               (progn
                                   (check-parens)
                                   (format "%s: Parentheses are balanced correctly" "${filePath}"))
                               (error 
                                   (format "%s: Parentheses error at line %d, column %d: %s" 
                                           "${filePath}"
                                           (line-number-at-pos (point))
                                           (current-column)
                                           (error-message-string err)))))
                       (kill-buffer temp-buffer)))`).join(" \"\\n\" ");
            const elisp = `(concat ${fileChecks})`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "get_buffer_list",
    description: "Get a list of all live buffers in Emacs. Returns buffer names with their associated files (if any).",
    parameters: zod_1.z.object({
        include_details: zod_1.z.boolean().default(false).describe("Include buffer details like file, size, and modification status"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            let elisp;
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
            }
            else {
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
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "emacs_keymap_analysis",
    description: "Analyze keymaps for one or more buffer contexts and write to /tmp/ClaudeWorkingFolder/keymap_analysis_<buffer_name>.txt. Shows major mode keymap, minor mode keymaps, and local bindings.",
    parameters: zod_1.z.object({
        buffer_names: zod_1.z.array(zod_1.z.string().refine(isValidBufferName, "Invalid buffer name")).min(1, "Must provide at least one buffer name"),
        include_global: zod_1.z.boolean().default(false),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            try {
                // Ensure /tmp/ClaudeWorkingFolder directory exists
                (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
                const bufferProcessing = args.buffer_names.map((bufferName) => {
                    const sanitizedBufferName = bufferName.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `keymap_analysis_${sanitizedBufferName}.txt`);
                    return `(condition-case err
                        (with-current-buffer "${bufferName}"
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
                            "${filename}"))
                        (error 
                          (format "Error processing buffer '${bufferName}': %s" (error-message-string err))))`;
                }).join("\n        ");
                const elisp = `(let ((successful-files '()))
                    ${bufferProcessing}
                    ${args.buffer_names.map((bufferName) => {
                    const sanitizedBufferName = bufferName.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `keymap_analysis_${sanitizedBufferName}.txt`);
                    return `(when (file-exists-p "${filename}") 
                                  (push "${filename}" successful-files))`;
                }).join("\n                    ")}
                    (format "Keymap analysis written to files: %s" (mapconcat 'identity (reverse successful-files) ", ")))`;
                (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    else {
                        resolve(stdout.trim());
                    }
                });
            }
            catch (writeError) {
                reject(new Error(`Failed to prepare keymap analysis: ${writeError}`));
            }
        });
    }),
});
mcp.addTool({
    name: "org_agenda_todo",
    description: "Change the state of an agenda item (e.g., TODO -> DONE). Can target items by line number in agenda buffer or by heading text in org files.",
    parameters: zod_1.z.object({
        target_type: zod_1.z.enum(["agenda_line", "org_heading"]).describe("Whether to target by agenda line number or org file heading"),
        target: zod_1.z.string().describe("Either agenda line number (1-based) or heading text to search for"),
        new_state: zod_1.z.string().optional().describe("New TODO state (if not provided, cycles through states)"),
        agenda_type: zod_1.z.string().optional().default("a").describe("Agenda type to work with (default 'a')"),
        org_file: zod_1.z.string().optional().describe("Specific org file path (required for org_heading type)"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            let elisp;
            if (args.target_type === "agenda_line") {
                // Work with agenda buffer directly
                const stateChange = args.new_state ? `"${args.new_state}"` : "nil";
                elisp = `(save-window-excursion
                    (let ((org-agenda-window-setup 'current-window))
                      (org-agenda nil "${args.agenda_type}")
                      (with-current-buffer "*Org Agenda*"
                        (goto-char (point-min))
                        (forward-line (1- ${parseInt(args.target)}))
                        (if (org-agenda-check-type nil 'agenda 'todo 'tags 'search)
                            (progn
                              (org-agenda-todo ${stateChange})
                              (format "Successfully changed state of item at line %s" "${args.target}"))
                          (error "No valid agenda item found at line %s" "${args.target}")))))`;
            }
            else {
                // Work with org file directly
                if (!args.org_file) {
                    reject(new Error("org_file parameter is required when target_type is 'org_heading'"));
                    return;
                }
                if (!isValidFilePath(args.org_file)) {
                    reject(new Error("Invalid or restricted file path"));
                    return;
                }
                const stateChange = args.new_state ? `"${args.new_state}"` : "nil";
                elisp = `(save-window-excursion
                    (find-file "${args.org_file}")
                    (goto-char (point-min))
                    (if (search-forward "${args.target}" nil t)
                        (progn
                          (org-back-to-heading t)
                          (org-todo ${stateChange})
                          (save-buffer)
                          (format "Successfully changed state of heading '%s' in %s" "${args.target}" "${args.org_file}"))
                      (error "Heading '%s' not found in %s" "${args.target}" "${args.org_file}")))`;
            }
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "org_capture",
    description: "Add a new agenda item via org-capture mechanism. Uses existing capture templates or allows custom capture.",
    parameters: zod_1.z.object({
        template_key: zod_1.z.string().optional().describe("Capture template key (single character). If not provided, will show available templates"),
        content: zod_1.z.string().optional().describe("Content to capture. If not provided, will use interactive capture"),
        immediate_finish: zod_1.z.boolean().default(true).describe("Whether to immediately finish capture without opening editor"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            let elisp;
            if (!args.template_key) {
                // Show available capture templates
                elisp = `(let ((templates org-capture-templates)
                              (result "=== AVAILABLE CAPTURE TEMPLATES ===\\n"))
                          (if templates
                              (dolist (template templates)
                                (setq result (concat result 
                                  (format "%s: %s\\n" 
                                    (car template) 
                                    (cadr template)))))
                            (setq result (concat result "No capture templates configured")))
                          result)`;
                (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    else {
                        resolve(stdout.trim());
                    }
                });
                return;
            }
            if (args.content) {
                // Capture with provided content using org-capture-string
                if (args.immediate_finish) {
                    elisp = `(condition-case err
                        (let ((org-capture-entry (assoc "${args.template_key}" org-capture-templates)))
                          (if org-capture-entry
                              (progn
                                (org-capture-string "${args.content.replace(/"/g, '\\"')}" "${args.template_key}")
                                (org-capture-finalize)
                                (format "Successfully captured item using template '%s': %s" "${args.template_key}" "${args.content.replace(/"/g, '\\"')}"))
                            (error "Capture template '%s' not found" "${args.template_key}")))
                      (error 
                        (format "Capture failed: %s" (error-message-string err))))`;
                }
                else {
                    elisp = `(condition-case err
                        (let ((org-capture-entry (assoc "${args.template_key}" org-capture-templates)))
                          (if org-capture-entry
                              (progn
                                (org-capture-string "${args.content.replace(/"/g, '\\"')}" "${args.template_key}")
                                (format "Capture buffer opened with template '%s'. Edit and press C-c C-c to finish." "${args.template_key}"))
                            (error "Capture template '%s' not found" "${args.template_key}")))
                      (error 
                        (format "Capture failed: %s" (error-message-string err))))`;
                }
            }
            else {
                // Interactive capture - start capture process
                elisp = `(condition-case err
                    (let ((org-capture-entry (assoc "${args.template_key}" org-capture-templates)))
                      (if org-capture-entry
                          (progn
                            (org-capture nil "${args.template_key}")
                            (format "Interactive capture started with template '%s'. Edit and press C-c C-c to finish." "${args.template_key}"))
                        (error "Capture template '%s' not found" "${args.template_key}")))
                  (error 
                    (format "Capture failed: %s" (error-message-string err))))`;
            }
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "org_get_all_todos",
    description: "Get all TODO items from org files, including unscheduled ones. Writes results to /tmp/ClaudeWorkingFolder/all_todos.txt.",
    parameters: zod_1.z.object({
        include_done: zod_1.z.boolean().default(false).describe("Include DONE items in results"),
        org_files: zod_1.z.array(zod_1.z.string()).optional().describe("Specific org files to search (defaults to org-agenda-files)"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            // Ensure /tmp/ClaudeWorkingFolder directory exists
            (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
            const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", "all_todos.txt");
            const orgFilesClause = args.org_files
                ? `'(${args.org_files.map(f => `"${f}"`).join(" ")})`
                : "org-agenda-files";
            const elisp = `(let ((result "=== ALL TODO ITEMS ===\\n")
                              (files ${orgFilesClause})
                              (todo-keywords '("TODO" "NEXT" "STARTED" "WAITING" ${args.include_done ? '"DONE" "CANCELLED"' : ''})))
                          (dolist (file files)
                            (when (file-exists-p file)
                              (with-temp-buffer
                                (insert-file-contents file)
                                (org-mode)
                                (goto-char (point-min))
                                (setq result (concat result "\\n=== FILE: " file " ===\\n"))
                                (while (re-search-forward "^\\\\*+ \\\\(TODO\\\\|NEXT\\\\|STARTED\\\\|WAITING${args.include_done ? '\\\\|DONE\\\\|CANCELLED' : ''}\\\\) " nil t)
                                  (let* ((heading-start (line-beginning-position))
                                         (heading-end (line-end-position))
                                         (heading-text (buffer-substring heading-start heading-end))
                                         (scheduled (org-entry-get (point) "SCHEDULED"))
                                         (deadline (org-entry-get (point) "DEADLINE"))
                                         (line-num (line-number-at-pos)))
                                    (setq result (concat result
                                      (format "Line %d: %s\\n" line-num heading-text)
                                      (if scheduled (format "  SCHEDULED: %s\\n" scheduled) "")
                                      (if deadline (format "  DEADLINE: %s\\n" deadline) "")
                                      "\\n")))))))
                          (write-region result nil "${filename}")
                          (format "All TODO items written to %s" "${filename}"))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "org_schedule_todo",
    description: "Schedule a TODO item by adding SCHEDULED property. Can target by heading text in org files.",
    parameters: zod_1.z.object({
        org_file: zod_1.z.string().describe("Path to the org file containing the heading"),
        heading_text: zod_1.z.string().describe("Text of the heading to schedule"),
        schedule_date: zod_1.z.string().describe("Date/time to schedule (e.g., '2025-01-15', '2025-01-15 10:00', '+1d', 'today')"),
        remove_schedule: zod_1.z.boolean().default(false).describe("Remove existing schedule instead of setting one"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            if (!isValidFilePath(args.org_file)) {
                reject(new Error("Invalid or restricted file path"));
                return;
            }
            let elisp;
            if (args.remove_schedule) {
                elisp = `(save-window-excursion
                    (find-file "${args.org_file}")
                    (goto-char (point-min))
                    (if (search-forward "${args.heading_text}" nil t)
                        (progn
                          (org-back-to-heading t)
                          (org-schedule '(4))
                          (save-buffer)
                          (format "Successfully removed schedule from heading '%s' in %s" "${args.heading_text}" "${args.org_file}"))
                      (error "Heading '%s' not found in %s" "${args.heading_text}" "${args.org_file}")))`;
            }
            else {
                elisp = `(save-window-excursion
                    (find-file "${args.org_file}")
                    (goto-char (point-min))
                    (if (search-forward "${args.heading_text}" nil t)
                        (progn
                          (org-back-to-heading t)
                          (org-schedule nil "${args.schedule_date}")
                          (save-buffer)
                          (format "Successfully scheduled heading '%s' for %s in %s" "${args.heading_text}" "${args.schedule_date}" "${args.org_file}"))
                      (error "Heading '%s' not found in %s" "${args.heading_text}" "${args.org_file}")))`;
            }
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "get_workspace_buffers",
    description: "Get the list of buffers in each workspace. Writes results to /tmp/ClaudeWorkingFolder/workspace_buffers.txt.",
    parameters: zod_1.z.object({
        workspace_name: zod_1.z.string().optional().describe("Specific workspace name to get buffers for (if not provided, gets all workspaces)"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            // Ensure /tmp/ClaudeWorkingFolder directory exists
            (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
            const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", "workspace_buffers.txt");
            let elisp;
            if (args.workspace_name) {
                elisp = `(let ((result ""))
                          (cond
                           ;; Check for Doom workspaces first
                           ((and (fboundp '+workspace-buffer-list) (fboundp '+workspace-switch))
                            (condition-case err
                              (let* ((workspace-names (+workspace-list-names))
                                     (current-workspace (+workspace-current-name)))
                                (if (member "${args.workspace_name}" workspace-names)
                                    (progn
                                      ;; Switch to target workspace
                                      (+workspace-switch "${args.workspace_name}" t)
                                      (setq result (concat result "=== WORKSPACE: " "${args.workspace_name}" " ===\\n"))
                                      (let ((buffers (mapcar 'buffer-name (+workspace-buffer-list))))
                                        (dolist (buf buffers)
                                          (setq result (concat result buf "\\n"))))
                                      ;; Switch back to original workspace
                                      (unless (string= current-workspace "${args.workspace_name}")
                                        (+workspace-switch current-workspace t)))
                                  (setq result (format "Doom workspace '%s' not found. Available: %s\\n" 
                                                      "${args.workspace_name}" 
                                                      (mapconcat 'identity workspace-names ", ")))))
                              (error (setq result (format "Error accessing Doom workspace: %s\\n" (error-message-string err))))))
                           ;; Fallback to Eyebrowse
                           ((boundp 'eyebrowse-current-window-config)
                            (let* ((workspace-configs (eyebrowse--get 'window-configs))
                                   (target-config (cl-find-if (lambda (config) 
                                                                (string= (eyebrowse-format-slot config) "${args.workspace_name}")) 
                                                              workspace-configs)))
                              (if target-config
                                  (let ((slot (car target-config))
                                        (window-config (cdr target-config)))
                                    (setq result (concat result "=== WORKSPACE: " "${args.workspace_name}" " ===\\n"))
                                    (eyebrowse-switch-to-window-config slot)
                                    (let ((buffers (mapcar 'buffer-name (buffer-list))))
                                      (dolist (buf buffers)
                                        (setq result (concat result buf "\\n"))))
                                    (setq result (concat result "\\n")))
                                (setq result "Workspace not found"))))
                           ;; No workspace system available
                           (t (setq result "No supported workspace system found - showing current buffer list\\n")
                              (let ((buffers (mapcar 'buffer-name (buffer-list))))
                                (dolist (buf buffers)
                                  (setq result (concat result buf "\\n"))))))
                          (write-region result nil "${filename}")
                          (format "Workspace buffers written to %s" "${filename}"))`;
            }
            else {
                elisp = `(let ((result "=== ALL WORKSPACE BUFFERS ===\\n"))
                          (cond
                           ;; Check for Doom workspaces first
                           ((and (fboundp '+workspace-buffer-list) (fboundp '+workspace-list-names))
                            (condition-case err
                              (let* ((workspace-names (+workspace-list-names))
                                     (current-workspace (+workspace-current-name)))
                                (dolist (workspace-name workspace-names)
                                  (setq result (concat result "\\n=== WORKSPACE: " workspace-name " ===\\n"))
                                  ;; Switch to workspace and get its buffers
                                  (+workspace-switch workspace-name t)
                                  (let ((buffers (mapcar 'buffer-name (+workspace-buffer-list))))
                                    (dolist (buf buffers)
                                      (setq result (concat result buf "\\n")))))
                                ;; Switch back to original workspace
                                (+workspace-switch current-workspace t))
                              (error (setq result (format "Error accessing Doom workspaces: %s\\n" (error-message-string err))))))
                           ;; Fallback to Eyebrowse
                           ((boundp 'eyebrowse-current-window-config)
                            (let ((workspace-configs (eyebrowse--get 'window-configs))
                                  (current-slot (eyebrowse--get 'current-slot)))
                              (dolist (config workspace-configs)
                                (let ((slot (car config))
                                      (window-config (cdr config)))
                                  (setq result (concat result "\\n=== WORKSPACE " (number-to-string slot) " ===\\n"))
                                  (eyebrowse-switch-to-window-config slot)
                                  (let ((buffers (mapcar 'buffer-name (buffer-list))))
                                    (dolist (buf buffers)
                                      (setq result (concat result buf "\\n"))))))
                              (eyebrowse-switch-to-window-config current-slot)))
                           ;; No workspace system available
                           (t (setq result (concat result "No supported workspace system found - showing current buffer list\\n"))
                              (let ((buffers (mapcar 'buffer-name (buffer-list))))
                                (dolist (buf buffers)
                                  (setq result (concat result buf "\\n"))))))
                          (write-region result nil "${filename}")
                          (format "All workspace buffers written to %s" "${filename}"))`;
            }
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "rename_workspace",
    description: "Rename a workspace by its slot number or current name.",
    parameters: zod_1.z.object({
        workspace_identifier: zod_1.z.string().describe("Current workspace name or slot number to rename"),
        new_name: zod_1.z.string().describe("New name for the workspace"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(cond 
                              ;; Check for Doom workspaces first
                              ((and (fboundp '+workspace/rename) (fboundp '+workspace-get))
                               (condition-case err
                                 (let* ((current-workspace (+workspace-current))
                                        (workspace-names (+workspace-list-names))
                                        (target-workspace (or 
                                                          ;; Try to find by name first
                                                          (cl-find "${args.workspace_identifier}" workspace-names :test 'string=)
                                                          ;; Try to find by number
                                                          (when (string-match-p "^[0-9]+$" "${args.workspace_identifier}")
                                                            (let ((index (string-to-number "${args.workspace_identifier}")))
                                                              (when (and (>= index 0) (< index (length workspace-names)))
                                                                (nth index workspace-names)))))))
                                   (if target-workspace
                                       (progn
                                         ;; Switch to workspace temporarily to rename it, then switch back
                                         (let ((current-workspace (+workspace-current-name))
                                               (old-name nil))
                                           (condition-case rename-err
                                             (progn
                                               ;; Switch to target workspace
                                               (+workspace-switch target-workspace t)
                                               ;; Rename it (this operates on current workspace)
                                               (setq old-name (+workspace-rename (+workspace-current-name) "${args.new_name}"))
                                               ;; Switch back to original workspace
                                               (unless (string= current-workspace target-workspace)
                                                 (+workspace-switch current-workspace t))
                                               (if old-name
                                                   (format "Successfully renamed Doom workspace '%s' to '%s'" old-name "${args.new_name}")
                                                 (format "Failed to rename Doom workspace '%s'" "${args.workspace_identifier}")))
                                             (error 
                                               ;; Try to switch back on error
                                               (ignore-errors (+workspace-switch current-workspace t))
                                               (format "Error during rename: %s" (error-message-string rename-err))))))
                                     (format "Doom workspace '%s' not found. Available: %s" "${args.workspace_identifier}" (mapconcat 'identity workspace-names ", "))))
                                 (error (format "Error renaming Doom workspace: %s" (error-message-string err)))))
                              ;; Fallback to Eyebrowse
                              ((boundp 'eyebrowse-current-window-config)
                               (let* ((workspace-configs (eyebrowse--get 'window-configs))
                                      (target-config (or 
                                                      (cl-find-if (lambda (config) 
                                                                   (string= (eyebrowse-format-slot config) "${args.workspace_identifier}")) 
                                                                 workspace-configs)
                                                      (cl-find-if (lambda (config) 
                                                                   (string= (number-to-string (car config)) "${args.workspace_identifier}")) 
                                                                 workspace-configs))))
                                 (if target-config
                                     (let ((slot (car target-config)))
                                       (eyebrowse-rename-window-config slot "${args.new_name}")
                                       (format "Successfully renamed Eyebrowse workspace %s to '%s'" "${args.workspace_identifier}" "${args.new_name}"))
                                   (format "Eyebrowse workspace '%s' not found" "${args.workspace_identifier}"))))
                              (t "No supported workspace system found (neither Doom workspaces nor Eyebrowse)"))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "create_workspace",
    description: "Create a new workspace with a given name.",
    parameters: zod_1.z.object({
        workspace_name: zod_1.z.string().describe("Name for the new workspace"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(cond 
                              ;; Check for Doom workspaces first
                              ((fboundp '+workspace-new)
                               (condition-case err
                                 (progn
                                   (+workspace-new "${args.workspace_name}")
                                   (format "Successfully created Doom workspace '%s'" "${args.workspace_name}"))
                                 (error (format "Error creating Doom workspace: %s" (error-message-string err)))))
                              ;; Fallback to Eyebrowse
                              ((fboundp 'eyebrowse-create-window-config)
                               (condition-case err
                                 (progn
                                   (eyebrowse-create-window-config)
                                   (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "${args.workspace_name}")
                                   (format "Successfully created Eyebrowse workspace '%s'" "${args.workspace_name}"))
                                 (error (format "Error creating Eyebrowse workspace: %s" (error-message-string err)))))
                              (t "No supported workspace system found"))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "delete_workspace",
    description: "Delete a workspace by name or identifier.",
    parameters: zod_1.z.object({
        workspace_identifier: zod_1.z.string().describe("Workspace name or identifier to delete"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(cond 
                              ;; Check for Doom workspaces first
                              ((and (fboundp '+workspace-kill) (fboundp '+workspace-list-names))
                               (condition-case err
                                 (let* ((workspace-names (+workspace-list-names))
                                        (target-workspace (or 
                                                          (cl-find "${args.workspace_identifier}" workspace-names :test 'string=)
                                                          (when (string-match-p "^[0-9]+$" "${args.workspace_identifier}")
                                                            (let ((index (string-to-number "${args.workspace_identifier}")))
                                                              (when (and (>= index 0) (< index (length workspace-names)))
                                                                (nth index workspace-names)))))))
                                   (if target-workspace
                                       (progn
                                         (+workspace-kill target-workspace)
                                         (format "Successfully deleted Doom workspace '%s'" target-workspace))
                                     (format "Doom workspace '%s' not found. Available: %s" "${args.workspace_identifier}" (mapconcat 'identity workspace-names ", "))))
                                 (error (format "Error deleting Doom workspace: %s" (error-message-string err)))))
                              ;; Fallback to Eyebrowse
                              ((boundp 'eyebrowse-current-window-config)
                               (condition-case err
                                 (let* ((workspace-configs (eyebrowse--get 'window-configs))
                                        (target-config (or 
                                                        (cl-find-if (lambda (config) 
                                                                     (string= (eyebrowse-format-slot config) "${args.workspace_identifier}")) 
                                                                   workspace-configs)
                                                        (cl-find-if (lambda (config) 
                                                                     (string= (number-to-string (car config)) "${args.workspace_identifier}")) 
                                                                   workspace-configs))))
                                   (if target-config
                                       (let ((slot (car target-config)))
                                         (eyebrowse-close-window-config slot)
                                         (format "Successfully deleted Eyebrowse workspace %s" "${args.workspace_identifier}"))
                                     (format "Eyebrowse workspace '%s' not found" "${args.workspace_identifier}")))
                                 (error (format "Error deleting Eyebrowse workspace: %s" (error-message-string err)))))
                              (t "No supported workspace system found"))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "view_buffer",
    description: "Get the contents of one or more Emacs buffers and write each to /tmp/ClaudeWorkingFolder/<buffer_name>.txt. Returns a list of file paths for all buffers.",
    parameters: zod_1.z.object({
        buffer_names: zod_1.z.array(zod_1.z.string().min(1, "Buffer name cannot be empty")).min(1, "Must provide at least one buffer name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            try {
                // Ensure /tmp/ClaudeWorkingFolder directory exists
                (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
                // Create Elisp to process all buffers
                const bufferProcessing = args.buffer_names.map((bufferName, index) => {
                    const sanitizedBufferName = bufferName.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `${sanitizedBufferName}.txt`);
                    return `(condition-case err
                        (with-current-buffer "${bufferName}"
                          (let ((content (buffer-string)) 
                                (lines (split-string (buffer-string) "\\n")))
                            (write-region (mapconcat (lambda (line) 
                                                      (format "%4d→%s" (1+ (cl-position line lines :test 'equal)) line)) 
                                                    lines "\\n") 
                                         nil "${filename}")
                            "${filename}"))
                        (error 
                          (format "Error processing buffer '${bufferName}': %s" (error-message-string err))))`;
                }).join("\n        ");
                const elisp = `(let ((results '()))
                    ${bufferProcessing}
                    (let ((successful-files '()))
                      ${args.buffer_names.map((bufferName) => {
                    const sanitizedBufferName = bufferName.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `${sanitizedBufferName}.txt`);
                    return `(when (file-exists-p "${filename}") 
                                    (push "${filename}" successful-files))`;
                }).join("\n                      ")}
                      (format "Buffer contents written to files: %s" (mapconcat 'identity (reverse successful-files) ", "))))`;
                (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    else {
                        resolve(stdout.trim());
                    }
                });
            }
            catch (writeError) {
                reject(new Error(`Failed to prepare multiple buffer processing: ${writeError}`));
            }
        });
    }),
});
mcp.addTool({
    name: "move_buffer_to_workspace",
    description: "Move a buffer to a specific workspace. Works with both Doom workspaces and Eyebrowse.",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().describe("Name of the buffer to move"),
        workspace_name: zod_1.z.string().describe("Name of the target workspace"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `
                (condition-case err
                    (cond
                     ;; Check for Doom workspaces first
                     ((and (fboundp 'persp-add-buffer) (fboundp '+workspace-get) (fboundp '+workspace-switch))
                      (let* ((buffer (get-buffer "${args.buffer_name}"))
                             (workspace-names (+workspace-list-names))
                             (target-workspace (cl-find "${args.workspace_name}" workspace-names :test 'string=))
                             (current-workspace (+workspace-current-name)))
                        (if (and buffer target-workspace)
                            (progn
                              ;; Switch to target workspace temporarily
                              (+workspace-switch target-workspace t)
                              ;; Add buffer to workspace
                              (persp-add-buffer buffer (+workspace-get target-workspace))
                              ;; Switch back to original workspace
                              (unless (string= current-workspace target-workspace)
                                (+workspace-switch current-workspace t))
                              (format "Successfully moved buffer '%s' to workspace '%s'" "${args.buffer_name}" "${args.workspace_name}"))
                          (cond
                           ((not buffer) (format "Buffer '%s' not found" "${args.buffer_name}"))
                           ((not target-workspace) (format "Workspace '%s' not found. Available: %s" "${args.workspace_name}" (mapconcat 'identity workspace-names ", ")))
                           (t "Unknown error")))))
                     ;; Check for Eyebrowse workspaces
                     ((fboundp 'eyebrowse-switch-to-window-config)
                      (let* ((buffer (get-buffer "${args.buffer_name}"))
                             (workspace-configs (eyebrowse--get 'window-configs))
                             (target-config (cl-find-if (lambda (config)
                                                          (string= (eyebrowse-format-slot config) "${args.workspace_name}")) 
                                                        workspace-configs))
                             (current-slot (eyebrowse--get 'current-slot)))
                        (if (and buffer target-config)
                            (progn
                              ;; Switch to target workspace
                              (eyebrowse-switch-to-window-config (car target-config))
                              ;; Display buffer in workspace
                              (switch-to-buffer buffer)
                              ;; Switch back to original workspace
                              (unless (= current-slot (car target-config))
                                (eyebrowse-switch-to-window-config current-slot))
                              (format "Successfully moved buffer '%s' to workspace '%s'" "${args.buffer_name}" "${args.workspace_name}"))
                          (cond
                           ((not buffer) (format "Buffer '%s' not found" "${args.buffer_name}"))
                           ((not target-config) (format "Workspace '%s' not found" "${args.workspace_name}"))
                           (t "Unknown error")))))
                     (t "No supported workspace system found"))
                  (error (format "Error moving buffer: %s" (error-message-string err))))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim().replace(/^"(.*)"$/, '$1'));
                }
            });
        });
    }),
});
mcp.start({
    transportType: "stdio",
});
