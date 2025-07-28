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
// TODO[A5OhAyxaiJ] Overly restrictive? Not sufficient sanitization?
const isValidEmacsSymbol = (str) => /^[a-zA-Z0-9-_]+$/.test(str);
mcp.addTool({
    name: "describe_function",
    description: "Get documentation for an Emacs function",
    parameters: zod_1.z.object({
        function_name: zod_1.z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    // Using execfile because it is safe from shell injection.
    // https://nodejs.org/api/child_process.html#child_processexecfilefile-args-options-callback
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", ["-e", `(documentation '${args.function_name})`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "describe_variable",
    description: "Get documentation for an Emacs variable",
    parameters: zod_1.z.object({
        variable_name: zod_1.z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", [
                "-e",
                `(documentation-property '${args.variable_name} 'variable-documentation)`,
            ], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "get_variable_value",
    description: "Get the current value of an Emacs variable",
    parameters: zod_1.z.object({
        variable_name: zod_1.z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", ["-e", `(symbol-value '${args.variable_name})`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "view_buffer",
    description: "Get the contents of a specific Emacs buffer and write it to /tmp/ClaudeWorkingFolder/<buffer_name>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the buffer content.",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().min(1, "Buffer name cannot be empty"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", ["-e", `(with-current-buffer "${args.buffer_name}" (let ((content (buffer-string)) (lines (split-string (buffer-string) "\\n"))) (mapconcat (lambda (line) (format "%4d→%s" (1+ (cl-position line lines :test 'equal)) line)) lines "\\n")))`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                const content = stdout.trim();
                try {
                    // Ensure /tmp/ClaudeWorkingFolder directory exists
                    (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
                    // Create filename from buffer name (sanitize special characters)
                    const sanitizedBufferName = args.buffer_name.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `${sanitizedBufferName}.txt`);
                    (0, fs_1.writeFileSync)(filename, content, "utf8");
                    resolve(`Buffer content written to ${filename}`);
                }
                catch (writeError) {
                    reject(new Error(`Failed to write file: ${writeError}`));
                }
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
const isValidFilePath = (str) => str.length > 0 && str.length < 1024;
mcp.addTool({
    name: "open_file",
    description: "Open a file in Emacs and return the buffer name",
    parameters: zod_1.z.object({
        file_path: zod_1.z.string().refine(isValidFilePath, "Invalid file path"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(progn (find-file "${args.file_path}") (buffer-name))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    const bufferName = stdout.trim().replace(/^"(.*)"$/, '$1');
                    resolve(`File opened in buffer: ${bufferName}`);
                }
            });
        });
    }),
});
mcp.addTool({
    name: "eglot_help_at_point",
    description: "Get LSP hover information for a symbol at a specific line in a buffer",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().refine(isValidBufferName, "Invalid buffer name"),
        line_number: zod_1.z.number().int().positive(),
        symbol_name: zod_1.z.string().refine(isValidSymbolName, "Invalid symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(with-current-buffer "${args.buffer_name}" (save-excursion (goto-char (point-min)) (forward-line ${args.line_number - 1}) (when (search-forward "${args.symbol_name}" (line-end-position) t) (goto-char (match-beginning 0)) (if (fboundp 'eglot-help-at-point) (eglot-help-at-point) "eglot-help-at-point not available"))))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim());
                }
            });
        });
    }),
});
mcp.addTool({
    name: "eglot_find_definition",
    description: "Find definition of a symbol at a specific line in a buffer",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().refine(isValidBufferName, "Invalid buffer name"),
        line_number: zod_1.z.number().int().positive(),
        symbol_name: zod_1.z.string().refine(isValidSymbolName, "Invalid symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(with-current-buffer "${args.buffer_name}" (save-excursion (goto-char (point-min)) (forward-line ${args.line_number - 1}) (when (search-forward "${args.symbol_name}" (line-end-position) t) (goto-char (match-beginning 0)) (if (fboundp 'eglot-find-definition) (progn (eglot-find-definition) (format "Jumped to %s:%d" (buffer-name) (line-number-at-pos))) "eglot-find-definition not available"))))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim());
                }
            });
        });
    }),
});
mcp.addTool({
    name: "eglot_find_references",
    description: "Find references to a symbol at a specific line in a buffer",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().refine(isValidBufferName, "Invalid buffer name"),
        line_number: zod_1.z.number().int().positive(),
        symbol_name: zod_1.z.string().refine(isValidSymbolName, "Invalid symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(with-current-buffer "${args.buffer_name}" (save-excursion (goto-char (point-min)) (forward-line ${args.line_number - 1}) (when (search-forward "${args.symbol_name}" (line-end-position) t) (goto-char (match-beginning 0)) (if (fboundp 'eglot-find-references) (progn (eglot-find-references) "References found - check *xref* buffer") "eglot-find-references not available"))))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim());
                }
            });
        });
    }),
});
mcp.start({
    transportType: "stdio",
});
